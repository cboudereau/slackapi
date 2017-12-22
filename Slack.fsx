#r "packages/FSharp.Data/lib/net45/FSharp.Data.dll"
open System
open System.IO
open System.Threading

module Async = 
    let map f x = async { let! x' = x in return f x' }
    let bind f x =  async.Bind(x, f)
    let ret x = async.Return(x)
    let ofOption zero = function Some x -> x | None -> ret zero

module Parsing = 
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern source = 
        let m = Regex.Match(source, pattern, RegexOptions.CultureInvariant ||| RegexOptions.IgnoreCase)
        if m.Success then [ for x in m.Groups -> x.Value ] |> List.tail |> Some
        else None

module String = 
    let icontains search source = (source:string).IndexOf((search:string), StringComparison.InvariantCultureIgnoreCase) <> -1
    let (|Contains|_|) search source =
        if icontains search source then Some source
        else None 

type [<Struct>] TeamMember = TeamMember of string
type [<Struct>] Email = Email of string
type [<Struct>] TextMessage = TextMessage of string
type [<Struct>] Channel = Channel of string
type [<Struct>] Bot = Bot of string

type SlackBotException (m,i) = inherit exn ((m:string),(i:exn))

open FSharp.Data
open System.Net.WebSockets

type [<Struct>] SlackUserId = SlackUserId of string
type RtmStart = JsonProvider< "rtmStart.sample.json" >

type RtmMessage = JsonProvider< "rtmMessage.sample.json", SampleIsList=true >
let [<Literal>] Token = "xoxb-62388246470-2S9tkBTE1YU9NgDIGK5XMjwU"

let rtmStart = Http.RequestString("https://slack.com/api/rtm.start", query=["token", Token], httpMethod="GET") |> RtmStart.Parse
let tryFindChannel (Channel channel) = rtmStart.Channels |> Array.tryFind(fun c -> c.Name = channel)

let tryFindUserName (SlackUserId userId) = rtmStart.Users |> Array.tryFind(fun u -> u.Id = userId) |> Option.map(fun u -> TeamMember u.Name)

let findBotId (Bot bot) = 
    let user = rtmStart.Users |> Array.tryFind(fun u -> u.Name = bot)
    user |> Option.map (fun x -> x.Id |> SlackUserId)

let tryFindUserId (Email email) = 
    rtmStart.Users
    |> Array.filter(fun u -> u.Profile.Email = Some email)
    |> Array.tryHead
    |> Option.map(fun u -> SlackUserId u.Id)

let rec receive (ms: MemoryStream) token (webSocket: ClientWebSocket) = 
    async {
        let buffer = new ArraySegment<byte>(Array.zeroCreate(8192))
        let! res = webSocket.ReceiveAsync(buffer, token) |> Async.AwaitTask
        ms.Write(buffer.Array, buffer.Offset, res.Count)
        if res.EndOfMessage 
        then
            ms.Seek(0L, SeekOrigin.Begin) |> ignore
            return ms.ToArray() 
        else
            return! webSocket |> receive ms token
    }

type Message = Message of Channel * TeamMember * TextMessage

let readMessage token (webSocket: ClientWebSocket) (SlackUserId botId) =
    async {
        printfn "readMessage for botId:%s" botId
        use ms = new MemoryStream ()
        return! 
            receive ms token webSocket
            |> Async.Catch
            |> Async.map (function
                | Choice1Of2 data -> 
                    let message = System.Text.ASCIIEncoding.ASCII.GetString(data) |> RtmMessage.Parse
                    match message.Type, message.ReplyTo, message.Text, message.User, message.Channel with
                    | String.Contains "pong" _, Some id, _, _, _ -> 
                        printfn "pong"
                        None
                    | String.Contains "message" _, _, Some (String.Contains botId text), Some user, Some channel when user <> botId ->
                        let userName = 
                            match user |> SlackUserId |> tryFindUserName with
                            | Some name -> name
                            | None -> TeamMember String.Empty
                        Some (Message (Channel channel, userName, TextMessage text))
                    | _ -> 
                        printfn "message skipped : %A" message
                        None
                | Choice2Of2 ex -> SlackBotException(sprintf "readMessage unhandled exception %s" ex.Message, ex) |> raise) }
let sendData token (webSocket: ClientWebSocket) (data: string) = 
    async {
        let buffer = new ArraySegment<byte>(System.Text.ASCIIEncoding.ASCII.GetBytes(data))
        do! webSocket.SendAsync(buffer, WebSocketMessageType.Text, true, token) |> Async.AwaitTask
    }
let ping token n webSocket = 
    async {
        printfn "ping"
        do! sprintf """{"id": %d, "type":"ping"}""" n |> sendData token webSocket
        printfn "ping sent"
    }
let socket token userId handler = 
    let handle = function Message (c, tm, m) -> handler c tm m

    let rec read token (webSocket:ClientWebSocket) = 
        async {
            match webSocket.State with
            | WebSocketState.Open -> 
                do! readMessage token webSocket userId |> Async.bind (Option.map handle >> Async.ofOption ())
                do! read token webSocket
            | _ -> return ()
        }
    
    async { 
        use webSocket = new ClientWebSocket()
        printfn "socket connecting"
        let rtmStart = Http.RequestString("https://slack.com/api/rtm.start", query=["token", Token], httpMethod="GET") |> RtmStart.Parse

        let rec pinG token n (webSocket:ClientWebSocket) =
            async {
                match webSocket.State with
                | WebSocketState.Open ->
                    let next = if n = Int32.MaxValue then 1 else n + 1
                    do! ping token next webSocket
                    do! Async.Sleep 1000
                    do! pinG token next webSocket 
                | _ -> return ()
            }

        printfn "rtmStart : %s" rtmStart.Url
        do! webSocket.ConnectAsync(Uri(rtmStart.Url), token) |> Async.AwaitTask
        do!
            [ read token webSocket
              pinG token 1 webSocket ]
            |> Async.Parallel
            |> Async.Ignore

        return webSocket.State
    }
let rec server (token:CancellationToken) userId handler = 
    let next = 
        let rnd = Random ()
        fun () -> rnd.Next(0, 10)
    
    let reconnect () =
        if not token.IsCancellationRequested then 
            let time = next ()
            printfn "retrying in %is" time
            time * 1000 |> Async.Sleep |> Async.bind (fun _ -> server token userId handler)
        else 
            printfn "socketServer finished"
            Async.ret ()

    async {
        do! 
            socket token userId handler
            |> Async.Catch
            |> Async.bind (function 
                | Choice1Of2 WebSocketState.Open -> printfn "server finished" |> Async.ret
                | Choice1Of2 state -> 
                    printfn "server finished not gracefully with state %s" (state.ToString())
                    reconnect ()
                | Choice2Of2 ex ->
                    printfn "reconnecting due to %O" ex
                    reconnect ())
    }
let post handler = 
    let actor = 
        MailboxProcessor.Start <| fun channel -> 
            let rec listen () = 
                async {
                    let! msg = channel.Receive ()
                    handler msg
                    do! listen ()
                }
            listen ()
    actor.Post
let slackBot token userId = server token userId (fun c t m -> post (printfn "slackBot received %A") (c,t,m) |> Async.ret)
let cancel = new CancellationTokenSource()

findBotId (Bot "belzebot") |> Option.iter (slackBot cancel.Token >> Async.RunSynchronously)