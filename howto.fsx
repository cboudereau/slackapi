#r "packages/FSharp.Data/lib/net45/FSharp.Data.dll"
#load "paket-files/cboudereau/fsharplib/core.fs"
#load "paket-files/cboudereau/fsharplib/log.fs"
#load "paket-files/cboudereau/fsharplib/parsing.fs"
#load "paket-files/cboudereau/fsharplib/string.fs"

//Register your token before loading module : for example bind the env var to a ignored file content
let (</>) x y = System.IO.Path.Combine(x, y)
System.Environment.SetEnvironmentVariable("SlackToken", System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ </> "token"))

//Then load the Slack module
#load "slack.fs"

open Core

Some Log.DEBUG |> Log.setLogLevel 

Slack.post (Slack.Channel "general") (Slack.TextMessage "hello") |> Async.RunSynchronously

//Disable logging
None |> Log.setLogLevel

//Enable logging
Some Log.INFO |> Log.setLogLevel

open System.Threading

open Slack

let slackBot config token userId = 
    let actor = Actor.spawn (fun state reply message -> 
        let notif = 
            message
            |> Option.map(fun n -> 
                let t = sprintf "slackBot received %A. Do whatever in another thread (actor) without disturbing the bot listener!" n.Message |> TextMessage
                printfn "%A" t
                t |> Notification.build n.Channel n.From
                ) 
            |> Option.map NonEmptyList.singleton
        Listen state |> HandlerResponse.build notif 
        |> Async.ret) (State ())
    listen config token userId (fun c t m -> actor (Notification.build c t m) |> async.Return )

let cancel = new CancellationTokenSource()

//Find your bot and enjoy !
let config = Slack.rtmStart ()
findBotId (Bot "belzebot") config |> Option.iter (slackBot config cancel.Token >> Async.Start)

//Stop the bot
cancel.Cancel()