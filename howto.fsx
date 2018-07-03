#r "packages/FSharp.Data/lib/net45/FSharp.Data.dll"

//Register your token before loading module : for example bind the env var to a ignored file content
let (</>) x y = System.IO.Path.Combine(x, y)
System.Environment.SetEnvironmentVariable("SlackToken", System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ </> "token"))

//Then load the Slack module
#load "Slack.fs"

//Disable logging
None |> Slack.Log.setLogLevel

//Enable logging
Some Slack.Log.INFO |> Slack.Log.setLogLevel

open System.Threading

open Slack

let slackBot token userId = 
    let actor = Actor.spawn (fun x _ -> printfn "slackBot received %A. Do whatever in another thread (actor) without disturbing the bot listener!" x |> Async.ret) ()
    faultTolerantServer token userId (fun c t m -> actor (c,t,m) |> async.Return)

let cancel = new CancellationTokenSource()

//Find your bot and enjoy !
findBotId (Bot "belzebot") |> Option.iter (slackBot cancel.Token >> Async.Start)

//Stop the bot
cancel.Cancel()