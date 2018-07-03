//Register your token before loading module
System.Environment.SetEnvironmentVariable("SlackToken", "xoxb-62388246470-dBVfIdRMtUNBEUhFoZ9ltdEp")

//Then load the Slack module
#load "Slack.fsx"

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