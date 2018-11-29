﻿// following https://github.com/RogueException/Discord.Net/blob/dev/samples/01_basic_ping_bot/Program.cs

open System
open System.Threading.Tasks
open Discord.WebSocket
open Discord

open Discordantly
open Discordantly.Commandments

let logAsync msg : Task =
    printfn "%A" msg
    Task.CompletedTask
let readyAsync():Task =
    printfn "Ready!"
    Task.CompletedTask
let msgAsync (fClient:unit -> DiscordSocketClient) (sm:SocketMessage) : Task =
    printfn "Msg! %A" sm.Source
    let notFound = "Dude, where's my response dude?"


    async{
        let send = sm.Channel.SendMessageAsync >> ignore
        let client = fClient()
        match (sm.Author.Id, client.CurrentUser.Id,sm.Content) with
        | IgnoranceIsBliss ->
            ()
        | Simpleton txt ->
            send txt
        | NotSimpleton (cmdName, Complex f) ->
            match f (ClientProxy client) sm with
            | Some r -> r |> ignore<Task>
            | None -> send <| sprintf "Could not understand command arguments for %s" cmdName
        | NotSimpleton (cmdName, _) ->
            send <| sprintf "Tell my creator he needs to learn how to code better"
        | UhOh ->
            send notFound
    }
    |> Async.StartAsTask
    :> Task

//type TaskResult<'t> =
//    | Happy of 't
//    | Unhappy of string*exn option
//let thenPrint title (x:Async<_>) =
//    async {
//        let! x = x
//        printfn "awaiting complete:%s" title
//        return x
//    }

let mainAsync () : Task<_> =
    let client = new DiscordSocketClient()
    client.add_Log(Func<_,_>(logAsync))
    client.add_Ready(Func<_> readyAsync)
    client.add_MessageReceived(Func<_,_> (msgAsync (fun () -> client)))
    async{
        printfn "Starting login"
        let token = Environment.GetEnvironmentVariable("discordToken")
        printfn "Token is %A" token
        do! Async.AwaitTask(client.LoginAsync(TokenType.Bot, token))
        printfn "Login finished?"
        do! Async.AwaitTask(client.StartAsync())
        printfn "Finished Starting Client"
        do! Async.AwaitTask(Task.Delay(-1))
    }
    |> Async.StartAsTask

[<EntryPoint>]
let main _argv =
    printfn "Hello World from F#!"
    mainAsync().GetAwaiter().GetResult()
    
    0 // return an integer exit code
