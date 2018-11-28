// following https://github.com/RogueException/Discord.Net/blob/dev/samples/01_basic_ping_bot/Program.cs

open System
open System.Threading.Tasks
open Discord.WebSocket
open Discord

let logAsync msg : Task =
    printfn "%A" msg
    Task.CompletedTask
let readyAsync():Task =
    printfn "Ready!"
    Task.CompletedTask
let msgAsync (fClient:unit -> DiscordSocketClient) (sm:SocketMessage) : Task =
    printfn "Msg! %A" sm.Source
    async{
        let client = fClient()
        if sm.Author.Id <> client.CurrentUser.Id && sm.Content = "!ping" then
            let! x = Async.AwaitIAsyncResult <| sm.Channel.SendMessageAsync("pong!")
            ()
    }
    |> Async.StartAsTask
    :> Task

type TaskResult<'t> =
    | Happy of 't
    | Unhappy of string*exn option
let thenPrint title (x:Async<_>) =
    async {
        let! x = x
        printfn "awaiting complete:%s" title
        return x
    }
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
let main argv =
    printfn "Hello World from F#!"
    mainAsync().GetAwaiter().GetResult()
    
    0 // return an integer exit code
