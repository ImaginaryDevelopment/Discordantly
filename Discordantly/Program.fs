// following https://github.com/RogueException/Discord.Net/blob/dev/samples/01_basic_ping_bot/Program.cs

open System
open System.Threading.Tasks

open Schema.Helpers
open Discord.WebSocket
open Discord

open Discordantly
open Discordantly.Commandments
open CommandSchema

let logAsync msg : Task =
    printfn "%A" msg
    Task.CompletedTask

let readyAsync():Task =
    printfn "Ready!"
    Task.CompletedTask

let printMessgMeta (sm:SocketMessage) =
    // hey look at Discord.net's horrible you need to cast something to check if it has properties. No SOLID or discoverability here, un uh.
    match sm.Channel with
    | :? SocketGuildChannel as sgc ->
        printfn "Msg! %s:%s:%A:%s" sgc.Guild.Name sm.Channel.Name sm.Source sm.Author.Username
    | _ ->
        printfn "Msg! %A:%s:%s" sm.Source sm.Channel.Name sm.Author.Username

// check for a command, react if found, suggest if arguments are not right
let msgAsync (fClient:unit -> DiscordSocketClient) (sm:SocketMessage) : Task =
    let notFound = "Dude, where's my response dude?"
    // if this isn't an appropriate channel for a bot, ignore it
    if sm.Channel.Name.Contains("bot") = false then
        if sm.Content.StartsWith "!" then
            printfn "ignoring a command from %s(%s)" sm.Channel.Name sm.Author.Username
        Task.CompletedTask
    else
        async{
            let send msgs =
                async{
                    for x in msgs do
                        let! _ = sm.Channel.SendMessageAsync(x)
                        ()

                }
                |> Async.Start
                |> ignore
                //sm.Channel.SendMessageAsync >> ignore
            let client = fClient()
            match (sm.Author.Id, client.CurrentUser.Id,sm.Content) with
            | IgnoranceIsBliss ->
                printMessgMeta sm
                ()
            | Simpleton lines ->
                send lines
            | NotSimpleton (cmdName, help, ReplyType.Complex f) ->
                match f (ClientProxy client) sm with
                | Some r -> r |> ignore<Task>
                | None ->
                    sprintf "Could not understand command arguments for `%s`" cmdName::help
                    |> send

            | NotSimpleton (_cmdName, _help, Multiple lines) ->
                sendMessagesAsync sm lines
                |> Async.Ignore
                |> Async.Start
                |> ignore
            | NotSimpleton (_cmdName, _, _) ->
                send ["Tell my creator he needs to learn how to code better"]
            | UhOh ->
                send [notFound]
        }
        |> Async.StartAsTask
        :> Task

let mainAsync () : Task<_> =
    let client = new DiscordSocketClient()
    client.add_Log(Func<_,_>(logAsync))
    client.add_Ready(Func<_> readyAsync)
    client.add_MessageReceived(Func<_,_>(msgAsync (fun () -> client)))
    async{
        printfn "Starting login"
        let token = Environment.GetEnvironmentVariable("discordToken")
        //printfn "Token is %A" token
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
