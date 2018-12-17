module CommandSchema

open Schema.BReusable
open Schema.Helpers
open Schema.Helpers.StringPatterns

open Discord.WebSocket
open System.Threading.Tasks


// let's abstract away to just the stuff we need
type ClientProxy(client:DiscordSocketClient) =
    member __.CurrentUser = client.CurrentUser
    member __.LoginState = client.LoginState
    member __.DMChannels = client.DMChannels
    member __.ShardId = client.ShardId
    member __.Latency = client.Latency
    member __.SetStatusAsync user = client.SetStatusAsync user
    member __.GetUser id = client.GetUser(id=id)
    member __.GetUser (username, discriminator) = client.GetUser(username, discriminator)
    //member __.X = client.
    interface System.IDisposable with
        member x.Dispose () = x.Dispose()
    member __.Dispose () = client.Dispose()

[<NoEquality;NoComparison>]
type ReplyType =
    | Simple of string
    | Multiple of string list
    //| Complex of (ClientProxy -> SocketMessage -> Task<Rest.RestUserMessage list> option)
    | Complex of (ClientProxy -> SocketMessage -> Async<unit> option)
type TriggerType =
    | Command of string
    | Method of beginsWith:string

type Command = {Trigger:string;ReplyType:ReplyType}
type NotSimple = {TriggerHelp:string list;F:ReplyType}

let (|Link|_|) =
    function 
    | After "<" (Before ">" uri)
    | After "`"(Before "`" uri)
    // this allows links that haven't had previews shut off by surrounding tokens
    | RMatchGroup @"(http[^ ]+)(\s|$)" 1 uri ->
        Some uri
    | _ -> None

let contentPath = System.Environment.CurrentDirectory
let sendMessageAsync (sm:SocketMessage) text =
    async {
        let! msg = sm.Channel.SendMessageAsync(text)
        return msg
    }

let sendMessagesAsync sm texts =
    texts
    |> Seq.map (sendMessageAsync sm)
    //|> Seq.map Async.AwaitTask
    |> Async.flatten
