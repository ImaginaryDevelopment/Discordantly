namespace Discordantly

open System.Threading.Tasks

open Schema.Helpers

open Discord.WebSocket
open Discord

// exposes only things the bot commands should have access to
type ClientProxy(client:DiscordSocketClient) =
    member __.CurrentUser = client.CurrentUser
    member __.LoginState = client.LoginState
    member __.DMChannels = client.DMChannels
    member __.ShardId = client.ShardId
    member __.Latency = client.Latency
    member __.SetStatusAsync user = client.SetStatusAsync user
    member __.GetUser id = client.GetUser(id=id)
    member __.GetUser (username, discriminator) = client.GetUser(username, discriminator)
type MessageType =
    | Keepsies of string
    | Deletesies of string
module SocketMessage =
    open Discord.Rest

    type AsyncReplyWrapper = {RestUserMessage:Rest.RestUserMessage; DeleteMe:bool}
    let reply' (sm:SocketMessage) (txt:string):Task<_>=
        upcast sm.Channel.SendMessageAsync txt
    let reply(sm:SocketMessage) (txt:MessageType list) :Task<_> =
        let single x :Async<AsyncReplyWrapper list> =
            async {
                let (deleteMe,x) =
                    match x with
                    | Keepsies x ->
                        false, x
                    | Deletesies x ->
                        true, x
                let! msg = sm.Channel.SendMessageAsync x
                return [{RestUserMessage = msg; DeleteMe = deleteMe}]
            }
        match txt with
        | [] -> single (Keepsies "Tell my creator that, due to creative differences, I will not reply to that message")
        | x::[] -> single x
        | replies ->
            async{
                let mutable items = List.empty
                for r in replies do
                    let deleteMe,txt =
                        match r with
                        | Keepsies txt ->
                            false, txt
                        | Deletesies txt ->
                            true, txt
                    let! result = sm.Channel.SendMessageAsync txt
                    items <- {RestUserMessage=result; DeleteMe=deleteMe} :: items
                    do! Async.Sleep 600
                return items
            }
        |> fun pipe ->
            async{
                let! items = pipe
                match items |> List.choose(fun m -> if m.DeleteMe then Some m.RestUserMessage else None) with
                | [] ->
                    return items
                | x ->
                    do! Async.Sleep 5000
                    x |> List.map(fun x -> x.DeleteAsync())
                    |> List.iter (Async.AwaitTask>>ignore)
                    return items
            }
            |> Async.StartAsTask


    //abstract member () ()
// [<NoSubstitute("for you")>]
[<NoEquality;NoComparison>]
type ReplyType =
    | Simple of string
    //| Complex of (ClientProxy -> SocketMessage -> Task<Rest.RestUserMessage list> option)
    | Complex of (ClientProxy -> SocketMessage -> Task option)
type TriggerType =
    | Command of string
    | Method of beginsWith:string

type Command = {Trigger:string;ReplyType:ReplyType}
// profile tracking, etc
module Exiling =
    open Regex
    type private ExileMap = Map<uint64,string>
    module Impl =
        type LikeAProperty<'t>(initialValue:'t,fSideEffect) =
            let mutable value = initialValue
            member __.Value
                with get() = value
                and set v =
                    value <- v
                    fSideEffect value

        module Profiling =

            let profiles = 
                let get,set =
                    Storage.createGetSet<ExileMap> "Exiling"
                let v =
                    get()
                    |> Option.defaultValue Map.empty
                LikeAProperty(v, Some>>set)

            let findExileUser (cp:ClientProxy) username =
                profiles.Value
                |> Map.tryPick(fun uid profileName ->
                    let u = cp.GetUser uid
                    if u.Username = username then
                        Some (u,profileName)
                    else None
                )
        let onProfileSearch =
            function
                | un, Some(_user,pn) ->
                        [   Keepsies <| sprintf "♫ %s has a path name it's O.S.C.A.R. ♫ Wait. No. My bad. It's %s" un pn
                            Deletesies <| sprintf "Served, get it while it's hot: https://www.pathofexile.com/account/view-profile/%s/characters" pn
                        ]
                | un, None ->
                        [Keepsies <| sprintf "♪ My baloney has a first name ♪, however %s, does not." un]

        // trimming the end in case there were stray spaces
        let (|ProfileName|_|) =
            function
            | RMatchGroup "\w.+$" 0 (Trim profileName) ->
                Some profileName

            | _ -> None
        // assumes a name can't end with whitespace
        let (|UserName|_|) =
            function
            | RMatchGroup "\w.+$" 0 (Trim userName) ->
                Some userName
            | _ -> None
        let getAuthorProfile (su:SocketUser) cp =
            (su.Username, Profiling.findExileUser cp su.Username)
            |> onProfileSearch

    open Impl
    open System.Net.Sockets


    let setProfile =
        "setProfile", Complex (fun cp sm ->
            match sm.Content with
            | After "setProfile " (RMatchGroup "\w.+$" 0 (Trim profileName)) ->
                Impl.Profiling.profiles.Value <-
                    Impl.Profiling.profiles.Value
                    |> Map.add sm.Author.Id profileName
                async {
                    do!
                        [Keepsies <| sprintf "I set %s's poeprofile name to %s. I hope you capitalized it properly." sm.Author.Username profileName]
                        |> SocketMessage.reply sm
                        |> Async.AwaitTask
                        |> Async.Ignore
                    do!
                        [Keepsies <| sprintf "I am growing, and I think I'll remember this, but I may forget."]
                        |> SocketMessage.reply sm
                        |> Async.AwaitTask
                        |> Async.Ignore
                }
                |> Async.Ignore
                |> Async.StartAsTask
                :> Task
                |> Some
            | RMatch "setProfile$" _ ->
                (sm.Author.Username, Impl.Profiling.findExileUser cp sm.Author.Username)
                |> onProfileSearch
                |> SocketMessage.reply sm
                :> Task
                |> Some
            | _ -> None
        )

    let getProfile =
        "getProfile",
            Complex (fun cp sm ->
                match sm.Content with
                | After "getProfile " (UserName userName) ->
                    (userName,Impl.Profiling.findExileUser cp userName)
                    |> onProfileSearch
                    |> SocketMessage.reply sm
                    :> Task
                    |> Some
                | RMatch "getProfile\s*$" _ ->
                    getAuthorProfile sm.Author cp
                    |> SocketMessage.reply sm
                    :> Task
                    |> Some
                | _ -> None
            )

module Commandments =
    open System

    let simpleReplies =
        [
         "ping","pong!"
         "play","would you like to play a game?"
         "poelink","https://jsfiddle.net/Maslow/e02ts1jy/show"
         "language","I'm always down to F#, do you speak my language? I hope you aren't a dirty C#er"
         "src", "https://github.com/ImaginaryDevelopment/Discordantly"
        ]
        |> List.map(fun (x,y) -> sprintf "!%s" x,y)
        |> Map.ofList

    let notSimpleReplies =
        [
            Exiling.getProfile
            Exiling.setProfile
        ]
        |> List.map(fun (x,y) -> sprintf "!%s" x,y)
        |> Map.ofList
    ()

    let (|Simpleton|NotSimpleton|UhOh|IgnoranceIsBliss|) (authorId,botUserId, content) =
        if authorId = botUserId || content |> String.IsNullOrWhiteSpace then
            IgnoranceIsBliss
        elif simpleReplies.ContainsKey content then
            Simpleton simpleReplies.[content]
        else
            notSimpleReplies
            |> Map.tryFindKey(fun k _ -> content.StartsWith k)
            |> function
                | Some x -> NotSimpleton (x,notSimpleReplies.[x])
                | None -> 
                    if content.StartsWith "!" then
                        UhOh
                    else IgnoranceIsBliss

