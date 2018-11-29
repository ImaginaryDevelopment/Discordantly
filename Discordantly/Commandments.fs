namespace Discordantly

open Discord.WebSocket
open System.Threading.Tasks
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
module SocketMessage =
    let reply (sm:SocketMessage) (txt:string) :Task=
        upcast sm.Channel.SendMessageAsync txt
    //abstract member () ()
// [<NoSubstitute("for you")>]
[<NoEquality;NoComparison>]
type ReplyType =
    | Simple of string
    | Complex of (ClientProxy -> SocketMessage -> Task option)
type TriggerType =
    | Command of string
    | Method of beginsWith:string

type Command = {Trigger:string;ReplyType:ReplyType}
// profile tracking, etc
module Exiling =
    open Regex
    module Impl =
        let mutable profiles = Map.empty
        let findExileUser (cp:ClientProxy) username =
            profiles
            |> Map.tryPick(fun uid profileName ->
                let u = cp.GetUser(uid)
                if u.Username = username then
                    Some (u,profileName)
                else None
            )
        let onProfileSearch reply =
            function
                | un, Some(_user,pn) ->
                    reply <| sprintf "♫ %s has a path name it's O.S.C.A.R. ♫ Wait. No. My bad. It's %s" un pn
                | un, None ->
                    reply <| sprintf "♪ My baloney has a first name ♪, however %s, does not." un

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
        let getAuthorProfile (su:SocketUser) cp singleReply =
            (su.Username, findExileUser cp su.Username)
            |> onProfileSearch singleReply 
    open Impl


    let setProfile =
        "setProfile", Complex (fun cp sm ->
            let singleReply txt = SocketMessage.reply sm txt
            match sm.Content with
            | After "setProfile " (RMatchGroup "\w.+$" 0 (Trim profileName)) ->
                profiles <-
                    profiles
                    |> Map.add sm.Author.Id profileName
                async {
                    do!
                        sprintf "I set %s's poeprofile name to %s. I hope you capitalized it properly." sm.Author.Username profileName
                        |> SocketMessage.reply sm
                        |> Async.AwaitTask
                        |> Async.Ignore
                    do!
                        sprintf "I am still a child, don't expect me to remember for long. I have a lot of growing to do."
                        |> SocketMessage.reply sm
                        |> Async.AwaitTask
                        |> Async.Ignore
                }
                |> Async.StartAsTask
                :> Task
                |> Some
            | RMatch "setProfile$" _ ->
                (sm.Author.Username, findExileUser cp sm.Author.Username)
                |> onProfileSearch singleReply 
                |> Some
            | _ -> None

        )
    let getProfile =
        "getProfile", 
            Complex (fun cp sm ->
                let singleReply = SocketMessage.reply sm
                match sm.Content with
                | After "getProfile " (UserName userName) ->
                    (userName,findExileUser cp userName)
                    |> onProfileSearch singleReply
                    |> Some
                | RMatch "getProfile\s*$" _ ->
                    getAuthorProfile sm.Author cp singleReply
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
         "language","I speak only F#"
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

