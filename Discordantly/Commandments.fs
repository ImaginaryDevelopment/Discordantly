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
    open Schema.Helpers

    type AsyncReplyWrapper = {RestUserMessage:Rest.RestUserMessage; DeleteMe:bool}
    let deleteAndReply (sm:SocketMessage) (txt:string):Task<_> =
        async{
            do! Async.AwaitTask(sm.DeleteAsync())
            return sm.Channel.SendMessageAsync(txt)
        }
        |> Async.StartAsTask

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
    | Multiple of string list
    //| Complex of (ClientProxy -> SocketMessage -> Task<Rest.RestUserMessage list> option)
    | Complex of (ClientProxy -> SocketMessage -> Task option)
type TriggerType =
    | Command of string
    | Method of beginsWith:string

type Command = {Trigger:string;ReplyType:ReplyType}
type NotSimple = {TriggerHelp:string list;F:ReplyType}

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
    open PathOfExile.Domain.TreeParsing.PassiveJsParsing


    let setProfile:string*NotSimple =
        "setProfile",
        {
            TriggerHelp=["setProfile [account name] (for example: DevelopersDevelopersDevelopers)"]
            F= Complex (fun cp sm ->
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
    }

    let getProfile:string*NotSimple=
        "getProfile",
        {
            TriggerHelp=["alone it will get your own";"or specify a username to another user's profile if they have one"]
            F=
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
        }
    //type NotSimple<'t> = {Trigger:string;TriggerHelp:string list;F:'t}
        //"getProfile", {SimpleTrigger="getProfile [UserName]";TriggerHelp=;F=f}
    let getClass =
        "getClass",
        {
            TriggerHelp=["getClass [PassiveTreeLink] (for example: `` `https://www.pathofexile.com/fullscreen-passive-skill-tree/3.4.5/AAAABAMAAQ==`\r\n``)"]
            F= Complex (fun cp sm ->
                match sm.Content with
                | After "getClass <" (Before ">" uri)
                | After "getClass `" (Before "`" uri)->
                    let reggie = Impl.regIt uri
                    printfn "Found uri, worked?%A, he's:%s" reggie.IsSome uri
                    reggie
                    |> Option.bind (fun _ ->
                            match nodeCache with
                            | Some nc -> Some nc
                            | None ->
                                match Impl.getMappedNodes(System.Environment.CurrentDirectory) with
                                | Some nc ->
                                    nodeCache <- Some nc
                                    Some nc
                                | None -> None
                    )
                    |> Option.bind(fun nc -> decodeUrl nc.nodes uri)
                    |> Option.bind(fun tree -> tree.Class)
                    |> Option.map(fun x ->
                            let display = Reflection.fDisplay x
                            SocketMessage.reply' sm <| display
                            :> Task
                    )
                | After "getClass " _ ->
                    sprintf "@%s !getClass uri must be surrounded by <> or `" sm.Author.Username
                    |> SocketMessage.deleteAndReply sm 
                    :> Task
                    |> Some
                | _ -> None
            )
    }

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

    let notSimpleReplies : Map<string, NotSimple>=
        [
            Exiling.getProfile
            Exiling.setProfile
            Exiling.getClass
        ]
        |> List.map(fun (x,c) ->  sprintf "!%s" x, c)
        |> Map.ofList
    ()

    let help =
        let s = simpleReplies |> Map.toSeq |> Seq.map fst |> List.ofSeq
        let ns = notSimpleReplies |> Map.toSeq |> Seq.map fst |> List.ofSeq
        let items =
            ("  -",s@ns)
            ||> List.fold(fun text line ->
                sprintf "%s\r\n  %s" text line
            )
        ["command list:";items]
        //|> Multiple

    // dispatch ( take in the author id, bot id, full message text
    let (|Simpleton|NotSimpleton|UhOh|IgnoranceIsBliss|) (authorId,botUserId, content) =
        let ns (cmd:string,help:string list, f) = NotSimpleton (cmd,help,f)
        if authorId = botUserId || content |> String.IsNullOrWhiteSpace then
            IgnoranceIsBliss
        elif simpleReplies.ContainsKey content then
            Simpleton [simpleReplies.[content]]
        elif content = "!help" then
            //let result : ReplyType = help
            Simpleton help
        else
            notSimpleReplies
            |> Map.tryFindKey(fun k _ -> content.StartsWith k)
            |> function
                | Some x -> ns (x,notSimpleReplies.[x].TriggerHelp,notSimpleReplies.[x].F)
                | None ->
                    if content.StartsWith "!" then
                        UhOh
                    else IgnoranceIsBliss

