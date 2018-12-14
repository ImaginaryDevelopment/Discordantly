namespace Discordantly

open System.Threading.Tasks

open Schema.BReusable
open Schema.Helpers
open Schema.Helpers.StringPatterns

open Discord
open Discord.WebSocket

open CommandSchema


    //abstract member () ()
// [<NoSubstitute("for you")>]

module Generalities =
    open System.Collections.Generic

    let cleaning:string*NotSimple =
        "maidservice",
        {   TriggerHelp=["Maid Service, you want hot towel?"]
            F= Complex (fun cp sm ->
                match sm.Content with 
                | EndsWithI "maidservice" _ ->
                    printfn "Cleaning up msgs"
                    let msgs = sm.Channel.GetMessagesAsync 40
                    let a =
                        async{
                            let! msgs = msgs
                            for sm in msgs do
                                printfn "Checking a message"
                                if sm.Author.Id = cp.CurrentUser.Id || sm.Content.StartsWith "!" then
                                    printfn "imma delete this one"
                                    sm.DeleteAsync() |> ignore
                                    do! Async.Sleep 800
                                    printfn "I finished one?"
                        }
                    //sm.Channel.CachedMessages
                    Async.StartAsTask a
                    :> Task
                    |> Some
                | _ -> None
            )
        }
// profile tracking, etc
module Exiling =
    open Schema.Helpers.StringPatterns
    type private ExileMap = Map<uint64,string>
    module Impl =
        open PathOfExile.Domain

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
        let onProfileSearch: _ -> string list=
            function
                | un, Some(_user,pn) ->
                    let baseResponse =
                        [   sprintf "♫ %s has a path name it's O.S.C.A.R. ♫ Wait. No. My bad. It's %s" un pn
                            sprintf "You just got served <https://www.pathofexile.com/account/view-profile/%s/characters>" pn
                        ]
                    //get character list somehow?
                    async{
                        match! PathOfExile.Domain.HtmlParsing.getCharacters pn with
                        | HtmlParsing.GetResult.FailedDeserialize ->
                            return baseResponse@[sprintf "Could not find characters"]
                        | HtmlParsing.GetResult.Success chars ->
                            let lines =
                                chars
                                //|> Seq.groupBy(fun x -> x.League)
                                //|> Seq.map(fun (_,x) -> x |> Seq.maxBy(fun c -> c.Level))
                                |> Seq.sortBy(fun x -> x.League = "Standard", x.Level)
                                |> Seq.map(fun x -> sprintf "%s Lvl %i %s - %s League" x.Name x.Level x.Class x.League)
                                |> List.ofSeq
                            return baseResponse@lines
                        | HtmlParsing.GetResult.FailedHttp msg ->
                            return baseResponse@[sprintf "Failed to fetch characters: %s" msg]
                    }
                    |> Async.RunSynchronously
                | un, None ->
                        [sprintf "♪ My baloney has a first name ♪, however %s, does not." un]

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
    open PathOfExile.Domain.TreeParsing.PathOfBuildingParsing
    open Schema.Helpers
    module HtmlParsing = PathOfExile.Domain.HtmlParsing


    let setProfile:string*NotSimple =
        "setProfile",
        {
            TriggerHelp=[
                "setProfile [account name] (for example: DevelopersDevelopersDevelopers)"
                "setProfile @userName '[account name]'"
            ]
            F=
                let replySet (sm:SocketMessage) username profileName =
                    async {
                        let! _ = sm.Channel.SendMessageAsync(sprintf "I set `%s`'s poeprofile name to `%s`. I hope you capitalized it properly." username profileName)
                        let! _ = sendMessageAsync sm <| sprintf "I am growing, and I think I'll remember this, but I may forget."
                        ()
                    }
                    |> Async.Ignore
                    |> Async.StartAsTask
                    :> Task
                    |> Some
                Complex (fun cp sm ->
                match sm.Content with
                | AfterI "setProfile" (RMatch "@.* '([^']+)'$" (RGroup 1 accountName)) ->
                    if sm.MentionedUsers.Count = 1 then
                        let targetUser = sm.MentionedUsers |> Seq.head
                        printfn "Running setProfile @ '%s'" accountName
                        Impl.Profiling.profiles.Value <-
                            Impl.Profiling.profiles.Value
                            |> Map.add targetUser.Id accountName
                        replySet sm targetUser.Username accountName
                    else
                        sendMessageAsync sm "unable to understand your comment, mention the user then the profile name in ''"
                        :> Task
                        |> Some
                | AfterI "setProfile " (RMatchGroup "\w.+$" 0 (Trim profileName)) ->
                    printfn "Running setProfile ..."
                    Impl.Profiling.profiles.Value <-
                        Impl.Profiling.profiles.Value
                        |> Map.add sm.Author.Id profileName
                    replySet sm sm.Author.Username profileName
                | RMatchI "setProfile$" _ ->
                    (sm.Author.Username, Impl.Profiling.findExileUser cp sm.Author.Username)
                    |> onProfileSearch
                    |> sendMessagesAsync sm
                    |> Async.Ignore
                    |> Async.StartAsTask
                    :> Task
                    |> Some
                | _ -> None
        )
    }

    let getProfile:string*NotSimple=
        "getProfile",
        {
            TriggerHelp=[
                "`getProfile` help - alone it will get your own"
                "or specify a username to get another user's profile if they have one"
                "`getProfiles` - list users with profiles"
            ]
            F=
                let serveProfile sm cp userName =
                    (userName,Impl.Profiling.findExileUser cp userName)
                    |> onProfileSearch
                    |> sendMessagesAsync sm
                    |> Async.Ignore
                    |> Async.StartAsTask
                    :> Task
                    |> Some
                Complex (fun cp sm ->
                    match sm.Content with
                    | AfterI "getProfiles" (NonValueString _) ->
                        let getUserInfo u pn =
                            async{
                                let! msg = sm.Channel.SendMessageAsync <| sprintf "%s is Exile %s" u pn
                                match! HtmlParsing.getCharacters pn with
                                | HtmlParsing.GetResult.FailedDeserialize -> ()
                                | HtmlParsing.GetResult.Success chars ->
                                    let highlightChar =
                                        chars
                                        //|> Seq.filter(fun ch -> StringHelpers.containsI "Standard" ch.League|> not)
                                        |> Seq.fold(fun highOpt char ->
                                            match highOpt with
                                            | None -> Some char
                                            | Some ({League=ContainsI "standard"} as x) when char.League |> StringHelpers.containsI "standard" ->
                                                if(char.Level > x.Level) then
                                                    Some char
                                                else Some x
                                            | Some {League=ContainsI "standard"} -> Some char
                                            // all standard comparison are done, league only from here on out
                                            | Some x ->
                                                if StringHelpers.containsI "standard" char.League then
                                                    Some x
                                                elif char.Level > x.Level then
                                                    Some char
                                                else Some x
                                        ) None
                                    match highlightChar with
                                    | Some char ->
                                        let msg':Rest.RestUserMessage = msg
                                        let f (mp:MessageProperties):unit=
                                            let txt = sprintf "%s is %s (%s - %i %s)" u pn char.Name char.Level char.League
                                            mp.Content <- Optional<string> txt
                                            ()
                                        let f' = System.Action<_>(f)

                                        let! _ = Async.AwaitTask(msg'.ModifyAsync(func=f',options=null))
                                        ()
                                    | None -> ()
                                | HtmlParsing.GetResult.FailedHttp msg ->
                                    eprintfn"Failed to get characters for %s(%s): %s" u pn msg
                            }
                        let exiles =
                            Impl.Profiling.profiles.Value
                            |> Map.toSeq
                            |> Seq.map(fun (x,y)-> cp.GetUser x |> fun u -> u.Username,y)
                        async {
                            let! _ = sm.Channel.SendMessageAsync "For more information use getProfile [username]"
                            exiles
                            |> Seq.map(fun (u,pn) -> getUserInfo u pn)
                            |> Async.Parallel
                            |> Async.Ignore
                            |> Async.Start
                        }
                        |> Async.StartAsTask
                        :> Task
                        |> Some
                    | ContainsI "getProfile" when sm.MentionedUsers.Count = 1 ->
                        let un = sm.MentionedUsers |> Seq.head
                        serveProfile sm cp un.Username
                    | AfterI "getProfile " (UserName userName) ->
                        serveProfile sm cp userName
                    | RMatch "getProfile\s*$" _ ->
                        getAuthorProfile sm.Author cp
                        |> sendMessagesAsync sm
                        |> Async.Ignore
                        |> Async.StartAsTask
                        :> Task
                        |> Some
                    | _ -> None
                )
        }
    let getMappedNodes directory =
        match Impl.getMappedNodes directory with
        | Some nc ->
            nodeCache <- Some nc
            Some nc
        | None -> None
    module PassiveParsing = PathOfExile.Domain.TreeParsing.PassiveJsParsing
    module Gems = PathOfExile.Domain.TreeParsing.Gems
    let getStat =
        "getStat",
        {
            TriggerHelp=["getStat '[stat]' (PassiveTreeLink)"; "stats:"; "  Spell Damage";"  Life"]
            F= Complex(fun cp sm ->
                match sm.Content with
                | NonValueString _ -> None
                // get just the count of available nodes with that stat
                | After "getStat " (Quoted (ValueString stat,Link uri)) ->
                    printfn "we have a full get stat request"
                    match getMappedNodes contentPath with
                    | None -> Some(sendMessageAsync sm "No node information available" :> Task)
                    | Some nc ->
                        printfn "node information found"
                        let search = stat
                        //    match stat with
                        //    | "Spell Damage" ->
                        //        "% increased Spell Damage"
                        //    | _ -> stat
                        match PassiveParsing.decodeUrl nc.nodes uri with
                        | None -> Some(sendMessageAsync sm "Tree decoding failed" :> Task)
                        | Some treeInfo ->
                            printfn "Conducting Search"
                            let relevantValues=
                                treeInfo.Nodes
                                |> Seq.collect(fun n -> n.sd)
                                |> Seq.filter (fun x -> x.Contains(search, System.StringComparison.InvariantCultureIgnoreCase))
                                |> List.ofSeq
                            printfn "Search completed"
                            let count = relevantValues.Length
                            Some(sendMessageAsync sm <| sprintf "Found %i nodes with %s in the tree" count stat :> Task)
                | After "getStat " (Quoted (ValueString stat,_))
                | After "getStat " (Quoted (ValueString stat,_)) ->
                    match getMappedNodes contentPath with
                    | None -> Some(sendMessageAsync sm "No node information available" :> Task)
                    | Some nc ->
                        let search =
                            match stat with
                            | "Spell Damage" ->
                                "% increased Spell Damage"
                            | _ -> stat
                        let relevantValues=
                            nc.nodes.Values
                            |> Seq.collect(fun x -> x.sd)
                            |> Seq.filter (fun x -> x.EndsWith search)
                            |> List.ofSeq
                        let count = relevantValues.Length
                        Some(sendMessageAsync sm <| sprintf "Found %i nodes with %s" count stat :> Task)
                | _ -> None

            )
        }
   // passive tree
    let getClass =
        "getClass",
        {
            TriggerHelp=["getClass [PassiveTreeLink] (for example: `` `https://www.pathofexile.com/fullscreen-passive-skill-tree/3.4.5/AAAABAMAAQ==`\r\n``)"]
            F= Complex (fun cp sm ->
                match sm.Content with
                | After "getClass <" (Before ">" uri)
                | After "getClass `" (Before "`" uri)->
                    let reggie = Impl.regPassiveTree uri
                    printfn "Found uri, worked?%A, he's:%s" reggie.IsSome uri
                    reggie
                    |> Option.bind (fun _ ->
                        getMappedNodes System.Environment.CurrentDirectory
                    )
                    |> Option.bind(fun nc -> decodeUrl nc.nodes uri)
                    |> Option.bind(fun tree -> tree.Class)
                    |> Option.map(fun x ->
                            let display = Reflection.fDisplay x
                            sendMessageAsync sm <| display
                            :> Task
                    )
                | After "getClass " _ ->
                    sprintf "@%s !getClass uri must be surrounded by <> or `" sm.Author.Username
                    //|> SocketMessage.deleteAndReply sm 
                    |> sendMessageAsync  sm
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
            Exiling.getStat
            PoBCommandAdapter.getSkills
            Generalities.cleaning
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

    // dispatch ( take in the author id, bot id, full message text
    let (|Simpleton|NotSimpleton|UhOh|IgnoranceIsBliss|) (authorId,botUserId, content) =
        let ns (cmd:string,help:string list, f) = NotSimpleton (cmd,help,f)
        match content with
        | _ when authorId = botUserId -> IgnoranceIsBliss
        | NonValueString _ -> IgnoranceIsBliss
        | _ when simpleReplies.ContainsKey content -> Simpleton [simpleReplies.[content]]
        | EqualsI "!help" -> Simpleton help
        | AfterI "!help" (RMatchGroup "^\s(\w+)" 1 value) ->
            let searchValue = sprintf "!%s" value
            match notSimpleReplies |> Map.tryFindKey(fun k _ -> String.equalsI k searchValue) with
            | Some key ->
                Simpleton notSimpleReplies.[key].TriggerHelp
                //|> List.map MessageLifeType.Keepsies
                //|> SocketMessage.reply sm
                //:> Task
                //|> Some
            | None ->
                Simpleton ["Sorry, I am unable to determine which command you want help with"]
        | _ ->
            notSimpleReplies
            |> Map.tryFindKey(fun k _ -> content.StartsWith k)
            |> function
                | Some x -> ns (x,notSimpleReplies.[x].TriggerHelp,notSimpleReplies.[x].F)
                | None ->
                    if content.StartsWith "!" then
                        UhOh
                    else IgnoranceIsBliss

