module PathOfExile.Domain.TreeParsing

open System
open System.Collections.Generic
open Newtonsoft.Json.Linq
open Schema.BReusable

type NodeType =
    |Normal
    |Notable
    |Keystone
    |Mastery
    |JewelSocket

//// incomplete translation
//type SkillNodeGroup() = // https://github.com/PoESkillTree/PoESkillTree/blob/f4a6119be852315ca88c63d91a1acfb5901d4b8a/WPFSKillTree/SkillTreeFiles/SkillNodeGroup.cs
//    member val Nodes = ResizeArray<SkillNode>() with get,set
//// mostly complete
//and SkillNode () = // https://github.com/PoESkillTree/PoESkillTree/blob/f4a6119be852315ca88c63d91a1acfb5901d4b8a/WPFSKillTree/SkillTreeFiles/SkillNode.cs
//    member val Attributes = Dictionary<string,float list>() with get,set
//    member val Connections = Set<int> with get,set
//    member val Neighbor = ResizeArray<SkillNode>() :> IList<_>
//    // the subset of neighbors with connections to this one
//    member val VisibleNeighbors = ResizeArray<SkillNode>()
//    member val SkillNodeGroup = None with get,set
//    member val A = 0 with get,set
//    member val attributes=Array.empty<string> with get,set
//    member val Da = 0 with get,set
//    member val G = 0 with get,set
//    member val Ia = 0 with get,set
//    member val Icon = String.Empty with get,set
//    member val Id = 0us with get,set
//    member val Type = NodeType.Normal with get,set
//    member val LinkId = ResizeArray<uint16>()
//    member val Name=String.Empty with get,set
//    member val Orbit= 0 with get,set
//    member val Sa = 0 with get,set
//    member val IsSkilled = false with get,set
//    member val Spc = Option<int>.None with get,set
//    member val IsMultipleChoice = false with get,set
//    member val IsMultipleChoiceOption = false with get,set
//    member val passivePointsGranted = 0 with get,set //"passivePointsGranted": 1
//    member val ascendancyName = String.Empty with get,set //"ascendancyName": "Raider"
//    member val IsAscendancyStart = false with get,set //"isAscendancyStart": false
//    member val reminderText = Array.empty<string> with get,set



// based on the javascript object the poe official tree viewer/planner returns
//require(['main'], function() {
//        require(['skilltree'], function (PassiveSkillTree) {
//            var passiveSkillTreeData = ...
module PassiveJsParsing =
    module Impl =
        open Schema.Helpers
        open System.Buffers.Text

        type Node = {
                g:int
                m:bool
                o:int
                da:int
                dn:string
                ia:int
                id:int
                ``in``:int list
                ks:bool
                sa:int
                sd:string list
                ``not``:bool
                ``out``:int list
                spc: int list
                icon:string
                oidx:int
                isJewelSocket:bool
                ascendancyName:string
                isMultipleChoice:bool
                isAscendancyStart:bool
                passivePointsGranted:int
                isMultipleChoiceOption:bool
        }
        type PassiveLookup = {
                root:JObject
                max_x:int
                max_y:int
                min_x:int
                min_y:int
                nodes:Dictionary<int,Node>
                groups:JObject
                extraImages:JObject
                characterData:JArray
                // don't want this one for now at least
                //assets:obj
                constants:JObject
        }
        let getMappedNodes folderPath =
            IO.Path.Combine(folderPath,"Passives3.5.json")
            |> IO.File.ReadAllText
            |> SuperSerial.deserialize<PassiveLookup>

        let decodebase64Url (x:string) =
            let partial =
                x
                 .Replace('-','+')
                 .Replace('_','/')
            match partial.Length % 4 with
            | 0 -> partial
            | 2 -> partial + "=="
            | 3 -> partial + "="
            | _ -> invalidArg "x" "Illegal base64url string"
            |> Convert.FromBase64String

        type Payload = {Version:int; CharClass:int; Ascendency:int; FullScreen:int; Nodes:int list}
        // translated from https://github.com/FWidm/poe-profile/blob/master/src/util/tree_codec.py
        // using https://repl.it/r
        // python struct pack/unpack reference: https://docs.python.org/3/library/struct.html
        let decodePayload (payload:byte[]) =
            let end' = if BitConverter.IsLittleEndian then Array.rev else id
            let bconv x = BitConverter.ToInt32(end' x,0)
            let bconv' x = BitConverter.ToUInt16(end' x,0) |> Convert.ToInt32
            {
                //bytes 0-3 contain the version
                Version = bconv payload.[0..3] //BitConverter.ToInt32((if BitConverter.IsLittleEndian then Array.rev else id )payload.[0..3],0)
                // bytes 4-6 contain cls, asc, full
                CharClass= payload.[4] |> Convert.ToInt32
                Ascendency = payload.[5] |> Convert.ToInt32
                FullScreen = payload.[6] |> Convert.ToInt32
                Nodes = Array.chunkBySize 2 payload.[7..] |> Array.map (bconv') |> List.ofArray
            }

        let regIt =
            function
            | RMatch "AAAA[^?]+" m -> Some m.Value
            | _ -> None

    open Impl
    open PathOfExile.Domain.Classes
    let mutable nodeCache : PassiveLookup option= None
    type Tree = {Version:int; Class:ChClass option;Nodes:Node list}
    let decodeUrl (nodes:IDictionary<int,Node>) url =
        url
        |> regIt
        |> Option.bind(fun x ->
            try
                x
                |> decodebase64Url
                |> decodePayload
                |> fun x ->
                    {
                        Version=x.Version
                        Class=
                            match x.CharClass,x.Ascendency with
                            | IsClass x -> Some x
                            | (cls,asc) ->
                                printfn "No class/asc setup for %A" (cls,asc)
                                None
                        Nodes= x.Nodes |> List.map(fun n -> nodes.[n])
                    }
                |> Some
            with ex ->
                printfn "Failed to decodeUrl '%s' '%s'" ex.Message url
                None
        )

// based on https://github.com/Kyle-Undefined/PoE-Bot/blob/997a15352c83b0959da03b1f59db95e4a5df758c/Helpers/PathOfBuildingHelper.cs
module PathOfBuildingParsing =
    type Gem = {SkillId:string; Name:string; Level:int; Quality:int;Enabled:bool}
    type SkillGroup = {Gems:Gem list;IsEnabled:bool; Slot:string; IsSelectedGroup:bool}
    type CharacterSkills = {MainSkillIndex:int; SkillGroups: SkillGroup list} with
        member x.MainSkillGroup =
            if x.MainSkillIndex >= 0 && x.MainSkillIndex < x.SkillGroups.Length then
                Some x.SkillGroups.[x.MainSkillIndex]
            else None
    type Summary = Map<string,string>
    type Item = {Id:int; Raw:string}
    type ItemSlot = {Name:string; Id:int}
    type MinionSummary = MinionSummary
    type Character = {Level:int; Class:string;Ascendancy:string; AuraCount:int; Config:string; CurseCount:int; Items:Item seq; ItemSlots:ItemSlot seq ; Summary:Summary; MinionSummary:MinionSummary;Skills:CharacterSkills;Tree:string}

    module Impl =
        open System.IO
        open Ionic.Zlib
        open System.Text
        open System.Xml.Linq
        open Schema.BReusable.StringHelpers
        open Schema.Helpers.Xml
        open System.Net.Http
        let getIntAttrib name =
            getAttribValue name
            >> Option.bind (|ParseInt|_|)

        let fromPasteBin (uri) =
            if not <| String.startsWith "https://pastebin.com/" uri then None
            else 
                let raw = "https://pastebin.com/raw/"
                let last = uri.Split('/') |> Array.last
                use client = new HttpClient()
                sprintf "%s%s" raw last
                |> client.GetStringAsync
                |> Async.AwaitTask
                |> Async.RunSynchronously
                |> Some

        let fromBase64ToXml(base64:string) =
            let dec =
                base64
                |> replace "-" "+"
                |> replace "_" "/"
                |> Convert.FromBase64String
            use input = new MemoryStream(dec)
            use deflate = new ZlibStream(input, CompressionMode.Decompress)
            use output = new MemoryStream()
            deflate.CopyTo output
            output.ToArray()
            |> Encoding.UTF8.GetString

        // include validation, only create if result is valid
        let tryCreateStat (xe:XElement) : (string*string) option =
            match getAttribValue "stat" xe,getAttribValue "value" xe with
            | Some (ValueString stat), Some(ValueString value) ->
                Some(stat,value)
            | None,_ -> None
            | _,None -> None
            | Some(statOpt), Some(valueOpt) ->
                eprintfn "Unexpected (stat='%s',value='%s')" statOpt valueOpt
                None

        let mapElementSequence key subKey f =
            getElement key
            >> Option.map(
                    getElements subKey
                    >> Seq.map f
                    >> List.ofSeq
            )

        let getPlayerStats: _ -> Map<string,string> option =
            mapElementSequence "Build" "PlayerStat" tryCreateStat
            >> Option.map(
                Seq.choose id
                >> Seq.groupBy fst
                >> Seq.choose(fun (name,x) ->
                    match x |> Seq.map snd |> Seq.tryHead with
                    | Some "0" -> None
                    | Some value ->
                        Some (name,value)
                    | None -> eprintfn "unexpected stat/value"; None
                )
                >> Map.ofSeq
            )

        let getGemsFromSkill =
            getElements "Gem"
            >> Seq.map(fun c ->
                {   SkillId= getAttribValueOrNull "skillId" c
                    Name= getAttribValueOrNull "nameSpec" c
                    Level= getIntAttrib "level" c |> Option.defaultValue -1
                    Quality = getIntAttrib "quality" c  |> Option.defaultValue -1
                    Enabled = getAttribValue "enabled" c |> Option.bind (|ParseBoolean|_|) |> Option.defaultValue false
                }
            )

        let getCharacterSkills (xe:XElement) =
            let mainGroup = xe |> getElement "Build" |> Option.bind (getIntAttrib "mainSocketGroup")
            let skills =
                xe
                |> mapElementSequence "Skills" "Skill" (
                        fun c ->        {
                                            Gems= getGemsFromSkill c |> List.ofSeq
                                            Slot= getAttribValue "slot" c |> Option.defaultValue null
                                            IsEnabled=getAttribValue "enabled" c |>  Option.bind (|ParseBoolean|_|) |> Option.defaultValue false
                                            IsSelectedGroup = false
                                        }
                )
            match skills with
            | Some skills ->
                Some {SkillGroups=skills;MainSkillIndex = Option.defaultValue -1 mainGroup}
            | None -> None

        let getItemSlots =
            mapElementSequence "Items" "Slot" (fun c ->
                    {
                        Name=getAttribValueOrNull "name" c
                        Id=getAttribValue "itemId" c |> Option.bind (|ParseInt|_|) |> Option.defaultValue -1
                    }
            )
        let getItems =
            mapElementSequence "Items" "Item" (fun c ->
                    {   Id=getAttribValue "id" c |> Option.bind (|ParseInt|_|) |> Option.defaultValue -1
                        Raw = string c
                    }
            )
        let parseCode (base64:string) : Character =
            let xDoc =
                let xml = fromBase64ToXml base64 |> XDocument.Parse
                let tXml = xml |> string |> replace "Spec:" String.Empty
                tXml |> XDocument.Parse
            dumpXE (Some "parsed") xDoc.Root
            let minionSum = MinionSummary

            let skills = getCharacterSkills xDoc.Root |> Option.defaultValue {SkillGroups=List.empty; MainSkillIndex= -1}

            let build = xDoc.Root |> getElement "Build"
            let getBuildAttribValue name = build |> Option.bind(getAttribValue name)
            let result =
                {   Skills=skills
                    Level= getBuildAttribValue "level"|>Option.bind (|ParseInt|_|) |> Option.defaultValue -1
                    Class= getBuildAttribValue "className" |> Option.defaultValue null
                    Ascendancy= getBuildAttribValue "ascendClassName" |> Option.defaultValue null
                    AuraCount = -1
                    CurseCount= -1
                    Items= getItems xDoc.Root |> Option.defaultValue List.empty :> _ seq
                    ItemSlots= getItemSlots xDoc.Root |> Option.defaultValue List.empty :> _ seq
                    Config=null
                    Summary=getPlayerStats xDoc.Root |> Option.defaultValue Map.empty
                    MinionSummary = minionSum
                    Tree = null
                }
            Schema.Helpers.Utils.dump (Some "character") id <| box result
            result

    open Impl
    let parseText =
        function
        | StartsWith "http" & Contains "pastebin" as x ->
            Impl.fromPasteBin x
        | x -> Some x
        >> Option.map parseCode
