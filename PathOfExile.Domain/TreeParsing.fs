module PathOfExile.Domain.TreeParsing

open System
open System.Collections.Generic
open Newtonsoft.Json.Linq
open Schema.Helpers

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
            IO.Path.Combine(folderPath,"Passives.json")
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
            | Regex.RMatch "AAAA[^?]+" m -> Some m.Value
            | _ -> None

    open Impl
    open PathOfExile.Domain.Classes
    let mutable nodeCache : PassiveLookup option= None
    type Tree = {Version:int; Class:ChClass option;Nodes:Node list}
    let decodeUrl (nodes:IDictionary<int,Node>) url =
        url
        |> regIt
        |> Option.map(fun x ->
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
        )
