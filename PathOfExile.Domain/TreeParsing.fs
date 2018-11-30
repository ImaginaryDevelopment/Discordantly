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

// incomplete translation
type SkillNodeGroup() = // https://github.com/PoESkillTree/PoESkillTree/blob/f4a6119be852315ca88c63d91a1acfb5901d4b8a/WPFSKillTree/SkillTreeFiles/SkillNodeGroup.cs
    member val Nodes = ResizeArray<SkillNode>() with get,set
// mostly complete
and SkillNode () = // https://github.com/PoESkillTree/PoESkillTree/blob/f4a6119be852315ca88c63d91a1acfb5901d4b8a/WPFSKillTree/SkillTreeFiles/SkillNode.cs
    member val Attributes = Dictionary<string,float list>() with get,set
    member val Connections = Set<int> with get,set
    member val Neighbor = ResizeArray<SkillNode>() :> IList<_>
    // the subset of neighbors with connections to this one
    member val VisibleNeighbors = ResizeArray<SkillNode>()
    member val SkillNodeGroup = None with get,set
    member val A = 0 with get,set
    member val attributes=Array.empty<string> with get,set
    member val Da = 0 with get,set
    member val G = 0 with get,set
    member val Ia = 0 with get,set
    member val Icon = String.Empty with get,set
    member val Id = 0us with get,set
    member val Type = NodeType.Normal with get,set
    member val LinkId = ResizeArray<uint16>()
    member val Name=String.Empty with get,set
    member val Orbit= 0 with get,set
    member val Sa = 0 with get,set
    member val IsSkilled = false with get,set
    member val Spc = Option<int>.None with get,set
    member val IsMultipleChoice = false with get,set
    member val IsMultipleChoiceOption = false with get,set
    member val passivePointsGranted = 0 with get,set //"passivePointsGranted": 1
    member val ascendancyName = String.Empty with get,set //"ascendancyName": "Raider"
    member val IsAscendancyStart = false with get,set //"isAscendancyStart": false
    member val reminderText = Array.empty<string> with get,set



// based on the javascript object the poe official tree viewer/planner returns
//require(['main'], function() {
//        require(['skilltree'], function (PassiveSkillTree) {
//            var passiveSkillTreeData = ...
module PassiveJsParsing =
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
    // by hand??? =(
    let getMappedNodes folderPath =
        IO.Path.Combine(folderPath,"Passives.json")
        |> IO.File.ReadAllText
        |> SuperSerial.deserialize<PassiveLookup>

