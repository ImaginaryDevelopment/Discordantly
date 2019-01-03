module PoBCommandAdapter

open System
open System.Threading.Tasks

open Schema.BReusable
open Schema.Helpers
open Schema.Helpers.StringPatterns

open CommandSchema


module PoB = PathOfSupporting.TreeParsing.PathOfBuildingParsing
module Gems = PathOfSupporting.TreeParsing.Gems
open PathOfSupporting.TreeParsing.Gems

// find a build's enabled skills along with minimum level to equip
let getSkills =
    let readSkillUri uri=
        let skills =
            PoB.Impl.fromPasteBin uri
            |> Option.bind PoB.parseText
            |> Option.bind(fun ch ->
                printfn "Found a character"
                ch.Skills.SkillGroups
                |> List.filter(fun sg -> sg.IsEnabled)
                |> List.collect(fun sg -> sg.Gems)
                |> List.filter(fun g -> g.Enabled)
                // if we did not manage to find a name (probably an item provided skill, not a skill gem)
                |> List.sortBy(function |{Name=NonValueString _} as x -> sprintf "ZzZ%s" x.SkillId |x -> x.Name)
                |> List.map(function |{Name=NonValueString _} as x -> x.SkillId | x -> x.Name)
                |> List.groupBy id
                |> List.map(fun (name,v) -> name, v |> Seq.length)
                |> function
                    |[] -> None
                    | skills ->
                        printfn "Found skill names!"
                        let skillNames = skills |> List.map fst
                        let skills = skills |> Map.ofList
                        Gems.getGemReqLevels {SkillGemJsonPath.ResourceDirectory=contentPath;Filename=None} null skillNames
                        |> Result.map(
                            List.map(fun (n,lvlOpt) ->
                                let suffix =
                                    if skills.ContainsKey n && skills.[n] > 1 then
                                        sprintf "(%i)" skills.[n]
                                    else String.Empty

                                sprintf "%s%s - %s" n suffix (Reflection.fDisplay lvlOpt))
                            >> Schema.BReusable.StringHelpers.delimit ",")
                        |> function
                            |Ok x -> Some x
                            | _ -> None
            )
        skills
    "getSkills",
    {   TriggerHelp=[
            "Examples:"
            "getSkills <pastebin uri>"
            "getSkills 'Brutality'"
        ]
        F= Complex (fun _cp sm ->
            match sm.Content with
            // https://pastebin.com/284NtPT5
            | After "getSkill" (Link (Contains "pastebin" & uri))
            | After "getSkills" (Link (Contains "pastebin" & uri)) ->
                readSkillUri uri
                |> Option.map (sendMessageAsync sm >> Async.Ignore)
            | After "getSkill" (Quoted(skillName,NonValueString _))
            | After "getSkills" (Quoted(skillName,NonValueString _)) ->
                Gems.getSkillGem {SkillGemJsonPath.ResourceDirectory=contentPath;Filename=None} skillName
                |> function
                    |Error (msg,_) ->
                        sprintf "Could not locate a skill named %s" skillName
                        |> sendMessageAsync sm
                    |Ok g ->
                        sprintf "%s - %i" g.Name g.Level
                        |> sendMessageAsync sm
                    |> Async.Ignore
                    |> Some

            | _ -> None

        )

    }
