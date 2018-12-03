namespace PathOfExile.Domain

open Schema.Helpers


module Classes =
    open Microsoft.FSharp.Reflection

    type MarauderType =
        |Juggernaut
        |Berserker
        |Chieftain
        with static member GetSubType = 
                function
                | 0 -> None
                | 1 -> Some Juggernaut
                | 2 -> Some Berserker
                | 3 -> Some Chieftain
                | x ->
                    printfn "%s doesn't know? %A" typeof<MarauderType>.Name x
                    None

    type RangerType =
        |Raider
        |Deadeye
        |Pathfinder
        with static member GetSubType =
                function
                | 0 -> None
                | 1 -> Some Raider
                | 2 -> Some Deadeye
                | 3 -> Some Pathfinder
                | x ->
                    printfn "%s doesn't know? %A" typeof<RangerType>.Name x
                    None
    type WitchType =
        | Occultist
        | Elementalist
        | Necromancer
        with static member GetSubType: _ -> WitchType option =
                function
                | 0 -> None
                | 1 -> Some Occultist
                | 2 -> Some Elementalist
                | 3 -> Some Necromancer
                | x ->
                    printfn "%s doesn't know? %A" typeof<WitchType>.Name x
                    None
    type DuelistType =
        | Slayer
        | Gladiator
        | Champion
        with static member GetSubType: _ -> DuelistType option =
                function
                | 0 -> None
                | 1 -> Some Slayer
                | 2 -> Some Gladiator
                | 3 -> Some Champion
                | x ->
                    printfn "%s doesn't know? %A" typeof<DuelistType>.Name x
                    None
    type TemplarType =
        | Inquisitor
        | Hierophant
        | Guardian
        with static member GetSubType: _ -> TemplarType option =
                function
                | 0 -> None
                | 1 -> Some Inquisitor
                | 2 -> Some Hierophant
                | 3 -> Some Guardian
                | x ->
                    printfn "%s doesn't know? %A" typeof<TemplarType>.Name x
                    None
    type ShadowType =
        | Assassin
        | Trickster
        | Saboteur
        with static member GetSubType: _ -> ShadowType option =
                function
                | 0 -> None
                | 1 -> Some Assassin
                | 2 -> Some Trickster
                | 3 -> Some Saboteur
                | x ->
                    printfn "%s doesn't know? %A" typeof<ShadowType>.Name x
                    None
    type ScionType =
        | Ascendant
        with static member GetSubType: _ -> ScionType option =
                function
                | 0 -> None
                | 1 -> Some Ascendant
                | x ->
                    printfn "%s doesn't know? %A" typeof<ScionType>.Name x
                    None

    [<RequireQualifiedAccess>]
    type ClassConstant =
        | Marauder = 1
        | Ranger = 2
        | Witch = 3
        | Duelist = 4
        | Templar = 5
        | Shadow = 6
        | Scion = 7
    let private getSub<'t> (x:int) =
        if x > 0 && System.Enum.GetValues typeof<'t> |> Seq.cast<int> |> Seq.contains x then
            System.Enum.ToObject(typeof<'t>,x)
            :?> 't
            |> Some
        else None

    type ChClass =
        |Marauder of MarauderType option
        |Ranger of RangerType option
        |Witch of  WitchType option
        |Duelist of DuelistType option
        |Templar of TemplarType option
        |Shadow of ShadowType option
        |Scion of ScionType option
        with
            member x.Name =
                //match x with
                //| Marauder _ -> Marauder None
                //| Ranger _ -> Ranger None
                //| Witch _ -> Witch None
                //| Duelist _ -> Duelist None
                //| Templar _ -> Templar None
                //| Shadow _ -> Shadow None
                //| Scion _ -> Scion None
                //|> fun x ->
                    FSharpValue.GetUnionFields(x,typeof<ChClass>)
                    |> fst
                    |> fun x ->
                        x.Name
            member x.HasAscendancy() =
                match x with
                | Marauder None
                | Ranger None
                | Witch None
                | Duelist None
                | Templar None
                | Shadow None
                | Scion None -> false
                | _ -> true
            override x.ToString():string =
                if not <| x.HasAscendancy() then
                    x.Name
                else sprintf "%A" x
            //    else
            //        FSharpValue.GetUnionFields(x,x.GetType())
            //        |> snd
            //        |> Seq.head // produces a ascendancy option but you can't cast that to object option apparently
            //        :?> obj option
            //        |> function
            //            | Some asc ->
            //                printfn "hello!"
            //                sprintf "%A(%s)"  (asc.ToString()) x.Name
            //            | None -> "?"


    let (|IsClass|_|) (x,y) =
        getSub<ClassConstant> x
        |> Option.bind (fun x ->
            let getSub f1 f2:ChClass option = f2 y |> f1 |> Some
            match x with
            | ClassConstant.Marauder -> getSub Marauder MarauderType.GetSubType
            | ClassConstant.Ranger -> getSub Ranger RangerType.GetSubType
            | ClassConstant.Witch -> getSub Witch WitchType.GetSubType
            | ClassConstant.Duelist -> getSub Duelist DuelistType.GetSubType
            | ClassConstant.Templar -> getSub Templar TemplarType.GetSubType
            | ClassConstant.Shadow -> getSub Shadow ShadowType.GetSubType
            | ClassConstant.Scion -> getSub Scion ScionType.GetSubType
            | _ ->
                printf "Unknown class %A" (x,y)
                None
        )
