module PathOfExile.Domain.Dps
// Phys damage calc jives with PoB up to a single hit
// conversions and attack speed not included yet

let debug = true
// helpers so that the output in linqpad isn't cluttered by `Some _` table wrapper(s)
module Pad =
    open System.Runtime.CompilerServices

#if LINQPAD
    type Option'<'t>(x:'t option) =
        member __.Value = x
        static member op_Implicit(x:Option'<'t>):Option<'t> = x.Value
        member x.ToDump() = sprintf "%A" x
        override x.ToString() = sprintf "%A" x.Value
       
    let none = Option'(None)
    let some x = Option'(Some x)
    let (|Some'|None'|) (x:Option'<'t>) =
        match x.Value with
        | Some v -> Some' v
        | None -> None'
    let unwrapOpt =
        function
        | Some' x -> Some x
        | None' -> None

#else
    let some x = Some x
    let none = None
    let (|Some'|None'|) =
        function
        | Some x -> Some' x
        | None -> None'
    [<Extension>]
    type Extensions =
        [<Extension>]
        static member Dump (x:obj) = printfn "%A" x
        [<Extension>]
        static member Dump (x:obj,title:string) = printfn "%s:%A" title x
    type Option'<'t> = Option<'t>
    let Dump x = x.Dump(); x
    let unwrapOpt = id

#endif
open Pad

type NumberBase = decimal
// general game math formula(s)
module GameMath=
    type DamageType =
        |Phys
        |Lightning
        |Cold
        |Fire
        |Chaos
        |Poison
        |Bleed
        with
            override x.ToString() = sprintf "%A" x
            member x.ToDump() = string x
    //type DamageVector = {Type:DamageType;Min:NumberBase;Max:NumberBase}
    type DamageRange = {Min:NumberBase;Max:NumberBase}
    type DamageMap = Map<DamageType,DamageRange>
    type ModifierType = |More | Increase
    type ModMap = Map<DamageType option, (ModifierType * NumberBase) list>
    type AddMap = Map<DamageType, DamageRange>
    let getDamageType dt' = Map.tryPick(fun dt v -> if dt = dt' then Some v else None)
    let getMods dt' = Map.toSeq >> Seq.choose (fun (k,v) -> match k with | None -> Some v | Some k -> if k = dt' then Some v else None) >> Seq.collect id



    let zero = LanguagePrimitives.GenericZero
    let noDamage = {Min=zero;Max=zero}
    let getEff base' inc more:NumberBase=
        let baseInc = (base'*(100m+inc)/100m)
        let total = baseInc * more
        if debug then
            (base',inc,more,baseInc,total).Dump("base',inc,more,baseInc,total")
        total
    let foldMinMax dr dr' =
        {Min=dr.Min+dr'.Min; Max=dr.Max+dr.Max}
    let foldDamage:seq<DamageRange> -> DamageRange =
        Seq.fold foldMinMax noDamage
    let filterType (dt':'a): ('a option*'b) -> 'b option =
        function
        // this value applies to all types
        |None,v -> Some v
        // this value applies only to the type we are interested in
        |Some dt,v when dt = dt' -> Some v
        | _ -> None
        
    let foldDmgType type' :seq<DamageType option*NumberBase> -> NumberBase =
        Seq.fold (fun t x ->
            match filterType type' x with
            | Some v -> t + v
            | None -> t
        ) (LanguagePrimitives.GenericZero)
        
    let foldMore :seq<NumberBase> -> NumberBase =
        Seq.fold (fun t v ->
            if v > LanguagePrimitives.GenericOne then invalidOp "Bad more mod"
            t * (LanguagePrimitives.GenericOne + v)
        ) (LanguagePrimitives.GenericOne)
        >> fun v ->
            if debug then
                v.Dump("More folded")
            v
()
open GameMath
module Weapons =
    let getWeaponDmg minDmg maxDmg speed:NumberBase=
        (minDmg + maxDmg) / 2.0m * speed
    type Weapon =
        {Name:string; Speed:NumberBase; Damage:DamageMap; DamageMods: ModMap} with
            member x.GetDmg type' =
                x.Damage
                |> getDamageType type'
                |> Option.defaultValue noDamage
            member x.PhysDmg=
                let dr =
                    x.Damage
                    |> Map.tryPick(fun dt v -> if dt = Phys then Some v else None)
                    |> Option.defaultValue noDamage
                getWeaponDmg dr.Min dr.Max x.Speed
    type WeaponStyle =
        |One of Weapon
        |Two of Weapon*Weapon
    ()
()
open Weapons

module Skills =
    type Skill = {Name:string; Eff:NumberBase}
    ()
()
open Skills
    
//type WeaponSkill = {Skill:Skill; Weapon:Weapon} with
//    member x.Min = x.Weapon.Min * x.Weapon.Speed * x.Skill.Eff
module Hits =
    // adds include things like added flat, added X from other sources (Jewels,Gear etc. not to include the flats on the weapon)
    let getHitBase (sk:Skill) (add:DamageRange option) (w:Weapon) =
        w.GetDmg Phys
        |> fun x ->
            let eff = sk.Eff
            add
            |> Option.defaultValue noDamage
            |> fun add ->
                {Min= x.Min*eff+add.Min * eff;Max=x.Max*eff+add.Max*eff}
    let getWeaponHit (adds:AddMap) (sk:Skill) (modifications:ModMap) (w:Weapon) =
        w.Damage
        |> Map.map (fun dt wdr ->
            let moar = modifications |> getMods dt |> Seq.choose (function |More,x -> Some x | _ -> None) |> Seq.map (fun x -> x / 100m) |> foldMore
            let inc = modifications |> getMods dt |> Seq.choose(function |Increase,x -> Some (Some dt,x) | _ -> None) |> foldDmgType dt
            let add = adds |> getDamageType dt
            let getEff v = getEff v inc moar
            let dr = getHitBase sk add w
            let result = {Min=getEff dr.Min;Max=getEff dr.Max}
            if debug then
                (moar,inc,dr,result).Dump("m,inc,dr,r")
            dt,result
        )
    ()
()
open Hits
[<NoComparison>]
type CharacterAttack = {WeaponStyle:WeaponStyle; Modifications:ModMap;Skill:Skill; SpeedIncrease:NumberBase;SpeedMore:NumberBase;Adds:AddMap} with
    member x.Get1h() =
        match x.WeaponStyle with
        | One(w) -> w
        | Two(w,_) -> w
    member x.GetPhys (w:Weapon) =
        w.GetDmg Phys
        |> fun dr ->
            let eff = x.Skill.Eff
            x.Adds
            |> getDamageType Phys
            |> Option.defaultValue noDamage
            |> fun dr' ->
                {Min=dr.Min*eff+dr'.Min*eff;Max=dr.Max*eff+dr'.Max*eff}
    member x.Phys1hHit =
        x.Get1h()
        |> fun x -> x.Dump("1h"); x
        |> getHitBase x.Skill (x.Adds |> getDamageType Phys)

    member x.Phys2h =
        match x.WeaponStyle with
        | One _ -> None
        | Two (_,w2) -> Some<| x.GetPhys w2
    member x.Phys1hHitTotal =
        let moar = x.Modifications|> getMods Phys |> Seq.choose(function |More,v -> Some v | _ -> None) |> Seq.map(fun x -> x / 100m) |> foldMore
        let inc = x.Modifications |> getMods Phys |> Seq.choose(function |Increase,v -> Some v | _ -> None) |> Seq.fold(fun inc v -> inc + v) zero

        let getEff v = getEff v inc moar
        let result =  {Min=getEff x.Phys1hHit.Min; Max= getEff x.Phys1hHit.Max}
        if debug then
            (moar,inc,x.Phys1hHit,result).Dump("m,inc,phys1hHit,r")
        result
    member __.Total1H = ()

module private MyExamples =
    let entropyScratch = {Name="Entropy Scratch";Damage=Map[Phys,{Min=109.0m;Max=310.0m}]; Speed=1.63m;DamageMods=Map[some Poison, [More,60m]]}
    let dragonBarb = {Name="Dragon Barb";Damage=Map[Phys,{ Min=91.0m; Max=275.0m}];Speed=1.94m;DamageMods=Map[Some Poison, [Increase,35m]]}
    let empyreanScalpel ={Name="Empyrean Scalpel";Damage=Map[Phys,{Min=71m;Max=164m}];Speed=1.4m;DamageMods=Map[Some Poison,[More,60m;Increase,26m]]}
    let lancingSteel = {Name="Lancing Steel"; Eff=0.81m}

    let iveBeenImpaled =
        {   WeaponStyle=Two(empyreanScalpel,dragonBarb)
            SpeedIncrease= 0.88m; SpeedMore= 0.1m;Skill=lancingSteel
            Modifications=Map[
                            none, [Increase,145m; More, -37m]
                            some Phys,[Increase,276m; More,20m]
                        ]
            Adds=Map[Phys, {Min=9.0m;Max=15.0m} ]
        }
        
    iveBeenImpaled 
    |> Dump
    |> ignore