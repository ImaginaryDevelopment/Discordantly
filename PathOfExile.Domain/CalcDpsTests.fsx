#load "Dps.fs"

open PathOfExile.Domain.Dps
open Pad
open GameMath
open Skills
open Hits
open Weapons

let expectPair eDr =
    function
        | {Min=mn;Max=mx} when mn=eDr.Min && mx=eDr.Max -> ()
        | {Min=mn;Max=mx}  when mn=eDr.Min -> failwithf "Bad max value:%f, expected %f" mx eDr.Max
        | {Min=mn;Max=mx}  when mx=eDr.Max -> failwithf "Bad min value:%f, expected %f" mn eDr.Min
        | {Min=mn;Max=mx} -> failwithf "Bad min/max values:(%f,%f), expected(%f,%f)" mn mx eDr.Min eDr.Max
module TestConstants =
    let empyreanScalpel ={Name="Empyrean Scalpel";Damage=Map[Phys,{Min=71m;Max=164m}];Speed=1.4m;DamageMods=Map[Some Poison,[More,60m;Increase,26m]]}
    let ls = {Skills.Name="Lancing Steel";Eff=0.81m}
    let adds = Map[Phys,{Min=9.0m;Max=15.0m}]
    let mods = Map[
                    none, [Increase,145m; More, -37m]
                    some Phys,[Increase,276m; More,20m]
    ]
let _ =
    let expected = 0.756m
    let result = [-0.37m;0.20m] |> foldMore
    if result <> expected then
        failwithf "Bad foldMore %f, expected %f" result expected
let _ =
    TestConstants.empyreanScalpel
    |> getHitBase TestConstants.ls (TestConstants.adds |> getDamageType Phys)
    |> expectPair {Min=64.8m;Max= 144.99m}
    ()

let _ =
    TestConstants.empyreanScalpel
    |> getWeaponHit TestConstants.adds TestConstants.ls TestConstants.mods
    |> getDamageType Phys
    |> function
        |None -> failwith "no Phys found"
        |Some (_,x) -> x

    |> expectPair {Min=255.231648m;Max=571.0808124m}
    ()
