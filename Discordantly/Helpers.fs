﻿[<AutoOpen>]
module Helpers
open System
let isValueString = String.IsNullOrWhiteSpace >> not
let (|ValueString|NonValueString|) x =
    if isValueString x then ValueString x
    else NonValueString

let afterOpt (delimiter:string) (x:string) =
    match x.IndexOf delimiter with
    | z when z < 0 -> None
    | i -> Some x.[i + delimiter.Length  ..]
let after (delimiter:string) (x:string) =
    match afterOpt delimiter x with
    | None -> invalidOp <| sprintf "'%s' does not contain '%s'" x delimiter
    | Some x -> x
let (|After|_|) (delimiter:string) = afterOpt delimiter

let (|StartsWith|_|) (delimiter:string) =
    if String.IsNullOrEmpty delimiter then invalidOp "Starts with does not accept a null or empty string"
    function
    | null | "" -> None
    | x when x.StartsWith delimiter ->
            Some x
    | _ -> None
let (|Trim|) =
    function
    | null |"" as x -> x
    | x -> x.Trim()

open System.Text.RegularExpressions

module Regex =
    let (|RMatch|_|) (pattern:string) x =
        let m = Regex.Match(x,pattern=pattern)
        if m.Success then Some m
        else None
    let (|RMatchGroup|_|) (pattern:string) (i:int) =
        function
        | RMatch pattern m ->
            if m.Groups.Count < i then
                invalidOp <| sprintf "Pattern captured %i groups, but %i was requested" m.Groups.Count i
            else
                Some m.Groups.[i].Value
        | _ -> None


