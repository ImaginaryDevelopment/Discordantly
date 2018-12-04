[<AutoOpen>]
module Schema.Helpers

open System

let (|UnsafeNull|_|) x =
    if isNull <| box x then Some()
    else None

let prepareForDelimiting (delim:string) (x:string) =
    if String.IsNullOrWhiteSpace delim then
        invalidOp "bad delimiter"
    if String.IsNullOrWhiteSpace x then
        None
    else Some x

let after delim =
    prepareForDelimiting delim
    >> Option.bind(fun x ->
        let i = x.IndexOf delim
        if i >= 0 then
            Some (x.[i+delim.Length..])
        else None
    )

let before delim =
    prepareForDelimiting delim
    >> Option.bind(fun x ->
        let i = x.IndexOf delim
        if i >= 0 then
            Some (x.[0..i-delim.Length])
        else None
    )
let (|After|_|) = after
let (|EndsWith|_|) delim =
    prepareForDelimiting delim
    >> Option.bind(fun x ->
        if x.EndsWith delim then Some() else None
    )
let (|Before|_|) = before
let (|Token|_|) delim =
    prepareForDelimiting delim
    >> Option.bind(fun x ->
       if x.IndexOf delim = 0 then
            x |> after delim
        else None
    )
let isValueString = String.IsNullOrWhiteSpace >> not
let (|ValueString|NonValueString|) x =
    if isValueString x then ValueString x
    else NonValueString

let (|StartsWith|_|) delim =
    prepareForDelimiting delim
    >> Option.bind(fun x ->
        if x.StartsWith delim then
            Some x
        else None
    )
let (|Trim|) =
    function
    | null |"" as x -> x
    | x -> x.Trim()

open System.Text.RegularExpressions
open System.Collections.Generic

module Regex =
    let (|RMatch|_|) (pattern:string) =
        prepareForDelimiting pattern
        >> Option.bind(fun x ->
            let m = Regex.Match(x,pattern=pattern)
            if m.Success then Some m
            else None
        )

    let (|RMatchGroup|_|) (pattern:string) (i:int) =
        function
        | RMatch pattern m ->
            if m.Groups.Count < i then
                invalidOp <| sprintf "Pattern captured %i groups, but %i was requested" m.Groups.Count i
            else
                Some m.Groups.[i].Value
        | _ -> None

// advance/pluck next quoted token
let (|Quoted|_|)=
    function
    | NonValueString -> None
    | Regex.RMatch @"^\s*'([^']+)'(\s|$)" x
    | Regex.RMatch @"^\s*""([^\""]+)\""(\s|$)" x as txt ->
        let token = x.Groups.[1].Value
        let i = x.Index+x.Length
        let rem = if txt.Length > i then txt.[i..] else String.Empty
        printfn "We have a match! remainder is '%s'" rem
        Some(token,rem)
    | _ -> None

open System.Linq
// http://www.fssnip.net/hv/title/Extending-async-with-await-on-tasks
type Microsoft.FSharp.Control.AsyncBuilder with
    member x.Bind(t:System.Threading.Tasks.Task<'t>, f:'t -> Async<'r>) : Async<'r> =
        x.Bind(Async.AwaitTask t,f)
    member x.Bind(t:System.Threading.Tasks.Task, f:unit -> Async<unit>) : Async<unit> =
        x.Bind(Async.AwaitTask t,f)

    // based on https://github.com/RogueException/Discord.Net/blob/ff0fea98a65d907fbce07856f1a9ef4aebb9108b/src/Discord.Net.Core/Extensions/AsyncEnumerableExtensions.cs
    member x.Bind(e:IAsyncEnumerable<IEnumerable<'t>>,f) =
        let t = e.SelectMany(fun y -> y.ToAsyncEnumerable()).ToArray()
        x.Bind(t,f)
    member x.Bind(e:IAsyncEnumerable<IReadOnlyCollection<'t>>,f) =
        let t = e.SelectMany(fun y -> y.ToAsyncEnumerable()).ToArray()
        x.Bind(t,f)

    //member __.For(e:IAsyncEnumerable<IEnumerable<'t>>,f) = e.SelectMany(fun y -> y.ToAsyncEnumerable()).ForEachAsync(Action<_>(f))
    member __.For(e:IAsyncEnumerable<IReadOnlyCollection<'t>>,f) = e.SelectMany(fun y -> y.ToAsyncEnumerable()).ForEach(Action<_>(f))
module Async =
    open System.Collections.Generic
    open System.Linq
    let map f x =
        async{
            let! x = x
            return f x
        }
    let bind f x =
        async{
            let! x = x
            let! x = f x
            return x
        }

module SuperSerial =
    open Newtonsoft.Json
    let inline serialize (x:_) = JsonConvert.SerializeObject(value=x)
    let inline deserialize<'t> x :'t option = 
        try
            JsonConvert.DeserializeObject<'t>(x)
            |> Some
        with ex ->
            System.Diagnostics.Trace.WriteLine(sprintf "Error deserialization failed:%s" ex.Message)
            None


module Storage =
    open System
    open System.IO
    open SuperSerial

    let private folderPath = IO.Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData),"Discordantly")
    let private getKeyPath key = Path.Combine(folderPath,sprintf "%s.json" key)

    let private getKeyValue key =
        let keyPath = getKeyPath key
        if Directory.Exists folderPath && File.Exists keyPath then
            File.ReadAllText keyPath |> deserialize 
        else None
    let private setKeyValue key value =
        if not <| Directory.Exists folderPath then
            Directory.CreateDirectory folderPath |> ignore
        let keyPath = getKeyPath key
        match value with
        | None ->
            if File.Exists keyPath then
                File.Delete keyPath
        | Some value ->
            File.WriteAllText(keyPath,serialize value)

    // allows simple creation of mated pairs of 't option with get,set
    let createGetSet<'t> key:(unit -> 't option)* ('t option -> unit)=
        let getter () =
            match getKeyValue key with
            | None ->
                printfn "I couldn't find my save file at %s" <| getKeyPath key
                None
            | Some x ->
                Some x
        let setter vOpt = setKeyValue key vOpt
        getter, setter


module Reflection =
    open Microsoft.FSharp.Reflection

    // we'd like to test if it is a union, but... no clue how
    let getUnionInfo =
        function
        | UnsafeNull -> None
        | x ->
            printf "Checking a value for unionized labor"
            let t = x.GetType()
            try
                let uInfo,fv = FSharpValue.GetUnionFields(x,t)
                Some (t,uInfo,fv)
            with _ -> None
    let (|Option|_|) (caseInfo:UnionCaseInfo)=
        if caseInfo.Name = "Some" && caseInfo.DeclaringType.IsClass && caseInfo.DeclaringType.IsGenericType then
            if caseInfo.DeclaringType.GetGenericTypeDefinition() = typedefof<Option<_>> then
                Some ()
            else None
        else None

    // Print/display options as 't instead of 't option
    let fDisplay (x:'t) :string =
        let rec fIt x =
            match x with
            | UnsafeNull -> sprintf "%A" x // using whatever is built-in to display null
            | x ->
                getUnionInfo x
                |> Option.map(
                    function
                    | _,uInfo, [| |] -> uInfo.Name
                    // scrape out "Some" from Option displays
                    | _, Option, [| x |] -> fIt x
                    | t, Option, values -> values |> Array.map fIt |> sprintf "%A"
                    | t,uInfo,[| x |] ->
                        sprintf "%s(%s)" uInfo.Name <| fIt x
                    | (t,uInfo,fieldValues) ->
                        sprintf "%A %A" uInfo.Name (fieldValues |> Array.map fIt)
                ) |> Option.defaultValue (sprintf "%A" x)
                
                
        fIt x
