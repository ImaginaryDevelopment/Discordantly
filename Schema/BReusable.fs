[<AutoOpen>]
module Schema.Helpers

open System

let (|UnsafeNull|_|) x =
    if isNull <| box x then Some()
    else None

let after (delim:string) (x:string) =
    match delim,x with
    | null,_|"",_ -> invalidOp "bad delimiter"
    | _, null | _, "" -> None
    | _ ->
        let i = x.IndexOf delim
        if i >= 0 then
            Some (x.[i+delim.Length..])
        else None
let before (delim:string) (x:string) =
    match delim,x with
    | null,_|"",_ -> invalidOp "bad delimiter"
    | _, null | _, "" -> None
    | _ ->
        let i = x.IndexOf delim
        if i >= 0 then
            Some (x.[0..i])
        else None
let (|After|_|) = after
let (|Before|_|) = before
let (|Token|_|) (delim:string) (x:string) =
    if x.IndexOf delim = 0 then
        x |> after delim
    else None
let isValueString = String.IsNullOrWhiteSpace >> not
let (|ValueString|NonValueString|) x =
    if isValueString x then ValueString x
    else NonValueString

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

module Async =
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

// http://www.fssnip.net/hv/title/Extending-async-with-await-on-tasks
type Microsoft.FSharp.Control.AsyncBuilder with
    member x.Bind(t:System.Threading.Tasks.Task<'t>, f:'t -> Async<'r>) : Async<'r> =
        x.Bind(Async.AwaitTask t,f)
    //member x.Bind(t:System.Threading.Tasks.Task, f: unit -> Async<unit>): Async<unit> =
    //    x.Bind(Async.AwaitTask t,f)

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
 
