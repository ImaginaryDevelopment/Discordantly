module Schema.Helpers
open System
open System.Collections.Generic
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

module StringPatterns =
    open Schema.BReusable

    let (|Trim|) =
        function
        | NonValueString x -> x
        | ValueString x -> x.Trim()

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
        | NonValueString _ -> None
        | RMatch @"^\s*'([^']+)'(\s|$)" x
        | RMatch @"^\s*""([^\""]+)\""(\s|$)" x as txt ->
            let token = x.Groups.[1].Value
            let i = x.Index+x.Length
            let rem = if txt.Length > i then txt.[i..] else String.Empty
            printfn "We have a match! remainder is '%s'" rem
            Some(token,rem)
        | _ -> None


module Reflection =
    open Schema.BReusable
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
                    | _, Option, values -> values |> Array.map fIt |> sprintf "%A"
                    | _,uInfo,[| x |] ->
                        sprintf "%s(%s)" uInfo.Name <| fIt x
                    | (_,uInfo,fieldValues) ->
                        sprintf "%A %A" uInfo.Name (fieldValues |> Array.map fIt)
                ) |> Option.defaultValue (sprintf "%A" x)
        fIt x
