module Schema.Helpers
open System
open System.Collections.Generic
open System.Linq

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

[<AutoOpen>]
module Utils =
    open System.IO
    let dump titleOpt (f:_ -> obj) (x:'t) =
            let vsCodePath = @"C:\Program Files (x86)\Microsoft VS Code\bin\code.cmd"
            if File.Exists vsCodePath then
                // write the xml out to temp for inspection
                let tmp =
                    titleOpt
                    |> function
                        |Some (title:string) ->
                            if Path.IsPathRooted title then
                                title
                            else Path.Combine(Path.GetTempPath(),sprintf "%s.json" title)
                        | None -> 
                            let fp = Path.GetTempFileName()
                            // we want to control the extension
                            Path.Combine(Path.GetDirectoryName fp, sprintf "%s.json" <| Path.GetFileNameWithoutExtension fp)
                match f x with
                | null -> "null"
                | :? string as txt -> txt
                | x -> Reflection.fDisplay x
                |> fun text ->
                    File.WriteAllText(tmp,text)
                // without full path it seemingly ignored the command
                // might be something else in path that code.cmd pulls in
                System.Diagnostics.Process.Start(vsCodePath,tmp)
                |> ignore

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
    let  inline serializeXmlNodePretty (x:System.Xml.XmlNode) = JsonConvert.SerializeXmlNode(x, Formatting.Indented)


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
    open System.Text.RegularExpressions

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
    let (|RGroup|) (i:int) (m:Match) = m.Groups.[i].Value

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


module Xml =
    open System.Xml.Linq

    let toXName = XName.op_Implicit


    let getAttribValue name (xe:XElement) : string option =
        toXName name
        |> xe.Attribute
        |> Option.ofObj
        |> Option.map(fun x -> x.Value)

    let getAttribValueOrNull name (xe:XElement) : string =
        getAttribValue name xe
        |> Option.getOrDefault null

    let getElement name (xe:XElement) = xe.Element(toXName name) |> Option.ofObj
    let getElements name (xe:XElement) = xe.Elements(toXName name)
    let getAllElements (xe:XElement) = xe.Elements()
    let getElementsByName name (xe:XElement) = xe.Elements(toXName name)

    open System.Xml

    // does this kill the root node? - // https://stackoverflow.com/questions/24743916/how-to-convert-xmlnode-into-xelement
    // concerns: https://stackoverflow.com/questions/24743916/how-to-convert-xmlnode-into-xelement
    // if there are issues try also my F# extensions.linq
    let toXmlNode (xe:XElement) =

        let xmlDoc = XmlDocument()
        (
            use xmlReader = xe.CreateReader()
            xmlDoc.Load(xmlReader)
        )
        xmlDoc.FirstChild

    let fromXmlNode (xn:XmlNode) =
        xn.CreateNavigator().ReadSubtree()
        |> XElement.Load

    let serializeXElementPretty (xe:XElement)=
        toXmlNode xe
        |> SuperSerial.serializeXmlNodePretty

    let dumpXE titleOpt (xe:XElement) = dump titleOpt (serializeXElementPretty>>box) xe
