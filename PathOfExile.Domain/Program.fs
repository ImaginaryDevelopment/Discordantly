open Schema.BReusable.StringPatterns
open System

[<EntryPoint>]
let main argv =
    printfn "%A" argv
    match List.ofArray argv with
    | StringEqualsI "pob" :: pobLink :: [] ->
        PathOfExile.Domain.TreeParsing.PathOfBuildingParsing.Impl.parseCode pobLink
        |> Map.toSeq
        |> Seq.iter(printfn "%A")
        Console.ReadLine() |> ignore
    | _ -> Console.Error.WriteLine "Unknown command"
    0




