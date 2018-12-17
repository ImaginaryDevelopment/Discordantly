<Query Kind="FSharpProgram">
  <Reference>&lt;RuntimeDirectory&gt;\System.Net.Http.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <NuGetReference>HtmlAgilityPack</NuGetReference>
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Newtonsoft.Json</Namespace>
  <Namespace>System.Net</Namespace>
  <Namespace>System.Net.Http</Namespace>
</Query>

let delimit (d:string) (s:string seq) = String.Join(d,values=s)
let replace (s:string) (r:string) (t:string) = t.Replace(s,r)

let (|ParseInt|_|) (x:string) =
    match Int32.TryParse x with
    | true,i -> Some i
    | _ -> None
let (|ValueString|NonValueString|) (x:string) = 
    if String.IsNullOrWhiteSpace(x) then
        NonValueString
    else ValueString (x.Trim())

let (|RMatch|_|) p x =
    let m = Regex.Match(x,p)
    if m.Success then
        Some m
    else None
    
module SuperSerial =
    let serialize : obj -> string = JsonConvert.SerializeObject
    let inline deserialize (x:string) = JsonConvert.DeserializeObject x
let text = 
    let f () =
        async{
        
            use client = new HttpClient()
            use hc = new FormUrlEncodedContent(
                                    [
                                        KeyValuePair<_,_>("accountName", "DevelopersDevelopersDevelopers")
                                    ] )
            let! resp = Async.AwaitTask <| client.PostAsync("https://www.pathofexile.com/character-window/get-characters", hc)
            if resp.IsSuccessStatusCode then
                let! raw = Async.AwaitTask <| resp.Content.ReadAsStringAsync()
                return raw
            else return sprintf "Fail:%A" resp.StatusCode
            
        }

//    Util.Cache(f, "html")
    f()
let parsed = 
    text
    |> Async.RunSynchronously

parsed
//|> fun x -> System.Windows.Forms.Clipboard.SetText x
|> Dump
|> ignore