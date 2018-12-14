module PathOfExile.Domain.HtmlParsing
open Schema.Helpers
open System.Net.Http
open System.Collections.Generic

type Character = {Name:string;League:string; Class:string;Level:int}
[<RequireQualifiedAccess>]
type GetResult =
    |Success of Character[]
    |FailedDeserialize
    |FailedHttp of string
let getCharacters accountName =
    async{
    
        use client = new HttpClient()
        use hc = new FormUrlEncodedContent(
                                [
                                    KeyValuePair<_,_>("accountName", accountName)
                                ] )
        let! resp = Async.AwaitTask <| client.PostAsync("https://www.pathofexile.com/character-window/get-characters", hc)
        if resp.IsSuccessStatusCode then
            let! raw = Async.AwaitTask <| resp.Content.ReadAsStringAsync()
            let chars:Character[] option = SuperSerial.deserialize raw
            match chars with
            | Some chars ->
                return GetResult.Success chars
            | None ->
                return GetResult.FailedDeserialize
        else return GetResult.FailedHttp <| sprintf "Fail:%A" resp.StatusCode
    }
