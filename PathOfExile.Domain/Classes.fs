namespace PathOfExile.Domain

open Schema.Helpers


module Classes =
    type MarauderType =
        |Berserker
        |Juggernaut
        |Chieftain

    type DuelistType =
        | Slayer
        | Gladiator
        | Champion
    type WitchType =
        | Occulist
        | Elementalist

    type ChClass =
        |Marauder of MarauderType option
        |Witch of  WitchType option
        |Duelist of DuelistType option
        with member x.ToDump() = sprintf "%A" x

//open Classes

//module Parsing =
//    let (|Class|) =
//        function
//                //AAAABAECA
//        |Token "AAAABAE" x -> 
//            match x with
//            | Token "A" x ->
//                Marauder None, x
//            | Token "B" x ->
//                Marauder (Some Juggernaut),x
//            | Token "C" x ->
//                Marauder (Some Berserker), x
//        | Token "AAAABAQ" x ->
//            match x with
//            | Token "A" x->
//                Duelist None,x
//            | Token "B" x ->
//                Duelist (Some Slayer), x
//            | Token "C" x ->
//                Duelist (Some Gladiator), x
//            | Token "D" x ->
//                Duelist (Some Champion), x
//        >> function
//            |c, "AQ==" -> c, None
//            |c, x -> c, Some x
