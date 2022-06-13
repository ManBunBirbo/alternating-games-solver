module Tests

open System
open Xunit
open RailwayLib
open RailwayExtra
open TestUtils

    

[<Fact>]
let ``Kasting example railway.`` () =
    let facit = 
        let ls = set [ "s10"; "s12"; "s20" ]
        let ps = set [ "s11" ]
        let csUp = Map [ (L "s10", P "s11"); (L "s20", M "s11"); (S "s11", L "s12") ]
        let csDown = Map [ (L "s12", S "s11"); (P "s11", L "s10"); (M "s11", L "s20") ]
        let ss = set [ "s10"; "s20" ]
        let ts = Map [ (L "s12", L "s10"); (L "s20", L "s12") ]

        N (ls, ps, csUp, csDown, ss, ts)

    let result = loadTestFile "00-kasting.txt" |> parse 

    Assert.Equal(facit, result)