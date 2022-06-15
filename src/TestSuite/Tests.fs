module Tests

open System
open Xunit
open RailwayLib
open RailwayExtra
open RailwayLib.NetworkFunctions
open TestUtils

let testFileWellFormed filename = 
    loadTestFile filename 
    |> parse 
    |> isWellFormed

[<Fact>]
let kasting00WellFormed () =
    let facit = 
        let ls = set [ "s10"; "s12"; "s20" ]
        let ps = set [ "s11" ]
        let csUp = Map [ (L "s10", P "s11"); (L "s20", M "s11"); (S "s11", L "s12") ]
        let csDown = Map [ (L "s12", S "s11"); (P "s11", L "s10"); (M "s11", L "s20") ]
        let ss = set [ "s10"; "s20" ]
        let ts = Map [ ("s12", "s10"); ("s20", "s12") ]

        N (ls, ps, csUp, csDown, ss, ts)

    let result = loadTestFile "00-kasting.txt" |> parse 

    Assert.True(result |> isWellFormed)
    Assert.Equal(facit, result)

[<Fact>]
let cycle02NotWellFormed () =  
    Assert.False(testFileWellFormed "02-cycle.txt")

[<Fact>] 
let linearDuplicate () =     
    Assert.False(testFileWellFormed "03-linear-duplicate.txt")

[<Fact>] 
let linearAndPointWithSameID () =  
    Assert.False(testFileWellFormed "04-linear-point-share-id.txt")

[<Fact>]
let branchWellFormed () = 
    loadTestFile "05-branch.txt" |> parse |> printfn "%A"
    Assert.True(testFileWellFormed "05-branch.txt")