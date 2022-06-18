module Tests

open Xunit
open RailwayLib
open RailwayLib.GenerateNetwork
open TestUtils
open Utils

let testFileToNetwork filename = 
    loadTestFile filename 
    |> parse 
    |> toNetwork

[<Fact>]
let kasting00 () =
    let facit = 
        let ls = set [ "s10"; "s12"; "s20" ]
        let ps = set [ "s11" ]
        let csUp = Map [ (L "s10", P "s11"); (L "s20", M "s11"); (S "s11", L "s12") ]
        let csDown = Map [ (L "s12", S "s11"); (P "s11", L "s10"); (M "s11", L "s20") ]
        let ss = set [ "s10"; "s20" ]
        let ts = Map [ ("s12", "s10"); ("s20", "s12") ]

        N (ls, ps, csUp, csDown, ss, ts)

    let result = testFileToNetwork "00-kasting.txt"
    Assert.Equal(facit, result)

let assertError testFileName msg = 
    Assert.Throws<NetworkError>(fun () -> testFileToNetwork testFileName |> ignore)
    |> fun err -> Assert.Equal(msg, err.Message)

let assertNoError testFileName = 
    Record.Exception(fun () -> testFileToNetwork testFileName |> ignore)
    |> fun err -> Assert.Null(err)

[<Fact>]
let cycleError () = 
    assertError "02-cycle.txt" "A cycle is present in the railway network."

[<Fact>] 
let linearDuplicate () =  
    assertError "03-linear-duplicate.txt" "Port \"s10\" is not deterministic."

[<Fact>] 
let linearAndPointWithSameID () =  
    assertError 
        "04-linear-point-share-id.txt" 
        "Linear segments and points must not share IDs (s20)"

[<Fact>]
let branchWellFormed () =
    assertNoError "05-branch.txt"

[<Fact>]
let Lyngby () = 
    assertNoError "07-lyngby.txt"

[<Fact>]
let invalidLinearGoesToItself () = 
    assertError 
        "08-linear-to-itself.txt" 
        "A cycle is present in the railway network."

[<Fact>]
let invalid09PointsConnected () = 
    assertError 
        "09-connected-point.txt"
        "Points must not be connected (s11.stem -/-> s12.stem)."