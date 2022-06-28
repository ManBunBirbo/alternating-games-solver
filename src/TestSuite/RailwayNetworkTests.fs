module RailwayNetwork

open Xunit
open RailwayLib
open RailwayLib.GenerateNetwork
open TestUtils
open ProgramUtils

[<Fact>]
let kasting00 () =
    let expected = 
        let ls = set [ "s10"; "s12"; "s20" ]
        let ps = set [ "s11" ]
        let csUp = Map [ (L "s10", P "s11"); (L "s20", M "s11"); (S "s11", L "s12") ]
        let csDown = Map [ (L "s12", S "s11"); (P "s11", L "s10"); (M "s11", L "s20") ]
        let ss = set [ "s10"; "s20" ]
        let ts = Map [ ("s12", "s10"); ("s20", "s12") ]

        N (ls, ps, csUp, csDown, ss, ts)

    let result = testFileToNetwork "00-kasting.txt"
    Assert.Equal(expected, result)

let assertRailwayFileError testFileName msg = 
    Assert.Throws<NetworkError>(fun () -> testFileToNetwork testFileName |> ignore)
    |> fun err -> Assert.Equal(msg, err.Message)

let assertNoError testFileName = 
    Record.Exception(fun () -> testFileToNetwork testFileName |> ignore)
    |> Assert.Null

[<Fact>]
let cycleError () = 
    assertRailwayFileError "02-cycle.txt" "A cycle is present in the railway network."

[<Fact>] 
let linearDuplicate () =  
    assertRailwayFileError "03-linear-duplicate.txt" "Port \"s10\" is not deterministic."

[<Fact>] 
let linearAndPointWithSameID () =  
    assertRailwayFileError 
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
    assertRailwayFileError 
        "08-linear-to-itself.txt" 
        "A cycle is present in the railway network."

[<Fact>]
let invalid09PointsConnected () = 
    assertRailwayFileError 
        "09-connected-point.txt"
        "Points must not be connected (s11.stem -/-> s12.stem)."
        
[<Fact>]
let trainOnPoint () =
    assertRailwayFileError
        "12-train-on-point.txt"
        "Train starts or ends on s11 which is not a linear segment."
        
[<Fact>]
let pointMissingConnection () =
    assertRailwayFileError
        "13-point-missing-connection.txt"
        "Point(s) with ID(s) s11 are invalid."
        
[<Fact>]
let sharedDestination () =
    assertRailwayFileError
        "14-shared-destination.txt"
        "Two trains start or end at s10 or s12"
        