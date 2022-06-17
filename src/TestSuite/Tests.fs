module Tests

open Xunit
open RailwayLib
open RailwayLib.GenerateNetwork
open TestUtils
open Utils

let testFileToNetwork filename = 
    loadTestFile filename 
    |> parse 
    |||> toNetwork

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

[<Fact>]
let cycleError () = 
    Assert.Throws<NetworkError>(fun () -> testFileToNetwork "02-cycle.txt" |> ignore)
    |> fun err -> Assert.Equal("A cycle is present in the railway network.", err.Message)

[<Fact>] 
let linearDuplicate () =  
    Assert.Throws<NetworkError>(fun () -> testFileToNetwork "03-linear-duplicate.txt" |> ignore)
    |> fun err -> Assert.Equal("Port \"s10\" is not deterministic.", err.Message)

[<Fact>] 
let linearAndPointWithSameID () =  
    Assert.Throws<NetworkError>(fun () -> testFileToNetwork "04-linear-point-share-id.txt" |> ignore)
    |> fun err -> Assert.Equal("Linear segments and points must not share IDs (s20)", err.Message)

[<Fact>]
let branchWellFormed () = 
    try
        testFileToNetwork "05-branch.txt" |> ignore 
        Assert.True(true) 
    with 
        | :? NetworkError -> Assert.True(false, "Branch seen as cycle.")


[<Fact>]
let Lyngby () = 
    try
        testFileToNetwork "07-lyngby.txt" |> ignore 
        Assert.True(true) 
    with 
        | :? NetworkError -> Assert.True(false)
