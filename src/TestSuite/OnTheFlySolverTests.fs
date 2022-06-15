module OnTheFlySolverTests

open OnTheFlySolver
open Xunit
open TestUtils
open RailwayExtra
open RailwayLib.GameFunctions
open RailwayLib.NetworkFunctions


// |----| |-
//   L1     \
//           \
// |----| |----| |----|
//   L2     P1     L3
//
// Representation: A tuple (double) of train position and point connection (+ or -):
//      (t, p) where t in {L1, L2, L3, crash}, p in {+, -}.
//
// Train can only drive in one direction, here right to left.
let simpleGame =
    let edges1 (pos, _) = set [ (pos, "+"); (pos, "-") ]

    let edges2 =
        function
        | ("L3", p) when p = "+" -> set [ ("L2", p) ]
        | ("L3", p) when p = "-" -> set [ ("L1", p) ]
        | ("L1", p) -> set [ ("crash", p) ]
        | ("L2", p) -> set [ ("crash", p) ]
        | _ -> set []

    let isGoalState (pos, _) = pos = "L2"

    edges1, edges2, isGoalState

[<Fact>]
let simpleGameSolved () =
    let solver =
        new OnTheFlySolver<string * string>(simpleGame, (=), (("L3", "-"), One))

    Assert.True(solver.solve)

[<Fact>]
let SimpleGameNotSolvable () =
    let solver =
        new OnTheFlySolver<string * string>(simpleGame, (=), (("L1", "+"), One))

    Assert.False(solver.solve)

let isTestFileSolvable testFileName = 
    let network = 
        loadTestFile testFileName 
        |> parse 

    Assert.True(isWellFormed network)

    toSolver network
    |> fun solver -> solver.solve

[<Fact>]
let Kasting00Solved () = 
    Assert.True(isTestFileSolvable "00-kasting.txt")

[<Fact>]
let KastingAlt01NotSolvable () = 
    Assert.False(isTestFileSolvable "01-kasting-altered.txt")

[<Fact>]
let Branch05Solvable () = 
    Assert.True(isTestFileSolvable "05-branch.txt")

[<Fact>]
let KastingNoSignals06Unsolvable () = 
    Assert.False(isTestFileSolvable "06-kasting-no-signals.txt")
