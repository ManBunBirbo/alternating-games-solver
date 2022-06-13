module OnTheFlySolverTests

open OnTheFlySolver
open Xunit
open TestUtils
open RailwayExtra
open RailwayLib.GameFunctions


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
let ``On-the-fly solver solves simple, legal game`` () =
    let solver =
        new OnTheFlySolver<string * string>(simpleGame, (=), (("L3", "-"), One))

    Assert.True(solver.solve)

[<Fact>]
let ``On-the-fly solver cannot solve simple, illegal game`` () =
    let solver =
        new OnTheFlySolver<string * string>(simpleGame, (=), (("L1", "+"), One))

    Assert.False(solver.solve)

[<Fact>]
let comeOn () = 
    let kasting = loadTestFile "00-kasting.txt"
    let solver = parse kasting |> toSolver

    Assert.True(solver.solve)

[<Fact>]
let please () = 
    let kastingAlt = loadTestFile "01-kasting-altered.txt"
    let solver = parse kastingAlt |> toSolver 

    Assert.False(solver.solve)

