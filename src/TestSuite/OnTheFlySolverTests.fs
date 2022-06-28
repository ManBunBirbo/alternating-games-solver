module OnTheFlySolverTests

open RailwayLib
open OnTheFlySolver.Player
open OnTheFlySolver.Solver
open Xunit
open TestUtils
open RailwayLib.ToGameSolver
open ProgramUtils
open RailwayLib.GenerateNetwork


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
    OnTheFlySolver<string * string>(simpleGame, (=), (("L3", "-"), One))
    |> fun solver -> Assert.True(solver.solve)

[<Fact>]
let SimpleGameNotSolvable () =
    OnTheFlySolver<string * string>(simpleGame, (=), (("L1", "+"), One))
    |> fun solver -> Assert.False(solver.solve)
    
[<Fact>]
let simpleGamePlayerTwoStarts () =
    OnTheFlySolver<string * string>(simpleGame, (=), (("L3", "-"), Two))
    |> fun solver -> Assert.False(solver.solve)

[<Fact>]
let Kasting00Solved () = Assert.True(isTestFileSolvable "00-kasting.txt")

[<Fact>]
let KastingAlt01NotSolvable () = Assert.False(isTestFileSolvable "01-kasting-altered.txt")

[<Fact>]
let Branch05Solvable () = Assert.True(isTestFileSolvable "05-branch.txt")

[<Fact>]
let KastingNoSignals06Unsolvable () = Assert.False(isTestFileSolvable "06-kasting-no-signals.txt")

[<Fact>]
let Lyngby07Solvable () = Assert.True(isTestFileSolvable "07-lyngby.txt")

[<Fact>]
let Lyngby10Solvable () = Assert.True(isTestFileSolvable "10-lyngby-more-trains.txt")

[<Fact>]
let Florence11Solvable () = Assert.True(isTestFileSolvable "11-florence.txt")