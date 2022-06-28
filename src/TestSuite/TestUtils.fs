module TestUtils 

open ProgramUtils
open RailwayLib
open RailwayLib.GenerateNetwork
open RailwayLib.ToGameSolver
open OnTheFlySolver.Solver

let testFileToNetwork filename = 
    __SOURCE_DIRECTORY__ + "/input/" + filename 
    |> System.IO.File.ReadAllText 
    |> parse 
    |> railwayParserOutputToNetwork
    
let isTestFileSolvable testFileName = 
    testFileToNetwork testFileName   
    |> toSolver 1
    |> fun (solver: OnTheFlySolver<TrainGameState>) -> solver.solve
