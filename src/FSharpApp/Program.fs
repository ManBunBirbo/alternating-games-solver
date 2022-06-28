// For more information see https://aka.ms/fsharp-console-apps
open RailwayLib
open GenerateNetwork
open RailwayLib.ToGameSolver
open ProgramUtils
open System.IO
open System.Timers

// How to read a file: https://stackoverflow.com/a/2366649

let isRailwayGameSolvable fileText = 
    try
        printfn "Parsing file..."
        
        let network = 
            parse fileText
            |> railwayParserOutputToNetwork
        
        printfn "File parsed successfully."

        let solver = toSolver 1 network

        printfn "Analyzing if game can be solved..."
        
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        let isSolvable = solver.solve 
        stopWatch.Stop()

        (if isSolvable then "" else "n't")
        |> printfn 
            "In %f ms, analyzing %i different states, it was determined that the railway network is%s solvable."
            stopWatch.Elapsed.TotalMilliseconds
            (getNoOfStates ())

    with
        | :? NetworkError as err -> 
            printfn $"NetworkError: %s{err.Message}"

[<EntryPoint>]
let main argv = 
    let cwdir = Directory.GetCurrentDirectory()

    match argv with 
    | [| filename |] ->
        let filePath = Path.Combine(cwdir, filename)
        
        try
            System.IO.File.ReadAllText filePath
            |> isRailwayGameSolvable 
        with 
            | :? FileNotFoundException -> 
                printfn $"No file with path %s{filePath}"
    | _ -> 
        printfn 
            $"Error: Please initialize the program with the path to a file \
              containing a representation of a strictly-alternating game.\n\
              The path can be absolute or relative to %s{cwdir}." 
    
    0 // Terminated successfully. 
