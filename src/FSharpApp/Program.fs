// For more information see https://aka.ms/fsharp-console-apps
open RailwayLib
open GenerateNetwork
open RailwayLib.GameFunctions
open Utils
open System.IO
open OnTheFlySolver

// How to read a file: https://stackoverflow.com/a/2366649

let isRailwayGameSolvable fileText = 
    try 
        parse fileText
        |> toNetwork
        |> toSolver 
        |> fun solver -> solver.solve    
        |> fun isSolvable -> if isSolvable then "" else "not "
        |> printfn "The railway network is %ssolvable."
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
    
    0 // Terminated succesfully. 
