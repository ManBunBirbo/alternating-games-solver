// For more information see https://aka.ms/fsharp-console-apps
open Solver
open RailwayLib


// How to read a file: https://stackoverflow.com/a/2366649


[<EntryPoint>]
let main argv = 
    match argv with 
    | [| filename |] -> 
        __SOURCE_DIRECTORY__ + "./../../Railway Tests/" + filename
        |> System.IO.File.ReadAllText
        |> RailwayExtra.parse 
        // |> NetworkFunctions.isWellFormed
        |> printfn "%A"
    | _ -> printfn "Program initialized incorrectly: State directory of railway system file as the only input."

    0 // Terminated succesfully. 
