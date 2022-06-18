module TestUtils 

open Utils 
open RailwayLib.GenerateNetwork

let loadTestFile fileName =
    __SOURCE_DIRECTORY__ + "/input/" + fileName 
    |> System.IO.File.ReadAllText 

let testFileToNetwork filename = 
    __SOURCE_DIRECTORY__ + "/input/" + filename 
    |> System.IO.File.ReadAllText 
    |> parse 
    |> toNetwork
