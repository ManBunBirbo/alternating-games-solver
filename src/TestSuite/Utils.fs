module TestUtils 

let loadTestFile fileName =
    __SOURCE_DIRECTORY__ + "/input/" + fileName 
    |> System.IO.File.ReadAllText 