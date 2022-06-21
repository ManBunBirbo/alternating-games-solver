module ProgramUtils

open FSharp.Text.Lexing

// use inputChannel = new StreamReader(File.OpenRead tempFileName)
// let lexbuf = LexBuffer<_>.FromTextReader inputChannel

// https://stackoverflow.com/q/7928438
let parse input =
    let lexbuf = LexBuffer<char>.FromString input
    lexbuf.EndPos <- { pos_bol = 0; pos_fname = input; pos_cnum = 0; pos_lnum = 1 }
    try 
        RailwayParser.start RailwayLexer.tokenize lexbuf
    with err -> 
        let pos = lexbuf.EndPos 
        let line = pos.Line
        let column = pos.Column 
        let lastToken = new System.String(lexbuf.Lexeme) 
        printfn
            $"Parse failed at line %d{line}, column %d{column}. Unknown token: %s{lastToken}.\n\
              [FsLexYacc error message] %s{err.Message}"
        if err.Message = "parse error" then 
            "Remember that the files must follow a strict layout:\n\
             connections = <ID>.<PORT_KIND>[, <ID>.<PORT_KIND>]\n\
             signals = <ID>.<PORT_KIND>[, <ID>.<PORT_KIND>]\n\
             trains = <ID> -> <ID>[, <ID> -> <ID>]\n"
            |> printfn "%s" 
        
        exit 1