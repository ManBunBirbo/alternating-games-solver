module RailwayExtra

open FSharp.Text.Lexing 

// use inputChannel = new StreamReader(File.OpenRead tempFileName)
// let lexbuf = LexBuffer<_>.FromTextReader inputChannel

// https://stackoverflow.com/q/7928438
let parse input =
    let lexbuf = LexBuffer<char>.FromString input
    lexbuf.EndPos <- { pos_bol = 0; pos_fname = input; pos_cnum = 0; pos_lnum = 1 }
    try Parser.start Lexer.tokenize lexbuf
    with err -> 
        let pos = lexbuf.EndPos 
        let line = pos.Line
        let column = pos.Column 
        let lastToken = new System.String(lexbuf.Lexeme) 
        printfn "\nParse failed at line %d, column %d:" line column
        printfn "Unknown token: %s" lastToken
        printfn "[FsLexYacc error message] %s" err.Message
        if err.Message = "parse error" then 
            "\nFsLexYacc have returned the unhelpful message \"parse error\". \
                This is probably because the file is not setup correctly. \
                It must have the following general layout:\n\
                \nconnections = <ID>.<PORT_KIND>[, <ID>.<PORT_KIND>]\
                \nsignals = <ID>.<PORT_KIND>[, <ID>.<PORT_KIND>]\
                \ntrains = <ID> -> <ID>[, <ID> -> <ID>]\n\
                \nWhere [, ...] means a list, ie. the pattern can repeat.\n"
            |> printfn "%s" 
        exit 1

