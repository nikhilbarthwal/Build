open System
open System.IO
open Antlr4.Runtime
open Antlr4.Runtime.Tree

type PythonSyntaxErrorListener() =
    inherit BaseErrorListener()
    
    let mutable errors = []
    
    member this.Errors = errors |> List.rev
    
    member this.HasErrors = not (List.isEmpty errors)
    
    override this.SyntaxError(output, recognizer, offendingSymbol, line, charPositionInLine, msg, e) =
        let errorMsg = sprintf "Line %d:%d - %s" line charPositionInLine msg
        errors <- errorMsg :: errors

type ParseResult = {
    Success: bool
    Errors: string list
    ParseTree: string option
    TokenCount: int
}

let tokenize (input: string) =
    let inputStream = AntlrInputStream(input)
    let lexer = Python3Lexer(inputStream)
    let tokens = CommonTokenStream(lexer)
    tokens.Fill()
    tokens.GetTokens() |> Seq.toList

let parse (input: string) : ParseResult =
    try
        let inputStream = AntlrInputStream(input)
        let lexer = Python3Lexer(inputStream)
        let tokens = CommonTokenStream(lexer)
        let parser = Python3Parser(tokens)
        
        let errorListener = PythonSyntaxErrorListener()
        parser.RemoveErrorListeners()
        parser.AddErrorListener(errorListener)
        
        let tree = parser.file_input()
        let tokenCount = tokens.GetTokens() |> Seq.length
        
        if errorListener.HasErrors then
            {
                Success = false
                Errors = errorListener.Errors
                ParseTree = None
                TokenCount = tokenCount
            }
        else
            {
                Success = true
                Errors = []
                ParseTree = Some (tree.ToStringTree(parser))
                TokenCount = tokenCount
            }
    with
    | ex ->
        {
            Success = false
            Errors = [sprintf "Parser error: %s" ex.Message]
            ParseTree = None
            TokenCount = 0
        }

let printTokens (tokens: IToken list) =
    printfn "\n=== Tokens ===" 
    for token in tokens do
        if token.Type <> Python3Lexer.Eof then
            let typeName = Python3Lexer.DefaultVocabulary.GetSymbolicName(token.Type)
            printfn "  [%3d] %-20s: '%s'" token.Type typeName (token.Text.Replace("\n", "\\n").Replace("\r", "\\r"))
    printfn ""

let printUsage () =
    printfn "Python Parser - An F# application using ANTLR to parse Python code"
    printfn ""
    printfn "Usage:"
    printfn "  PythonParser <file.py>          Parse a Python file"
    printfn "  PythonParser --code \"<code>\"    Parse Python code string"
    printfn "  PythonParser --tokens <file.py> Show tokens for a Python file"
    printfn "  PythonParser --help             Show this help message"
    printfn ""
    printfn "Examples:"
    printfn "  PythonParser example.py"
    printfn "  PythonParser --code \"print('Hello, World!')\""
    printfn "  PythonParser --tokens example.py"

let parseFile (filePath: string) =
    if not (File.Exists(filePath)) then
        printfn "Error: File not found: %s" filePath
        1
    else
        let code = File.ReadAllText(filePath)
        let result = parse code
        
        if result.Success then
            printfn "Successfully parsed: %s" filePath
            printfn "Token count: %d" result.TokenCount
            printfn "\nParse tree:"
            match result.ParseTree with
            | Some tree -> 
                if tree.Length > 500 then
                    printfn "%s..." (tree.Substring(0, 500))
                else
                    printfn "%s" tree
            | None -> ()
            0
        else
            printfn "Parse errors in: %s" filePath
            for error in result.Errors do
                printfn "  %s" error
            1

let parseCode (code: string) =
    let codeWithNewline = if code.EndsWith("\n") then code else code + "\n"
    let result = parse codeWithNewline
    
    if result.Success then
        printfn "Successfully parsed code"
        printfn "Token count: %d" result.TokenCount
        printfn "\nParse tree:"
        match result.ParseTree with
        | Some tree -> printfn "%s" tree
        | None -> ()
        0
    else
        printfn "Parse errors:"
        for error in result.Errors do
            printfn "  %s" error
        1

let showTokens (filePath: string) =
    if not (File.Exists(filePath)) then
        printfn "Error: File not found: %s" filePath
        1
    else
        let code = File.ReadAllText(filePath)
        let tokens = tokenize code
        printTokens tokens
        0

[<EntryPoint>]
let main args =
    match args |> Array.toList with
    | [] ->
        printUsage()
        0
    | ["--help"] | ["-h"] ->
        printUsage()
        0
    | ["--code"; code] ->
        parseCode code
    | ["--tokens"; filePath] ->
        showTokens filePath
    | [filePath] ->
        parseFile filePath
    | _ ->
        printfn "Invalid arguments. Use --help for usage information."
        1
