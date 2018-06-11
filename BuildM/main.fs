// Main

//open definaiotns
open Utils


let Start (F:string) =
    (*
        Tokens = Lexer.Lex(F)
        Error.Check()
        Definations = Parser.Mail(Tokens)
        Error.Check()
        Verify.Main(Definations)
        Error.Check()
        Program = Transform(Definations)
        Error.Check()
        Execute(Transform)
        Error.Check()
    *)
    printfn "Build was successfull!"

[<EntryPoint>]
let main argv =
    printfn "BUILD - Nikhil Barthwal\n"

    if (argv.Length = 0) then
        printfn "Build Usage: Build [File Name]\n\n"
    else if (argv.Length = 1) then
        Start(argv.[0])
    else
        printfn "Too many arguments, Build Usage: Build [File Name]\n\n"

    0 // return an integer exit code
