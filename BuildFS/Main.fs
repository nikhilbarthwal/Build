(*

  - Common.fs: Common functions functions
  - Source.fs: Source functions & definations
  - Program.fs: Program definations & convertions- Includes all verification on Single file
  - Transform.fs: Model defination and  convert from Program, includes all errors
  - Execute.fs: Executing the model
  - Eval.fs: Helper utilities to evaluate the expr

*)


//TODO: Check intermingling of blocks and statement blocks
//TODO: Non empty list
//TODO: Is herader allowed to have list or nlist?
//TODO: Can you get Pos from all?
//TODO: Compplete all statements: LoopStatements & FuncStatements
//TODO: Add "any" with sequence
//TODO: Update namespace

module Main


[<EntryPoint>]
let main argv =
    printfn "%A" argv
    0 // return an integer exit code
