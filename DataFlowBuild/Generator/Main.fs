// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

module Generator

open System.IO
open System.Diagnostics
open Utils

[<EntryPoint>]
let main argv = 
    LogMessage " *** Generator started *** "
    LogMessage (sprintf "Build Id : %s " Utils.BuildId)
    LogMessage (sprintf "Time stamp : %s " Utils.TimeStamp)
    if not (Directory.Exists Target) then
        (Directory.CreateDirectory Utils.Target |> ignore)
        LogMessage (sprintf "Creating Target directory : %s " Utils.Target)
    else
        LogMessage (sprintf "Target directory exists: %s " Utils.Target)
    LogMessage "Generating definations ..."
    let max = Publish [Definations.PrintDefination]
    LogMessage (sprintf "Line Max length: %i " max)
    0


