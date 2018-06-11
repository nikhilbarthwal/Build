// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

module program

open System
open System.Diagnostics


type Point = { X : float; y: float; z: float; }
type func = Point -> Point

type Build() =
    static let id = (System.Guid.NewGuid()).ToString().ToUpper()
    static let timeStamp = System.DateTime.Now.ToString("F")

    static do
        System.Diagnostics.Debug.Listeners.Add(new TextWriterTraceListener(Console.Out)) |> ignore
        System.Diagnostics.Debug.AutoFlush <- true
        System.Diagnostics.Debug.WriteLine("Welcome!")

    static member Id() = id
    static member Timestamp() = timeStamp
    static member Log(s:string) = System.Diagnostics.Debug.WriteLine(s)



[<EntryPoint>]
let main argv = 
    
    let m = { X = 1.0; y = 1.0; z = 0.0; }
    printfn "%f" m.X
    let g = Build.Id()
    Build.Log(g)
    Build.Log("Nikhil")
    let A = [1;2;3;4]
    //dgsdg
    0 // return an integer exit code
