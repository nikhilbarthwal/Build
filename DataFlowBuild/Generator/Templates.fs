// Templates and File Data

module Templates

open Utils

(*
    LineNull
    LineFill of char * string * string //char, prefix, suffix
    LineCenter of string * string * string * char //main, prefix, suffix, padding
    LineStr of string * string * string //string, prefix, suffix
    Line2Str of string * string * string * string //main1, main2, prefix, suffix
*)

let Spacing = [ LineNull; LineNull ]

let FileHeader ((moduleName, fileName, description) : string * string * string) =
    List.append [
            LineFill('*', "// ", "");
            LineNull;
            LineCenter("B U I L D E R", "// " , "", ' ');
            LineCenter("by", "// ", "", ' ');
            LineCenter("Nikhil Barthwal", "// ", "", ' ');
            LineNull;
            LineStr("Build Id: " + Utils.BuildId, "// ", "");
            LineStr("Build Time: " + Utils.TimeStamp, "// ", "");
            LineNull;
            LineFill('*', "// ", "");
            LineCenter(fileName + ": " + description, "// ", "", ' ');
            LineFill('*', "// ", "");
            LineNull;
            LineStr("module " + moduleName, "", "")
         ] Spacing
 
let TypeHeader (n:string) (d:string) = [ LineFill('_', "// ", "") ; LineCenter(n + ": " + d, "// ", "", ' ') ; LineNull ]


