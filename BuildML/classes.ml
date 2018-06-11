let debug = true or false

Build

files Files = []
lock = false
AddFile(File F) :-
     if lock then raise error
     Files += F

private function Convert (line list) -> (string list)
private get TimeStamp: string
private getGuid: string

Generate(NewRoot):
     
     lock = true
     Get a new guid
     Get date time stamp
     Create Root if not exists
     for each file:
         FileName = Root + Name
          line list = F.Print(guid , stamp)
          open that file
               for i in F.Printer(guid , stamp) // Convert (F.Print(guid , stamp))
                    if tab(true) print s
          close File


file is virtual class: mercury source or Makefile or HTML or anything

File is Virtual Class:
     Name:string
     Location: []string list
     Print (guid:string) (timeStamp:string): (line list)

text  (txt:string) (tab:bool) : bool
= let validChar c = if (c=9) then tab else ( (c>=32) & (c<127))  in List.fold (fun x s -> s & validChar x)) true (txt.to_chars)

