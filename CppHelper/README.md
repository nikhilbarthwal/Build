# README #

This is the source code generation "Builder" which would generate the MVP code in wide variety of formats

### What is this repository for? ###

* config.h has settings you need to modify
* Makefile will Generate the builder
* Repository hosted in [Bit Bucket](https://bitbucket.org/)


Element(Name , Description, Vector<sequence>)
Sequence(Description, Vector<Token>)
Token -> Symbol, White, Element, Keyword, Number, Identier

Element(Defaut text), 


HTML
Latex
Text

-> Build Items: Source (statements), Text (string list), Container (group of items), Folder
.Generate(folder name) is common
init: date & guid

Object:
string Type()
string Description()
private string description
private string type
protected bool lock
Object() lock = false
Lock()-> set lock = true
virtual string ToString()
Ptr() get the unique_ptr value for it

Number: int with all overloads, & toString. Div is div & Value with overload*

ReadOnly<T> (Get, Set) with overload * 
OP abstract-> Sum, Max, Min:Update, Value

List<T>
Map<K,V>

misc utils: Will provide Os in depend PathCombine, Create directory - os.cpp

statement
buildItem: Folder, Group, Source, Text

config.cpp ::Init: Guid, DateTime, bool build=false, bool BuildOk()StartBuild(), bool EndBuild(), bool startBuild, bool endBuild, Generate(Build(Root)): Will build=true, build, build=false, reset date & time



config.h: Windows, seperation char

Nikhil Barthwal

