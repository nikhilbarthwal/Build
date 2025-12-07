# Streams

#### Definition:

~~~
interface Stream[T]:
    Current: T
    MoveNext: Result[bool]

[<TailRec>]
let rec Search[T] stream[T] (f: T -> Result[Maybe[T]]): Result[bool] =
    match stream.MoveNext() with
    | Ok(true) -> match (f stream.Current) ->
                  | Ok(false) -> Search stream f
                  | x -> x
    | x -> x

~~~

---
#### ListStream:

~~~
class ListStream[T](List[Maybe[T]]): Stream[T]
~~~

#### CombineStream:

~~~
class StreamOfStream[T](Stream[Stream[T]]): Stream[T]
~~~

---

#### FilterStream:

~~~
class FilteredStream[T](stream[T], T -> Result[bool]): Stream[T]

    let filter(vars): Result[bool] =
        b.Eval(vars).HandleResult(fun b -> if b then update Curent ; Ok(B))

    member MoveNext(): Result[bool] = Result.Search stream filter
~~~

---

#### StreamOfStream:

~~~
class TreeStream[T](z: T, l: Array[T -> [Result[Maybe[Stream[T]]]]]): Stream[T]

    assert l.Length > 0
    let max = Blocks.Length
    let streams = Queue<stream>()

    let init (v: Vars) (n: int): Result[bool] =
        assert Stream.Count = n
        if n = max then Ok[true] else
            match l[n].Handle(apply <| initHandle n)

    let initHandle n stream: Result[Maybe[bool]] =
        Streams.push(stream)
        Result.Search stream <| init (n + 1)

    let next n: Result[Maybe[bool]] =
        assert Stream.Count > 0
        asser n < max
        if n < 0 then Ok(false) else
            assert Stream.Count = n + 1
            match streams.Top().Next().Handle(nextHandle n)

    let nextHandle n vars: Result[Maybe[bool]] =
        if b then (init streams.Top().Current <| n + 1) else
            streams.Pop() ; (next <| n - 1)


    member Curent: T =
        assert streams.Count = max
        return stream.Top().Current

    member MoveNext(): Result[bool] =
        if streams.Count = 0 then (init z 0) else (next <| max - 1)
~~~

---

#### PredDefination:

~~~
function Stream n [l: List[Streams[Vars]]: Result[List[Streams[Vars]] =
    match p[n].GetStream(vars) with
    | Error(e) -> Error(e)
    | Ok(z) -> Stream (n+1) <| match (b) with Yes(stream) -> stream::l | No -> l

function Get l = match l with [] -> Ok(No) | _ -> StreamsOfStreams(ListStream(l))
    let s: Stream = PredDefinationStream(stream, PredInstances) in Ok(Yes(s))

function PredDefination::GetStream(VarMap) -> Result[Maybe[Stream]] =
    (Stream 0 []).HandleResult(Get)
~~~

----

#### PredBlock

~~~
function Get(stream): Maybe[Stream] = Yes PredBlockStream(stream, b)

function PredBlock::GetStream(VarMap) -> Result[Maybe[Stream]] =
    p.GetStream(VarMap).HandleResult(Apply Get)
~~~

---

#### PredInstance

~~~
function PredInstanceStream vars b: Maybe[Stream[Vars]] =
    if b then (Yes <| TreeStream(vars, Blocks.Map(_.GetStream[Vars])) else No

function Eval (v: vars): Result[Maybe[Stream]] =
    IntialBlock.Eval(v).HandleResult(PredInstanceStreamStream b |> Ok)

// PredInstance(PredParams, InitialBlock, Blocks: List[PredBlock])
function PredInstance::GetStream(varMap) -> Result[Maybe[Stream] =
    match p.Construct(varMap).HandleResult(Apply Eval)
~~~
