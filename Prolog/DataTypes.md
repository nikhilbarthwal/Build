# Data Types

**Data Types:**

- Structs: Typical Data classes providing structural equality with optional attached
methods. Can inherit interfaces.
- ADT: Standard discriminated union with optional attached methods. Can inherit
interfaces.
- Tuples: Simple a collection of mixed data
- Classes: Typical Scala/F# type classes with mutable/immutable members with optional
attached methods. Can inherit interfaces.

---
**Structs**

~~~
type identifier = { Identifier: type, Identifier: type, ... }
~~~


All identifiers are unique and type is mandatory. But structs can be generic.
Generic types always start with Capital.

Anonymous structs can be declared as:

~~~
{ Identifier: type, Identifier: type, ... }
~~~

---
**Tuples**

~~~
type identifier = (type, type, type)
~~~

All identifiers are unique and type is mandatory. But structs can be generic.
Generic types always start with Capital.
Anonymous tuples can be declared as:

~~~
(type, type, ... )
~~~

---
**ADT**

~~~
type identifier = tag1(type, type, type), tag2(type, type, ...)
~~~

The above is a single line definition. The multiline definition is:
~~~
type identifier:
    tag1(type, type, type)
    tag2(type, type, ...)
~~~

You can't have tags before types. There is no anonymous defination for ADT, it has
to be referenced by name. But they can have methods to it. That can only be in
multiline defination. And those can be let or def

~~~
type identifier:
    tag1(type, type, type)
    tag2(type, type, ...)

begin
    let ...
    def ...
end
~~~
ADT's can be generics which start with capital. but they can be preceded by private
or public. Default is public!

---
**Classes**
~~~
class Identifier( Identifier: type, Identifier: type)
    new
    def
    var
    let
    end
~~~
Class will have identifier whose types are mandatory. def, var and let are there.
There will be primary constructors.

And then there are secondary constructor. Default definition is private.

---
**Type alias**

type signatures create alias

~~~
type identifier = ...
~~~
