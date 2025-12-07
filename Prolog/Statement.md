# Statement

var declaration
~~~
var Identifier: type = expression
~~~
Here identifier is a mutable variables. Type is optional.

---
**let declaration**

~~~
let Pattern: type = expression
~~~

Here pattern consist of unbounded identifier packaged in structs/tuples/adt but is
immutable. Type is optional.

---
**def declaration, single line**

~~~
def Identifier(Pattern, Pattern, ...): type = expression
~~~

Define a function. Here the list can be empty or missing also.
---
**def declaration, multi line**
~~~
def Identifier(Pattern: type, Pattern: type, ...) -> type:

    expression1

    expresison2

    expression3

end
~~~

Define a function (Same as above) but this is a block with last expression evaluated
and all other expression should evaluate nil. types are optional
---
**if then else declaration**

~~~
if {bool expr} then {expr} else {expr} // Can precede with def or let definitions

if {bool expr} then: // Can precede with def or let definitions,
                        without else is only valid for void.
    expression1
    expresison2
    expression3
end

if {bool expr} then: // Can precede with def or let definitions,
                        without else is only valid for void.
    expression1
    expresison2
    expression3
else expression

if {bool expr} then: // Can precede with def or let definitions,
                        without else is only valid for void.
    expression1
    expresison2
    expression3
else:
    expression1
    expresison2
    expression3
end
~~~
If then else definitions. These are expressions and would proceed with let/var/def
but not def block. Notice that end is only needed when last clause is block.

---
**Pipe expressions are:**
~~~
expression1 >>   expression1 >>   expression1 >>  expression1
~~~

They can be preceded with let def or var definitions, if they are expressions. If
they are multiline then, for unit it is:
~~~
def/let/var = expression1
              >>  expression1
              >>  expression1
              >>  expression1
~~~
Key to understand that for expressions, operators can flow to next line. This is
true for other operators also.

---
**Operators**

Operators are basically classified into groups defined by interfaces (eg type
classes). They can be overridden. In parsing operators, can spill to next line.

- *BooleanOperator:* =, >= , <=
- *ArithemeticOperator:* +, -, *. /
- *LogicalOperator:* or, and, not
- *BitwiseShift:* >>, <<

---
**List Comprehensions**

- *[num1 .. num2]:* Here num2 >= num1. List will be num1, num1+1, ... , num2 -1.
- *[num1 .. num2: step]* : Here no relation between num2 and num1.
But if num1 > num2, then step should be -1

*[ Expr || clause, clause, clause, ...]*

Here clause can be:
- Bool condition
- Members e.g. X in [1..10]
- Assignment with let

---
**Let expressions**

Another type of expression is:
~~~
let Pattern: type in {expr}
~~~

---
**Function evaluation using Type**

~~~
Func # Identifer
~~~

This is an expression. The identifier here is a tuple which the
functions takes and evaluates as arguments.

---
**Module Syntax**

~~~
module Identifier

import Identifier -- Other modules
from Identifier import functions, classes, types, alias

begin

    def, let, var , functions, types, etc.
end
~~~

The definitions can be preceded by private or public. Default is public.
