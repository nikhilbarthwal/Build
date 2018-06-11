Program Top: Level definations
==============================


Top level definitions are:

 - class
 - types
 - do (actions)
 - var (constants or open variables)
 - functions
 - define (body/body less)

---------------------

## Type definitions

identifier ::= lower(lowerIdentifier) ; upper(upperIdentifier)

simpleType ::= (lowerIdentifier, maybe(list(simpleType)))
complexType ::= (simpleType, maybe(list(complexType)))
simpleTypeGeneric ::= (identifier, maybe(list(simpleType)))
complexTypeGeneric ::= (simpleTypeGeneric, maybe(list(complexTypeGeneric)))
param ::= (maybe(complexType), upperIdentifier)
params ::= list(param) // At least one
header :: = (upperIdentifier, maybe(list(upperIdentifier)), params)


## Class definitions

Class are records. Contains functions. Type's are always small except generic ones. But Name's are optional. Should have at least 1 parameter. If body is present, at least one class body entity should be present.
Classes can have body or they can be bodyless.

typeName ::= (lowerIdentifier __Name__, maybe(list(upperIdentifier) __TypesList__))
typeParam ::= (complexTypeGeneric __Type__, maybe(upperIdentifier) __Name__)
classEntity ::= classErrorDef(error def) ; classFuncDef(function def) __//TODO: Replace it with 1 do block__
geneticHeader ::= (typeName __Name__, list(classParam) __ClassParams__)
classDefinition ::= (typeName __Name__ , list(classParam) __ClassParams__, maybe(list(classBodyEntity)) __Body__)

~~~~
class name<TypeA, TypeB>(Type1 Name1, Type2 Name2, ...); // Should have atleast 1 param

class name<TypeA, TypeB>(Type1 Name1, Type2 Name2, ...)
{
    // Body consists of functions and error definitions - should contain atleast 1 entity
}
~~~~

---------------------

## Type definitions

typeEntity ::= (lowerIdentifier __Name__, list(classParam) __ClassParams__, maybe(list(classBodyEntity)) __Body__)
typeDefinition ::= (typeName __Name__ , list(typeEntity) __Members__)

~~~~
type name<T1, T2, T3>
{
    // Types should have atleast 1 class member
    name1(Type1 Name1, Type2 Name2, ...); // Should have atleast 1 param
    name2(Type1 Name1, Type2 Name2, ...)
    {
        // Body of class
    }
}
~~~~


---------------------

## Do actions

actionDefinition ::= (maybe(header) __Header__, list(list(actionStatements)) __Body__)

~~~~
do
{
    // Actions statements - block 1
|
    // Actions statements - block 2
}

do Name<T1, T2>(Type1 Name1, Type2 Name2)
{
    // Actions statements
}

~~~~


---------------------

## Var definition

varParam ::= (maybe(complexType), maybe(upperIdentifier))

varDefinition ::=

 - openVar(upperIdentifier __Name__, complexType __TypeDef__)
 - openRecord(upperIdentifier __Name__, list(typeParam) __TypeDefArgs__)
 - closedVar(identifier __Name__, maybe(complexType) __TypeDef__, expr __Value__)
 - closedRecord(identifier __Name__, list(varParam) __TypeDefArgs__ , list(list(expr)) __Values__)


~~~~
var type X;
var Name(type1, type2, type3);

var Y = value;
var type Z = value;
var Name(type1, type2 Name2, type3 Name3)
{
    { A1, A2, A3};
    { B1, B2, B3};
    { C1, C2, C3};
}

~~~~

