-----------------------------------------------------------------------------------------------
# Extended Pseudo-Code (PXC)

.....



-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## Nybbles {#nyb}

A _nybble_ is four bits. Each bit is either 0 or 1 in value. There are therefore sixteen 
different values that a nybble can have. 

The bits of a nybble can be represented as an array B, where 

 - B3 is the highest bit, bit 3, and is written on leftmost side
 
 - B0 is the lowest bit, bit 0, and is written on the rightmost side

The value of a nybble is given by Sum(Bi^2i) where i varies from 0 to 3. Bi means the i-th 
element of the array B, and x^y means 'x to the power of y'. 

For example, for the nybble:

    0001
    
the lowest bit, bit 0, has value 1, and the whole nybble has value 1, 

whereas for:

    1000
    
the highest bit, bit 3, has value 1, and the whole nybble has value 8. 

The nybble 0000 has value 0, and the nybble 1111 has value 15. 




-----------------------------------------------------------------------------------------------
## Micro Language

The PXC _micro language_ is a very simple way of writing what a PXC module contains without 
simply writing a sequence of [nybble](#nyb) values. 

The clumsy acronym _PXCML_, the PXC Micro-Language, can be pronounced 'pitch-mul'. 

.....








-----------------------------------------------------------------------------------------------
## Basic Encoding

A PXC file is a [universal file header](?????) file .....

.....

The overall format of the file is: 

 1. the file header; 
 
 2. one PXC module section. 
 
 
### PXC Module Section

The identification code for a PXC module section is ?????

The payload of the section is an _encoded PXC stream_ .....

An encoded PXC stream is a sequence of [nybbles](#nyb).

Each quad of the payload contains 16 nybbles, in big-endian format. The first nybble is in the
upper nybble of the most significant byte. The most significant byte is first (the one at
offset 0 from the beginning) of the quad. The nybbles immediately following those in a quad are
those in the immediately following quad. 



-----------------------------------------------------------------------------------------------
## Encoding Stream

The overall structure of a PXC encoding stream is:

 1. 
 
 2. 
 
 3. the [symbol table](#symtab); 
 
 4. the [export table](#exptab); 
 
 5. 
 
 6. 
 
 7. the [definition stream](#defstr). 



-----------------------------------------------------------------------------------------------
## Definition Stream {#defstr}

.....

..... [morphid](#mor) .....



-----------------------------------------------------------------------------------------------
## Morphids {#mor}

Within the [definition stream](#defstr), each sequence of nybbles that encodes something is 
preceded by a single nybble, called the _morphid_, which identifies the kind of thing that 
follows. 

| Code  | Followed By                  |
| ----- | ---------------------------- | 
|     0 | [Any](#any)                  |
|     1 | [Null Value](#null)          |
|     2 | [Mini-Integer](#mint)        |
|     2 | [Small Integer](#smint)      |
|     2 | [General Integer](#gint)     |
|     3 | [General Number](#gnum)      |
|     | Address Token                |
|     | Tuple |
|     | Array |
|     | [Compact Nybble Array](#cna)   |
|     |  |
|     | Invoke                      |
|     |  |
|     |  |
|     |  |
|     |  |
|     |  |







.....


-----------------------------------------------------------------------------------------------
## Any {#any}









-----------------------------------------------------------------------------------------------
## Mini-Integer {#mint}

A _mini-integer_ is an encoding of an integer between the values of 0 and 65535. The encoding 
takes up  five nybbles.: one for the [morphid](#mor), followed by four holding the value (the 
first of these nybbles is the most significant). 

.....




-----------------------------------------------------------------------------------------------
## Small Integer {#smint}




-----------------------------------------------------------------------------------------------
## General Integer {#gint}

A _general integer_ value is a fixed integer value (a whole number). 

In PXCML, any static expression that evaluates to an integer but whose value cannot be encoded
 as a [small integer](#smint) or a [mini-integer](#mint) is encoded as a general integer. 

An encoded general integer value is encoded as the following sequence of nybbles:

 1. One nybble, which encodes the width (the length of the length), W, of the value; 
    
 2. W nybbles, encoding the length of the value, as an unsigned binary value, L; 
 
 3. L nybbles, encoding the value, as an unsigned binary value, V; 

The value is encoded in nybble-oriented little-endian order. This means that the least 
significant nybble comes first. The nybbles are considered to be an array N indexed from 0 
(the first, least significant) up to L-1. The value V is Sum(Ni^16i). 

The width W is between 0 and 15. If the width is zero, then the value of the numeric value is 
zero. 

For example, the following sequence of nybbles:

    0101 (a)
    0001 (b)
    0011 (c)
    0011 (d)
    0110 (d)
    0100 (e)
    0101 (f)
    
Represents the value 1605, analysed as follows:

 (a) the morphid for a general integer, 5
 (b) W = 1
 (c) L = 3
 (d) 6
 (e) 4
 (f) 5

V = 6x256 + 4x16 + 5 = 1605

As another example:

    0101 (a)
    0001 (b)
    0010 (c)
    0110 (d)
    0101 (e)

Represents the value 99, analysed as follows:

 (a) the morphid for a general integer, 5
 (b) W = 1
 (c) L = 2
 (d) 6
 (e) 5

V = 6x16 + 5 = 99

.....






-----------------------------------------------------------------------------------------------
## General Number {#gnum}

An _encoded_ numeric value is a fixed integer value (a whole number) or a rational value (in 
essence, one integer divided by another) encoded directly into the PXC. 

It comprises two integer values, a _magnitude_ M and a _scale_ S. Its value, V, is M/S (M 
divided by S). 

An encoded numeric value is encoded as a sequence of nybbles:

 1. One nybble, which encodes the width (the length of the length), W, of both the magnitude 
    and the scale; 
    
 2. W nybbles, encoding the length of the scale, as an unsigned binary value, Ls; 
 
 3. W nybbles, encoding the length of the magnitude, as an unsigned binary value, Lm; 
 
 4. Ls nybbles, encoding the scale, as an unsigned binary value, S; 
 
 5. Lm nybbles, encoding the magnitude, as an unsigned binary value, M; 

The scale and the magnitude are encoded in nybble-oriented little-endian order. This means 
that the least significant nybble comes first. The nybbles are considered to be an array N 
indexed from 0 (the first, least significant) up to L-1, where L is Ls for the scale or Lm for 
the magnitude. The value (of either the scale or the magnitude) is Sum(Ni^16i). 

The width is between 0 and 15. If the width is zero, then the value of the numeric value is 
zero. 

The value, V, of the encoded immediate numeric value is as follows:

 - if Ls = 0, V = M (an integer)

 - if Ls > 0, v = M/S (a fractional value)

For example, the following sequence of nybbles:

    0001
    0011
    0011
    0101
    0000
    0100
    0110
    0100
    0101
    
Represents the value 1.25, analysed as follows:

 - W = 1
 - Ls = 3
 - Lm = 3
 - S = 5x256 + 0x16 + 4 = 1284
 - M = 6x256 + 4x16 + 5 = 1605
 - V = 1605/1284 = 1.25

Of course 1.25 would, in practice, be represented as:

    0001
    0001
    0001
    0100
    0101

Which would be analysed thus:

 - W = 1
 - Ls = 1
 - Lm = 1
 - S = 4
 - M = 5
 - V = 5/4 = 1.25

As another example:

   0001
   0000
   0010
   0110
   0101

Represents the value 99, analysed as follows:

 - W = 1
 - Ls = 0
 - Lm = 2
 - S = 1 (implicitly)
 - M = 6x16 + 5 = 99
 - V = 99

.....






-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 








-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## Expressions {#expr}

An _expression_ is any of: 

 * a [literal](#lit) or [formula](#form); 
 
 * an [invocation](#inv)
 
 * a [tuple](#tup);

 * an [array](#arr);

 * [](#);

 * [](#);

 * [](#);

 * a [null](#null);

 * a [compact nybble array](#cna);

 * a [definition array](#defarr);

 * [](#);

 * [](#);


.....


-----------------------------------------------------------------------------------------------
## Literals {#lit}

.....





### Character

A character literal is written in PXCML as the character between `'` single quotes ....

The character is encoded exactly the same as a [general number](#gnum) that is an integer and 
whose value is the [Unicode][2] code point of the character. 

Be aware that some characters can be, or can only be, represented by two or more code points. 
In this case, you cannot use a character literal, you must use a string literal instead. 

The single quote character itself .....


### String

A string literal is written in PXCML as a sequence of (zero or more) characters between `"` 
double quotes. 

The string is encoded as an array of general numbers (that are integers). The value of each 
element of the array represents a [Unicode][2] code point. 

Be aware that some characters can be, or can only be, represented by two or more code points 
in sequence. 

It is not strictly defined how PXCML will encode such characters, but generally 
they will be encoded as the necessary sequence of characters according to Unicode's Normalized 
Form ????? D.  

The double quote character itself ......

How a programming language or a support library encodes strings will be defined by the 
language or library. 



### Tuple {#tup}

A _tuple_ is a sequence of two or more expressions that .....

For example:

    (23, 46)
    
This tuple would be encoded as the number 23 followed by the number 46

.....



### Array {#arr}

An _array literal_ is a sequence of expressions with a length preceding it .....



    [23, 46]
    
This array would be encoded as the number 2 (the length) followed by 23 and then 46

.....









### Definition Array {#defarr}

A _definition array_ is a kind of expression that contains a set of definitions. 

It only ever serves the purpose of writing, in PXCML, a set of declarations in a more 
convenient and concise manner. 

There are two kinds of definition array:

 * an _implicit definition array_, where the associated bodies are implied; 
 
 * an _explicit definition array_, where the associated bodies values are specified explicitly. 

An implicit definition array is written in PXCML as a list of identifiers surrounded by `{` 
curly `}` braces. The identifiers are separated from each other by  a `,` comma. 

For each identifier, there is an implicit definition, whose head is a functor (the specified 
identifier) with no arguments, and associated body is an integer value, starting at 0 for the 
first identifier in the array and then incrementing for subsequent identifiers. 

An explicit nomination set is written in PXCML as a list of [definitions](#def), separated by 
a `,` comma and surrounded by `{` curly `}` braces. 

Either kind of definition array can have a _prefix_ in front of it. The functors of the 
definitions within it will all then be assumed to have the prefix in front of them. 

The value of a definition array is an array whose elements are the bodies of the (implicit or 
explicit) definitions, in the same order. 

For example, the following declaration:

    acme.traffic_lights = acme.traffic_light.{red, amber, green};

is the exact equivalent of the following four declarations:
    
    acme.traffic_lights = [0, 1, 2];
    acme.traffic_light.red = 0;
    acme.traffic_light.amber = 1;
    acme.traffic_light.green = 2;

Alternatively, the following declaration:

    acme.traffic_lights = acme.traffic_light.{red = 7, amber = 9, green = 23};

is the exact equivalent of the following four declarations:

    acme.traffic_lights = [7, 9, 23];
    acme.traffic_light.red = 7;
    acme.traffic_light.amber = 9;
    acme.traffic_light.green = 23;



-----------------------------------------------------------------------------------------------
## Formulas {#form}

PXCML supports the simplest kinds of _formula_ that will be very familiar. 

Unlike in normal programming languages, in PXCML a formula is always static, meaning that its value can be 
calculated at load time. 

The two numeric unary operators are: 

| Op  | Meaning                           |
| --- | --------------------------------- |
| `+` | Does not change the value         |
| `-` | Negative numeric value            |

The five numeric binary operators are:

| Op  | Meaning                           |
| --- | --------------------------------- |
| `+` | Add
| `-` | Subtract
| `*` | Multiply
| `/` | Divide
| `^` | Power of                          |

The array binary operator is:

| Op  | Meaning                           |
| --- | --------------------------------- |
| `&` | Concatenate                       |

Any use of one operator, with its argument or arguments, must be enclosed by parentheses 
before it can be used with another (different) operator. There is no precedence or 
associativity, therefore. 

It is conventional to put spaces around a binary operator, but not between a unary operator 
and its operand. 

Formulas are purely for convenience and readability. Using a formula is the equivalent of 
using a literal. The only difference is that the PXC loader calculates the result of the 
formula so you don't have to. 


### Examples

    +23
    
this is the same as `23`, and only exists to help improve human readability of PXC. 

    -23
    
This gives the value of minus 23. 

    17 + (-23)
    
.....

    2 * 3 * 4
    
This gives a value of 24. Note that, because only one operator is used (the `*` asterisk, for 
multiplication), no parentheses are necessary. This example could have been written as either 
of: 

    (2 * 3) * 4
    2 * (3 * 4)

But the parentheses are not necessary, and the result is the same. 

    (2 * 3) / 5
    
This gives a value of (exactly) 1.2. To be pedantic, it results in the rational number 6/5, as 
in PXC non-integers are encoded as two integers, one divided by another. 

.....



-----------------------------------------------------------------------------------------------
## Definitions {#def}

A _definition_ is a name that acts as a substitute for an expression in PXC. 

Definitions are ..... 


### Syntax

Syntactically, a definition comprises: 

 1. the _head_; 
 
 2. an `=` equals sign; 
 
 3. the _body_; 

 4. a `;` semicolon 'terminator'.
 
The head comprises:

 1. optionally, one or more [tags](#tag); 
 
 2. an identifier; 

 3. optionally, an `(` open parenthesis, an _argument list_, and then a `)` 
    close parenthesis. 
    
The argument list comprises a sequence of _argument identifiers_, with a `,` comma in between. 

The body is an expression. 


### Invocation

The argument identifiers can be used in the body (expression)



.....






### Expansion versus Emission

Wherever a definition is invoked, the Realizor makes a decision as to whether to:

 * resolve the definition by expanding it inline; or instead to 

 * emit machine code and resolve the definition with a call to the emitted code. 



.....



### Examples

.....

For example:

    foo.bar.hum = f(17, 89);
    
This definition would be encoded as:

 1. the morphid for 'definition' (?????); 
 
 2. .....

 3. the mini-number 2 (the number of arguments); 

 4. the arguments as expressions (each preceded by its appropriate morphid), in this case the 
    numbers 17 and 89. 

.....


```pxc
pi = 3.1415926;
circle = 2 * pi;
```

.....


```pxc
f(x) = x * 2;
apple_counter = f(23);
```

In this example, the first definition's identifier is `f`, with one argument identifier `x`. 
This definition is substituted by the value of the argument doubled. 

Then there is a definition named `apple_counter`, whose value is 46 (double 23). 

.....



-----------------------------------------------------------------------------------------------
## Tag Decorations {#tag}

A [definition](#def) or an [expression](#expr) can be _decorated_ with one or more _tags_. The 
tag or tags are placed immediately before the definition or expression. If there are two or 
more, the order in which they are written is unimportant. 

.....

Syntactically each tag is a `$` dollar sign followed by a _tag-word_. A tag-word is a letter 
character optionally followed by one or more alphanumeric characters. Conventionally, roman 
letters are in upper case. 

.....



### Debugging

The significance of putting a `$SUBR` tag in front of a definition is that an entry 
is made in the [symbol table](#symtab) for the module. 

A tag is put in front of 

.....

When realised in normal mode, the Realizor inserts an implicit invocation of the special 
`system.trace` definition, passing it the name of the definition and the name of the 
definition's module. 

.....






### Recognised Tags

Currently, the following tags are recognised:

| Tag       | In Front Of  | Meaning
| --------- | ------------ | ---------------------
| `$CONST`  | Definition   | [Constant](#const)    
| `$DATA`   | Definition   | [Datum](#data)
| `$SUBR`   | Definition   | [Subroutine](#subr)
| `$NYB`    | Expression   | [Compact Nybble Array](#cna)
| `$EXPORT` | Definition   | [Export](#export)







-----------------------------------------------------------------------------------------------
## Constants {#const}

.....








-----------------------------------------------------------------------------------------------
## Data {#data}

.....

sequence of [nybbles](#nyb)





















-----------------------------------------------------------------------------------------------
## Subroutines {#subr}

A [definition](#def) marked with a `$SUBR` [tag](#tag) is a _subroutine_. 

A subroutine is approximately the PXC equivalent of a procedure or function in most procedural 
languages. 

.....





For example, the following definition:

    $SUBR foo.bar.hum = f(17, 89);
    
defines a subroutine named `foo.bar.hum`. 


### Outcomes

A subroutine always returns a value as a result of its execution. If the subroutine is an 
implementation of a non-value returning procedure (or any callable language construct that 
does not return a value), then the subroutine should, by convention, return the number value 0 
to indicate that it executed successfully, or any other value (of any kind) to indicate that 
an error occurred. 

See the [`seq` predefined subroutine](#seq) for more. 

.....






### Predefined Subroutines

PXC predefines a set of subroutines whose bodies can be assumed to carry out the kinds of 
fundamental operations that (almost) any modern conventional (von Neumann) processor can be 
expected to provide. 

Naturally, calls to these subroutines are likely to be replaced by the appropriate machine 
code of the target architecture, rather than an actual call. 

Nevertheless, having these fundamental operations encoded in PXC as if they were ordinary 
calls means that any of them could actually be implemented as normal subroutines, and it also 
opens the door to adding new predefined subroutines in the future without disturbing existing 
PXC files. 

.....

Currently, the predefined subroutines are as follows.

| Name and Arguments | Returns/Does                   | See                               |
| ------------------ | ------------------------------ | --------------------------------- |
| `add(x,y)`         | `x` plus `y`                   | [Arithmetic](#arith)              |
| `sub(x,y)`         | `x` minus `y`                  | [Arithmetic](#arith)              |
| `mul(x,y)`         | `x` times `y`                  | [Arithmetic](#arith)              |
| `div(x,y)`         | `x` divided by `y`             | [Arithmetic](#arith)              |
| `seq(A,h)`         | calls each member of array `A` | [Sequence](#seq)                  |
| `jmp(n,t)`         | jump to `t`, `n` levels up     | [Jump](#jump)                     |
| `if(c,x,y)`        | `x` if `c` is true, else `y`   | [Binary Choice](#binchoi)         |
| `case(v,A,C)`      | chooses an expression          | [Multiple Choice](#multchoi)      |
| `` |  | [](#) |
| `` |  | [](#) |
| `` |  | [](#) |
| `` |  | [](#) |
| `` |  | [](#) |
| `` |  | [](#) |
| `` |  | [](#) |
| `` |  | [](#) |
| `` |  | [](#) |
| `` |  | [](#) |
| `` |  | [](#) |
| `` |  | [](#) |






All of these predefined subroutines are described fully in the following sections.



-----------------------------------------------------------------------------------------------
## Arithmetic Pre-Defined Subroutines





-----------------------------------------------------------------------------------------------
## ..... Pre-Defined Subroutine





-----------------------------------------------------------------------------------------------
## ..... Pre-Defined Subroutine





-----------------------------------------------------------------------------------------------
## ..... Pre-Defined Subroutine





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## Sequence Pre-Defined Subroutine {#seq}

A _sequence_ (in PXC terminology) is a predefined subroutine which takes two arguments: the 
first is an array of expressions that are to be evaluated one after the other, in the ascending 
index order of the array. 

.....

    seq(A,h)

.....

If any expression that is a member of the array `A`, other than the last member, returns any 
value other than the number 0, that indicates an error occurred, and the evaluation of the 
array immediately ends, returning the value. 

If 0 is returned, evaluation continues with the next expression in the array. 

Whatever value is returned by the last expression in the array is returned by the evaluation 
of the sequence. 


### Handlers {#hand}

The second argument, `h`, of the `seq` subroutine is an _error handler_.

The handler must be a pointer to a subroutine that takes one argument. That argument is called 
the _error value_. 

If any expression in the array `A` returns a value other than the number 0, indicating an 
error, the value returned is passed into the error handler, with its error value argument 
given the value. 


### Examples

.....





-----------------------------------------------------------------------------------------------
### Jump Pre-Defined Subroutine {#jump}

The _jump_ pre-defined subroutine is a very special subroutine, which defies the normal 
constraints of the semantics of subroutines. There is no way that this subroutine could ever 
be implemented as a normal subroutine. In particular, when a jump is evaluated it never 
returns, as such. 

Nevertheless, its semantics are simple enough to understand, and this is a fundamentally
important subroutine. 

The jump subroutine is called as follows:

    jmp(n,t)

The parameter `n` must be a non-negative integer and is the number of _escape levels_. The 
parameter `t` must also be a non-negative integer and is the _target_ of the jump. 

A jump must be within a [sequence](#seq). That sequence must be within another sequence if `n` 
is greater than 0. That sequence, in turn, must be within yet another sequence if `n` is 
greater than 1, and so on. 

The jump, when evaluated, causes evaluation to revert to the `t`th expression in the sequence 
`n` levels up from the current sequence. 

.....


### Examples 

.....





-----------------------------------------------------------------------------------------------
## ..... Pre-Defined Subroutine





-----------------------------------------------------------------------------------------------
## ..... Pre-Defined Subroutine





-----------------------------------------------------------------------------------------------
## ..... Pre-Defined Subroutine





-----------------------------------------------------------------------------------------------
## ..... Pre-Defined Subroutine





-----------------------------------------------------------------------------------------------
## ..... Pre-Defined Subroutine





-----------------------------------------------------------------------------------------------
## ..... Pre-Defined Subroutine





-----------------------------------------------------------------------------------------------
## ..... Pre-Defined Subroutine





-----------------------------------------------------------------------------------------------
## Rehydrate and Dehydrate Pre-Defined Subroutine

A task can be dehydrated and rehydrated



.....



The Ada properties `Read_And_Resume` and `Suspend_And_Write`







-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## Allocations {#alloc}

?????



An _allocation_ is an expression whose value is intended to be written into the [target 
output](#output). 

The actual binary data that is written into the target output will be in the appropriate format for the target, and it will have a [target address](?????)







-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## Labels


????? Needs review.



In various different kinds of context, one or more _labels_ can be written in front of an 
expression. The label will then have a value associated with that expression, depending on 
what kind of label it is and the context. 

If the same label has already been defined elsewhere, it must be the same kind of label and 
have the same value as the previous definition; it is an error otherwise. 


### Value Labels {#vallab}

A _value label_ is written as an [identifier](#id) followed by an `=` equal sign. 

The label will have the value of the expression.

.....

A value label can be parametised. .....



### Positional Labels {#poslab}

A _positional label_ occurs within a literal array, enumeration type declaration, compound type declaration, or ?????

The expression it applies to will be one in a sequence of expressions. The label will have the
 value of the position of the expression within this sequence; the first expression will have 
value 0, the second will have value 1, and so on. 

A positional label is written as an [identifier](#id) followed by `:` colon. 



### Address Labels {#addlab}

An _address label_ is associated with an expression that is an [allocation](#alloc)

An address label is written as an [identifier](#id) followed by `:` colon. 

The value of the address label will be the target address of the allocation. 

In addition, for any address label `L`, additional labels are defined:

 * a label named `L.size`, which is the number of [target allocation units](#alloc) taken up by 
   the allocation; 
   
 * a label named `L.type`, which is the type of the allocation; 













-----------------------------------------------------------------------------------------------
## Declarations

.....



### Value Declaration

    pi = 3.1415926;

A _value declaration_ defines the value of a [value label](#vallab), 


### Allocation Declaration





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## Data Types

.....

A _datatype expression_ is a more human-readable way to express the encoding of a type. 







### Integer

.....

An integer type signifies whole numbers within a specified _range_.

An integer type is written in [PXCML](#pxcml) as follows:

    sys.typ.int(n, d)

The _length_ of the type's range is `n`, and `d` is the lower bound (inclusive) of the range.

Both `n` and `d` must be integer values, and `n` must be non-negative. 

The upper bound `u` of the range is related to `n` and `d` according to the equation:

    u = d + n - 1

It is permitted for `n` to be 0, but then the type will be a [null type](#nulltypes)

.....





### Enumeration

An _enumeration type_ is a type whose valid values are a set of integer values, but there is a 
name given to each value. 

An enumeration type is written in PXCML as 

    sys.typ.enum(A)
    
where `A` is an array whose values are all integers. A [definition array](#defarr) is a 
convenient way to create this array. 

For each value of the array, the value must be an integer which is greater than all the 
preceding values in the array. 
 
.....



If the array has `n` values, and the values of the array start at 0 and each subsequent value 
is the previous value plus one, then the type is considered by the Realizor to be exactly the 
same as `sys.typ.int(n, 0)`. This kind of enumeration type is termed are _straight_. 

Otherwise, the enumeration type is termed _sparse_. 

Remember that a type in PXC defines a set of valid values, but not operations. 





For example, the following declaration:

    acme.types.traffic_light = sys.typ.enum(acme.traffic_light.{red, amber, green});
    
declares a sparse enumeration type `acme.types.traffic_light` with three values.

An alternative to this declaration would be the four declarations:

    acme.types.traffic_light = sys.typ.int(3, 0);
    acme.traffic_light.red = 0;
    acme.traffic_light.amber = 1;
    acme.traffic_light.green = 2;



As another example, .....

    acme.types.traffic_light = 
       sys.typ.sparse_enum(acme.traffic_light.{red = 7, amber = 9, green = 23});
    
declares a sparse enumeration type which has three defined values, and is the equivalent of: 

    acme.types.traffic_light = enum([7, 9, 23]);
    acme.traffic_light.red = 7;
    acme.traffic_light.amber = 9;
    acme.traffic_light.green = 23;

.....





### Boolean

Any Boolean type is represented by the enumeration type which, for convenience, is 
pre-declared as if by: 

    sys.typ.bool = sys.typ.enum({false, true});

.....



### Array

An _array_ is .....

An array type is written in PXCML as: 

    sys.typ.array(e, i)
    
where `e` is the element type and `i` is the index type. 




The index type must be an array whose elements are integer types. These are called the _index 
type elements_. 


.....




A sparse enumerated type cannot be directly used as an index type element. However, a hastable 
can be created that will convert from a spare enumerated type into an integer type. This can 
be used to achieve the effect of using a sparse enumerated type as an index type element. 


### Set

A discrete set, whose values are all of a discrete type, is represented by an array of Booleans 
.....



For convenience, .....

    sys.typ.set(t) = sys.typ.array(sys.typ.bool, t);




For example, a type representing a set of traffic lights could be declared like this:

    acme.types.traffic_light_set = set(acme.types.traffic_light);

This is exactly the same as the following declaration:

    acme.types.traffic_light_set = array(bool, traffic_light);

.....

..... can be converted to and from integer .....


### Compound

A _compound_ type (the same as a `struct` in C or a `record` in Pascal or Ada) is a type that combines two or more other types, which are called the _members_ of the compound type. 

A compound type is written in PXCML as:

    sys.typ.struct(A)
    
where A is an array of types. These are the _member types_ of the compound type. 

The definition of a compound of two members named `p.m1` and `p.m2`, whose types are `t1` and 
`t2` respectively, would be typically written in PXCML as: 

    sys.typ.struct(p.[m1: t1, m2: t2]);
    
For example, the following declaration:

    acme.types.point = sys.typ.struct(acme.coord.[x: sys.typ.float32, y: sys.typ.float32]);

defines a compound type with two members, labelled `x` and `y`. This compound type might be 
used for values that are the co-ordinates on a (flat) map, perhaps. 

This declaration is the exact equivalent of the the following three declarations: 

    acme.types.point = sys.typ.struct([sys.typ.float32, sys.typ.float32]);
    acme.coord.x = 0;
    acme.coord.y = 1;





### Character and String

.....

Characters are represented as integers. The values or meanings of the integers is not defined 
by PXC, and different programming languages will have different definitions of their own, but 
in PXCML it is conventional to use the code point values of [Unicode][2] .....

and for a string to simply be an array of characters. 

.....



### Binary Data

.....

array of bits, nybbles, bytes, etc.

For convenience,the following types are pre-declared: 

    sys.typ.bit  = sys.typ.int(1, 0);
    sys.typ.nyb  = sys.typ.int(16, 0);
    sys.typ.byte = sys.typ.int(2^8, 0);
    sys.typ.word = sys.typ.int(2^16, 0);
    sys.typ.long = sys.typ.int(2^32, 0);
    sys.typ.quad = sys.typ.int(2^64, 0);

So an array type that comprised four quads would be written: 

    sys.typ.array(sys.typ.quad, [int(4, 0)])

.....


### Overlay

An _overlay_ type (the same as a `union` in C) is a type that could be any of two or more 
different types at different times. The different possible types are called the _alternatives_ 
of the overlay type. 

An overlay of two types, `t1` and `t2`, would be written in PXCML as:

    sys.typ.union([m1: t1, m2: t2]);
    

The size of a union type is ......



### 



### 



### 



### Null Types {#nulltypes}

....

    sys.typ.null


### Encodings: Predefined Type Definitions

.....

```pxc
sys.typ.int(lo,hi) = [1, lo, hi];
sys.typ.enum(A) = [2] & A;
sys.typ.sparse_enum(A) = [3] & A;
sys.typ.ptr(t) = [4, t];
sys.typ.array(e,i) = [5, e, i];
sys.typ.struct(A) = [6] & A;
sys.typ.union(A) = [7] & A;


```



### 



### 



### Nybble Array {#nybarr}

A _nybble array_ is an array of nybbles indexed by an integer starting from 0.

For convenience, it is pre-defined as:

    sys.typ.nybarr(n) = sys.typ.array(sys.typ.nyb, [sys.typ.int(n, 0)]);


### Compact Nybble Array {#cna}

A special type, the _compact nybble array_, is an array of nybbles indexed by an integer 
between 0 and 15 (inclusive). A compact nybble array is encoded as the length minus one, as a 
single nybble, followed by the nybbles that are the elements of the array. 

As a PXCML literal, a compact nybble array is written as the special word `$NYB` followed by 
an [array literal](#arr). For example: 

    $NYB [1,2,3]

represents a compact nybble array of length 3 (indexes 1 to 3) whose elements are: 1, then 2, 
then 3. 



### 



### Type Type

The pre-defined type `sys.typ.type` is the type of values that are themselves types. 






### 















-----------------------------------------------------------------------------------------------
## Sizes



.......



```pxc
sys.typ.size(T) =
   if((T # 0) = 1, sys.math.logb(2, T # 1),
   if((T # 0) = 2, sys.math.logb(2, T # (sys.len(T) - 1)),
   if



sys.typ.int(n,d) = [1, n, d];
sys.typ.enum(A) = [2] & A;
sys.typ.ptr(T) = [4] & T;
sys.typ.array(e,i) = [5, e, i];
sys.typ.struct(A) = [6] & A;
sys.typ.union(A) = [7] & A;
```






-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## Ada Include

.....

    ada,pxc.txt



```pxc
ada: definitions
```

.....


### Integers

An Ada integer type .....

```pxc
   ada.int.min = sys.int.min;
   ada.int.max = sys.int.max;

   ada.int = int(ada.int.min,ada.int.max);
```


### 

```pxc

```


### 

```pxc

```


### 

```pxc

```


### 

```pxc

```


### 

```pxc

```


### Arrays

An Ada array type is encoded as the vector of lengths `L`, then the vector of starting indices 
(offsets) `S`, then the element type `T`. Both vectors are PXC arrays whose length is the number of 
dimensions of the Ada array. 

```pxc
   ada.array(L,S,T) = cmpd([ len: L, off: S, elty: T ]);
```

.....


## Characters and Strings

An Ada character type is an integer between 0 and 127. An Ada string type is a 
single-dimension array starting from 1 of Ada characters. 

```pxc
   ada.char = int(0,127);
   
   ada.string(n) = ada.array([n],[1],ada.char);
```

.....


### 

```pxc

```


### 

```pxc

```


### 

```pxc

```


### 

```pxc

```


### 

```pxc

```


### 

```pxc

```


### 

```pxc

```


### 

```pxc

```


### 

```pxc

```


### 

```pxc

```


### 

```pxc

```


### 

```pxc

```






-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## Null Types {#nulltypes}

A _null type_ is a type that has no (defined, allowed, valid) values. 

.....






-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## Symbol Table {#symtab}

........



-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## Export Table {#exptab}

........





-----------------------------------------------------------------------------------------------
## Exports {#export}

If the `$EXPORT` tag .....

Definitions without this tag are _internal definitions_, and are only visible within the 
module they are defined within. 

An external definition has an entry for it added to the 
[export table](#exptab) for the module 

.....



-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 













????? should be programming language independent


PXC partition is actually expressed with Ada source in the form of qualified expressions, as 
described in [13.8 Machine Code Insertions][1] of the Ada Reference Manual. 

.....

For example, the following shows the declaration of a procedure which contains PXC partition:

```ada
with System.Machine_Code;

type Integer_Array is array (Positive range <>) of Integer;

procedure Rotate_Left (A: in out Integer_Array; Temp: in out Integer)
is
   use System.Machine_Code;
begin
   PXC.COPY'(A'Address, Temp'Address, Integer'Size);
   PXC.COPY'(
   ???????
end;
```

.....

ECLAT also permits the machine code of any of its supported target architectures to be included 
in Ada source text, in exactly the same way. 

.....











-----------------------------------------------------------------------------------------------
## References

[1](https://www.adaic.org/resources/add_content/standards/05rm/html/RM-13-8.html)


























### Advice

PXC source text is, in practice, only just human-readable. It is almost always preferable to 
use another programming language (normal Ada, for example) to express software, because it 
will be more readable and maintainable. 

The [Run Time Support](RTS.md) modules use ......






?????### Declarations

?????Although the Ada Reference Manual section 13.8 does not allow it, ECLAT demonstrates a rare 
defiance of the Ada standard by allowing certain kinds of declaration in a code statement 
procedure: 

.....




    
#### Labels

?????A normal Ada label is allowed to precede a code statement, and declares a location in the 
PXC instructions to which a JUMP or CALL can be directed. The instruction corresponding to the 
code statement is the instruction to which the JUMP or CALL will go. 









### 




### 




### Type Declarations









-----------------------------------------------------------------------------------------------
## Data Types

PXC recognises the following _basic data types_:

| Name    | Description                          |
| ------- | ------------------------------------ |
| `INT8`  | Signed 8-bit integer                 |
| `INT16` | Signed 16-bit integer                |
| `INT32` | Signed 32-bit integer                |
| `INT64` | Signed 64-bit integer                |
| `UNS8`  | Unsigned 8-bit integer               |
| `UNS16` | Unsigned 16-bit integer              |
| `UNS32` | Unsigned 32-bit integer              |
| `UNS64` | Unsigned 64-bit integer              |
| `SNG`   | IEEE 32-bit floating-point           |
| `DBL`   | IEEE 64-bit floating-point           |
| `ADDR`  | Memory address (offset plus segment) |
| `OFF`   | Memory offset                        |
| `SEG`   | Memory segment                       |
| `BIT`   | Binary digit                         |
| `T#N`   | Index into `N`th dim of array of `T` |
| `T*`    | Index into any element of array `T`  |
| ``      |                                      |
| ``      |                                      |
| ``      |                                      |



A _data type_ can be any of:

 * a basic data type

 * any array of any other data type
 
 * an index of an array of any other data type
 
An array has indexes from 0 up to N-1, where N is the number of elements in the array.

An array of N elements of a data type T is written:

    T[N]
    
A row-major matrix with N rows and M columns of data type T is written:

    T[M][N]
    
If the matrix were column-major, it would be written:

    T[N][M]
    
This can continue for up to 8 dimensions.

An index into an array A is written:

    A#1
    
For example, if we had a row-major matrix of 17 rows by 5 columns of 32-bit floats, and wanted 
the index data type into its columns, we would write:

    SNG[5][17]#1
    
If we wanted the index data type into its rows, we could write either of:

    SNG[5][17]#2
    SNG[5]#1
    
We can declare a name N for a data type T with the line:

    TYPE N=T
    
For example:

    TYPE MYMAT1=SNG[5][17]
    TYPE XROW=MYMAT1#1
    TYPE XCOL=MYMAT1#2




-----------------------------------------------------------------------------------------------
## Registers

The PXC concept of registers is that there is an infinite array of registers of each data type





-----------------------------------------------------------------------------------------------
## Instructions

.....

### Copy



    COPY 





-----------------------------------------------------------------------------------------------
## 








.....

excerpted from [Library for Text File Search and Replace.](?????) Copyright (C) 2003 Martin Krischik:

```ada
package body sarCCTranslate.CommandLine is

    package IO         renames Ada.Text_IO;
    package U_String   renames Ada.Strings.Unbounded;
    package F          renames AdaCL.SAR.Filter;
    package F_List     renames AdaCL.SAR.Filter.List;
    package F_I        renames AdaCL.SAR.Filter.Insert;
    package F_S_Inc    renames AdaCL.SAR.Filters.Search.Includes;
    package GetOpt     renames AdaCL.GetOpt;
    package Super      renames AdaCL.SAR.CommandLine;
    package FU         renames AdaCL.OS.FileUtil;

    procedure Filter_File (
        --  the Object itself
        This : in out Object)
    is
        --  Trace : AdaCL.Trace.Object := AdaCL.Trace.FuncTrace (AdaCL.Trace.Entity & ':' & AdaCL.Trace.Source);
        --  pragma Unreferenced (Trace);

        This_S    : Super.Object renames Super.Object (This);
        This_C    : Object'Class renames Object'Class (This);
    begin
        Super.Filter_File (This_S);

        Sort : declare
            Out_File  : constant String := Get_OutFile  (This_C);
            Temp_File : constant String := Out_File & ".Temp";
        begin
            FU.Sort.File (From    => Out_File,
                          To      => Temp_File,
                          Options => FU.Sort.UniqueOnly);

            FU.Text.To_Unix (File => Temp_File);

            FU.Copy.File (From    => Temp_File,
                          To      => Out_File,
                          Options => FU.Copy.Verbose);
        end Sort;
    exception
        when AnException : others =>
            AdaCL.Trace.Write_Error (AnException, AdaCL.Trace.Entity, AdaCL.Trace.Source);
    end Filter_File;
```




```pxc








```





















-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## References

[2] <https://www.unicode.org/charts/> "Unicode 14.0 Character Code Charts"





