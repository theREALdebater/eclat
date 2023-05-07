-----------------------------------------------------------------------------------------------
# __ECLAT__

Experimental Compiler Library And Tools

# Monitoring

Author: Nick Roberts
Date: December 2019

ECLAT will provide a general-purpose mechanism, called __monitoring__, for the programmed 
intervention in the normal execution of programs. 

This mechanism will form the basis for:

 * debugging and tracing
 * profiling
 
However, it is so general-purpose it may end up being used for many other things in the future.



-----------------------------------------------------------------------------------------------
## Predefined Monitoring Library

There is a predefined library unit, which is a package, named `AdaOS.Monitoring`. This library 
unit cannot be declared in the normal way, and will instead be generated automatically by the 
compiler within the primary library. It will never exist in any library other than the primary. 

This package is the _predefined monitoring package_, or _PMP_. Whenever this document says "the 
PMP declares" it should be taken to read "the predefined monitoring package contains the 
visible declaration of". 



-----------------------------------------------------------------------------------------------
## Flowpoints

Monitoring is based on the notion of a _flowpoint_. 

Flowpoints are conceptual locations in the program’s source text. Informally, during execution 
of the program, execution moves from an executable construct to a flowpoint and then to another 
executable construct, and then to another flowpoint, and so on. Generally there is a flowpoint 
just before every explicit executable construct, but sometimes there are implicit executable 
constructs that also have flowpoints before them. 

In Ada, an executable construct is any of:

 * a statement (that is 'executed') 
 * a declarative item (that is 'elaborated')
 * a function in an expression (that is 'called')
 * an `Inspection_Point` pragma if it doesn't already have a flowpoint immediately after it

The elaboration of a declarative item and the calling of a function in an expression both count terminologically as execution. 

As an example, in this slice of (very fictional) Ada code:

```
   $ A: Footle( X );
   $ B: Integer := 42;
begin
   $ A.Barking_Level := B;
   $ B := B + X;
```
 
I have indicated where a flowpoint would be with the `$` (dollar) sign. 

The PMP declares a type derived from `Standard.Integer` named `Monitoring_Flowpoint`. Each 
value of this type corresponds to, and identifies, a different flowpoint in the effective 
library. The lower bound is always 0. If the number of different flowpoints in the effective 
library is n, the upper bound is n-1. 

Execution of the program is defined in the Ada Reference Manual, but flowpoints add to this 
execution model: a flowpoint immediately before a statement is _handled_ just before the 
statement is executed; a flowpoint immediately before a declarative item is handled just before 
the elaboration of the declarative item. 

In a compound statement, there is a flowpoint before the compound statement and also before 
each statement within it. As another Ada example: 

```
   $ S1;
   $ if C then
      $ S2;
   else
      $ S3;
   end if;
   $ S4;
```

And another:

```
   $ S1;
   $ for N in 1..10 loop
      $ S2( N );
   end loop;
   $ S3;
```

In the last example, the flowpoint before the `for` will be executed once each time execution 
flows through this snippet of statements, but the flowpoint before the call to `S2` will be 
handled 10 times (just as `S2` will be called 10 times). 

If this were done using a `while` loop:

```
   $ N := 0;
   $ while $ N < 10 loop
      $ N := $ @ + 1;
      $ S2( N );
   end loop;
   $ S3;
```

In this example, there is an extra flowpoint in front of the test condition `N < 10`, because 
it contains a call to the `"<"` function. This flowpoint, as well as the one before the call to 
`S2`, will be executed 10 times. Similarly, there is an extra flowpoint in front of the 
expression `@ + 1` because it contains a call to the function `"+"`. 

Finally, for a procedure that has no explicit `return` statement:

```
   $ subtype LED_Number is Integer range 1..10;

   procedure Cycle( N: in out LED_Number ) is
   begin
      $ if $ N = LED_Number'Last then
         $ N := LED_Number'First;
      else
         $ N := $ @ + 1;
      end if;
   $ end;
```

There is an extra flowpoint after the last statement in the body of the procedure. Because 
there is no explicit `return` statement, there is an implicit one at the end of the procedure, 
and a corresponding flowpoint just before it. 

Note the flowpoint just before the `subtype` declaration. All the other flowpoints in this 
example will be contained (q.v.) by the procedure `Cycle`. 



-----------------------------------------------------------------------------------------------
## Categories

A library can have one or more _flowpoint categories_, each of which has its own unique 
identifier.

The predefined monitoring package contains the visible declaration of an enumerated type named `Flowpoint_Category`. The members of this type are the flowpoint categories for the library. 

Every flowpoint is associated with a set of flowpoint categories. (Therefore, every category is 
associated with a set of flowpoints.)

The predefined monitoring package contains the visible declaration of the type 
`Flowpoint_Category_Mapping`:

```ada
   type Flowpoint_Category_Mapping is 
      array (Monitoring_Flowpoint, Flowpoint_Category) of Boolean;
```

This is a Boolean array that has the value `True` for a flowpoint and a category when the 
flowpoint is in the set of flowpoints of the category and, conversely, the category is in the 
set of categories of the flowpoint. (It has `False` otherwise.)

There are two predefined categories:

 * `Debugging`, see [Debugging](Debugging.md)
 * `Profiling`, see [Profiling](Profiling.md)

The configuration pragma `Monitoring_Category` can be used to add further categories. 

For example:

```ada
pragma Monitoring_Category (Flowpoint_Auditing);
```

would add the category named `Flowpoint_Auditing`, and set its handler (q.v.) to be the 
procedure `AdaOS.Monitoring.Handle_Flowpoint_Auditing`.

By default, every flowpoint is in every category. 

However, there are certain rules that can eliminate a flowpoint from a category, and also the
pragma ??? can be used .....



The predefined monitoring package contains the visible declaration of the function 
`Category_Mapping` which returns the mapping

```ada
   function Category_Mapping return Flowpoint_Category_Mapping;
```



-----------------------------------------------------------------------------------------------
## Handling Flowpoints

For each category `C` there must be a library-level procedure called its _handler_ which must 
conform to the following profile:

```
   procedure AdaOS.Monitoring.Handle_C (Flowpoint: in Monitoring_Flowpoint)
```

The procedure must be declared as a child of the package `AdaOS.Monitoring`.

The procedure may be [inlined][1], and it is recommended to do so, because the handling of 
flowpoints is likely to be very speed critical.

The procedure, and all subprograms it could possibly call (directly or indirectly), must have 
no flowpoint that is in the category `C`. If there is one, it is a fatal (static) error (and 
the compilation is rejected). 

.....



-----------------------------------------------------------------------------------------------
## Containers and Locations

Every flowpoint is considered to be inside a compilation unit ......

The predefined monitoring package contains the visible declaration of the function `Container`
which returns the fully qualified name of the compilation which contains a specific flowpoint:

```ada
   function Container (Flowpoint: in Monitoring_Flowpoint) return Wide_String;
``` 

A _location_ is a place in the source text. A location identifies a specific character in a 
specific file. Since it is possible (if unlikely) for source text to not correspond to any 
identifiable file, and it is possible that the information to identify a character in a file 
might not be available, there is a concept of a _null location_ which simply signifies that a
location is unknown. 

The PMP declares the record type `Location':

```ada
   type Location is 
      record
         File: AdaOS.Reflection.Library_File;
         Page: Ada.Text_IO.Count;
         Line: Ada.Text_IO.Count;
         Col:  Ada.Text_IO.Count;
      end record;
```


-----------------------------------------------------------------------------------------------
## Control Flowpoints

A flowpoint `B` is a _precedent_ of flowpoint `A` if `B` is handled immediately before `A` 
(with no other flowpoint handled in between). 

A _control flowpoint_ is defined as a flowpoint which is capable of being the precedent of two 
or more other flowpoints. 

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
## References

[1]: <http://ada-auth.org/standards/12rm/html/RM-6-3-2.html> "6.3.2 Inline Expansion of 
     Subprograms"




