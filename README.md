-----------------------------------------------------------------------------------------------
# __ECLAT__
# Experimental Compiler Library And Tools

Author: Nick Roberts  
Date:   June 2019  

__ECLAT__ is going to be a compiler for multiple programming languages, targeting multiple 
instruction set architectures and platforms (operating systems). 

ECLAT is in three parts:

 * the compiler
 * the builder
 * the realizor

The compiler compiles source text into libraries. The builder builds pseudo-code modules from 
libraries. The realizor ‘realises’ modules into directly executable machine code. 

-----------------------------------------------------------------------------------------------
## Current Status

As of June 2019:

Just starting.

-----------------------------------------------------------------------------------------------
## Aims

The objectives of this part of the project are:

 * An alternative to the GNAT/GCC technology. This is the only open-source Ada compiler 
   currently available, to my knowledge, and I think it would be healthier for there to be more
   than just one.

 * A compiler that demonstrates state-of-the-art optimisation techniques (eventually).

 * An Ada compiler with full garbage collection.

 * Ultimately, to provide one piece of the giant jigsaw puzzle that has to be solved to create 
   a new general-purpose operating system with modern features.

-----------------------------------------------------------------------------------------------
## Plan of Action

As of June 2019:

 1. Make a minimal ECLAT capable of self-hosting 
 2. Add stuff on from there

The self-hosting will initially be on the Microsoft(R) Windows(R) x86-64 platform, I think. 
Initially the only kind of source text that will be read is 8-bit Latin-1, and only a subset of 
the standard libraries will be implemented. 

There is no timeline at this stage.

-----------------------------------------------------------------------------------------------
## What Does ‘Self-Hosting’ Mean?

A compiler can be self-hosting if it can compile itself and build itself to the point of being 
able to run. Obviously a compiler can only be self-hosting if it is written in the same 
languages that it is able to compile. 

Until a compiler is self-hosting, it must be possible to compile it using some other toolset.

ECLAT is written in Ada. In the case of ECLAT, I am aiming for it to become self-hosting. Until 
it does, it will be compiled using GNAT (see <https://www.adacore.com/community>). 

I’m not ruling out the possibility of using other languages in the development of ECLAT, but I 
don't have plans in that direction at this point. 

-----------------------------------------------------------------------------------------------
## Project

ECLAT is a part of the [AdaOS Project](./doc/Project.md).

-----------------------------------------------------------------------------------------------
## More Details

See [ECLAT](./doc/ECLAT.md).

