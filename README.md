# ECLAT
Experimental Compiler Library And Tools (Ada compiler)

Super early stage of developing this one. Much more details later. Bear with.

## What Is It?

ECLAT is going to be a compiler for multiple programming languages, targeting multiple instruction set architectures and platforms (operating systems). 

## Programming Languages

Languages already on the slate are Ada and C++. Hopefully C++ will also cover C in a sensible way. There will hopefully be many more languages supported in the future. 

## Targets

Target platforms will be general-purpose operating systems, such as Microsoft(R) Windows(R), Linux, the BSDs, Mac OSX, and similar. Target architectures will be x86-64 and ARMv8 (AArch64); further architectures may be added later. 

I'm going to start with Microsoft(R) Windows(R) on the x86-64.

## How Will It Work?

ECLAT will compile source text (plain text files) to add units to an ECLAT library. It will also build units in a library into modules (binary files) containing an intermediate code that is architecture-agnostic. Then a separate program will generate executable files, containing machine code, from the intermediate code.

Currently I am calling the intermediate code 'PCX', and the separate program that generates machine code the 'Realizor', but these names might change.

To start with, there will be one command-line program that will act as a kind of 'make' program but with the ECLAT compiler and builder built-in. 

This program will be named `eclat` on Unix-based platforms, `eclat.exe` on the Microsoft(R) Windows(R) platform, and it might be `eclat.pxc` when ECLAT gets to being self-hosting.

The Realizor program might be named `pxcr`.

## Debugging

The intermediate code (PXC) will support the addition of data objects, which will be used by ECLAT to store debugging information. There will also be a general-purpose 'monitoring' framework -- whereby intervening code can be automatically inserted at 'flowpoints' -- which will facilitate the addition of debugging functions.

Hopefully, at some point, I'll be able to implement a GUI debugger (called 'EIDOS') which can operate remotely over a network. I intend this debugger to be a lot more sophisticated than the usual.

## Optimisation

ECLAT will offer two modes in which it should build intermediate code (PXC): normal; published. In normal mode, debugging information is included and no optimisation is performed. In published mode, all debugging information and monitoring functionality is removed and maximum optimisation is performed. 

The Realizor will itself perform optimisation on PXC that has been built in published mode. In addition, the Realizor will automatically carry out a periodic cycle of: generate code that, when run, records execution performance statistics (profiling data); re-realise the code, with improvements based on the previously recorded statistics. When the cycle reaches the stage where re-realisation produces no further improvements, the cycle is stopped. 

Initially, only normal mode will be supported.

## Configuration

I'm going to introduce a configuration framework -- something that standard Ada currently lacks -- and an implementation based on YAML. That will be used by ECLAT and the Realizor. 

## Logging

I'm also going to introduce a logging (and auditing) framework, at some point.

## Services

I intend to eventually implement the full Annex E (DSA) with implementations of the PCS. See the Ada Reference Manual for details.

In addition to this, I intend to introduce a framework for intra-program services, and implementations of it.

I also intend to introduce a utility that, given a description of the interface(s), can be used to create the Ada source text that implements the service (server-side skeleton) and access to the service (client-side). The implementation will support a configurable choice of mechanisms (web API, web service, RPC/UDP optionally tunnelling through TCP/TLS, etc.). 

## Plan of Action

As of April 2019:

1. Make a minimal ECLAT capable of self-hosting
2. Add stuff on from there

The self-hosting will initially be on the Microsoft(R) Windows(R) (x64) platform, I think. Initially the only kind of source text that will be read is 8-bit Latin-1, and only a subset of the standard libraries will be implemented.

## What Does 'Self-Hosting' Mean?

A compiler can be self-hosting if it can compile itself and build itself to the point of being able to run. Obviously a compiler can only be self-hosting if it is written in the same languages that it is able to compile. 

Until a compiler is self-hosting, it must be possible to compile it using some other toolset.

ECLAT is written in Ada. In the case of ECLAT, I am aiming for it to become self-hosting. Until it does, it will be compiled using GNAT (see <https://www.adacore.com/community>). 

I'm not ruling out the possibility of using other languages in the development of ECLAT, but I don't have plans in that direction at this point. 

## Licence

The ECLAT project source text will be published under the GNU General Public Licence version 3 (see <http://www.gnu.org/licenses/>). 

## Aims

The objectives of this project are:

* An alternative to the GNAT/GCC technology. This is the only open-source Ada compiler currently available, to my knowledge, and I think it would be healthy to have more than just one.

* An Ada compiler that demonstrates (eventually) state-of-the-art optimisation techniques.

* Ultimately, to provide one piece of the giant jigsaw puzzle that has to be solved to create a new general-purpose operating system with modern features.

## Who Are You?

I am Nick Roberts, crusty veteran of the computer world, and still dreaming.

E-mail: <mailto:nick.roberts@acm.org>

I am seeking contributors and supporters for this project.

By the way, I am from the UK, so please forgive me if my spelling sometimes seems strange. I intend the identifiers in all source text to have North American spelling.