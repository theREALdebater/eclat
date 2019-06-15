-----------------------------------------------------------------------------------------------
# __ECLAT__
# Experimental Compiler Library And Tools
# In Slightly More Detail

Author: Nick Roberts
Date: June 2019

__ECLAT__ is going to be a compiler for multiple programming languages, targeting multiple 
instruction set architectures and platforms (operating systems). 

ECLAT is in three parts:

* the compiler
* the builder
* the realizor

The compiler compiles source text into libraries. The builder builds pseudo-code modules from 
libraries. The realizor ‘realises’ modules into directly executable machine code. 

Here is just a little bit more information about the plans for ECLAT.

-----------------------------------------------------------------------------------------------
## Programming Languages

Languages already on the slate are:

* Ada
* C
* C++

There will hopefully be many more languages supported in the future. 

For C and C++, the emphasis will be on flexibility. The primary purpose of supporting these 
languages (at least initially) will be to port existing code with minimum effort. I think I’m 
going to aim for conformity with (in order): the standards for C and C++; POSIX C; GNU C and 
C++. 

Thankfully, all Ada implementations adhere to the standard very closely, so there is only one 
thing to aim for. On the other hand, I’ll aim to maximise compatibility with GNAT 
implementation-defined featues. At some point I’ll make a tool that converts a tree of GNAT 
project files into the equivalent ECLAT configuration.

-----------------------------------------------------------------------------------------------
## Targets

Target platforms will be general-purpose operating systems, such as Microsoft(R) Windows(R), 
Linux, the BSDs, Mac OSX, and similar. Target architectures will be x86-64 (x64) and ARMv8 
(AArch64); further architectures may be added later. 

I’m going to start with Microsoft(R) Windows(R) on the x86-64.

After a while, I’m hoping that there will be a ‘bare metal’ target, which supports all the different machine architectures but with no external platform or operating system at all. This would facilitate certain embedded applications and similar projects, but it would also be a step towards AdaOS (where the entire operating system resides in the run time support). 

-----------------------------------------------------------------------------------------------
## How Will It Work?

The compiler will compile source text (plain text files) to add units to an ECLAT library. The
builder will build units in a library (an effective library q.v.) into modules (binary files) 
containing pseudo-code code that is architecture-independent. The realizor will generate 
executable files, containing native machine code, from the pseudo-code modules. 

Currently I am calling the pseudo-code ‘PCX’. 

To start with, there will be one command-line program that will act as a kind of ‘make’ program 
but with the ECLAT compiler, builder, and realizor built-in. 

This program will be named `eclat` on Unix-based platforms, `eclat.exe` on the Microsoft(R) 
Windows(R) platform, and it might be `eclat.pxc` when ECLAT gets to being self-hosting. 

The reason why ‘ECLAT’ mentions *libraries* and tools is because the whole of ECLAT’s 
functionality will be exposed as an ECLAT library (itself named `ECLAT`). This will allow 
tools, services, and application programs to have some or all of ECLAT’s functionality built in 
without resorting to clunky execution of external programs. 

Ada defines what a library unit is, and what its name is. It is possible for one source file to 
contain multiple library units (but it is not possible for any one library unit to be in more 
than one source file).

For C and C++, a library unit will be a translation unit, and its name will be the name of the 
file (including elements of its path, if that is necessary to disambiguate). Note that the way 
ECLAT works is, in certain ways, different to the standard C/C++ model. There are no object 
files. I believe this would be sufficient to enable existing software to be readily ported. I might include a way for object files to be linked in, but this would have certain security 
consequences.  

-----------------------------------------------------------------------------------------------
## Libraries

There are two kinds of library: _wrapped_; _unwrapped_.

Only an unwrapped library can be changed, by compiling source text which may cause 
configuration settings or library units to be added or replaced. Every time the library is 
changed, its version is updated. 

An unwrapped library is kept as a set of files within one directory. When a (re)compilation 
occurs, one or more new files are added, which might contain library units that supersede 
those in previously generated files. There are also files that keep an index to the source 
files, ASIS data, and so on. 

A command-line tool will allow an unwrapped library to be converted into a wrapped libary. A 
wrapped library is all stored in one file (with the extension `.elw`). It is assigned a 
universally unique name, which is a URN. All unused and inaccessible units are removed from the 
wrapped library; all references to the source files are removed. This is the only way to create 
a wrapped library; a wrapped library cannot be modified. 

Any library is permitted to depend on one or more wrapped libraries. They, in turn, could 
depend on other wrapped libraries, and so on to form a tree of dependencies. The resulting 
‘effective library’ comprises the visible units of all the libraries in this tree, and there 
must not be any name clashes. Every dependency would specify the version of the wrapped library 
it depends on. 

This approach will make it possible for wrapped libraries to be distributed as a resource. 

Hopefully, at some point, I’ll be able to implement a GUI library manager (called ‘ALDUS’) to 
make it easy to create and manage a ‘sanctuary’ of wrapped libraries. It would also make it 
easy to browse for and use libraries in sanctuaries, as well as updating to newer versions, 
etc. There’ll be a command-line tool as well. 

-----------------------------------------------------------------------------------------------
## Debugging and Monitoring

Pseudo-code (PXC) modules will support the inclusion of data objects, which will be used by 
ECLAT to store debugging information (symbol data). 

There will also be a general-purpose ‘monitoring’ framework -- whereby intervening code can be 
automatically inserted at ‘flowpoints’ -- which will facilitate the addition of debugging 
functions.

ECLAT’s exception mechanism will use the debugging information, if available, to enrich 
exception information. 

Hopefully, at some point, I’ll be able to implement a GUI debugger (called ‘EIDOS’) which can 
operate remotely over a network. I intend to make this debugger a lot more sophisticated than 
typical current offerings. 

The monitoring framework could also support a variety of other purposes. 

-----------------------------------------------------------------------------------------------
## Optimisation

ECLAT will offer two modes in which it builds pseudo-code (PXC) modules: _normal_; _release_. 

In normal mode, debugging information is included and no optimisation is performed. In release 
mode, all debugging information and monitoring functionality is removed and maximum 
optimisation is performed. 

Initially, only normal mode will be supported. 

At some point in the future, I think we could offer the possibility of programs being 
distributed in the form of PXC modules (built in release mode). Each platform (operating 
system) would have its own independent realizor (tool/program/service) to convert these into 
machine code. 

This independent realizor would itself perform optimisation on pseudo-code (that has been built 
in release mode), and automatically generate machine code that records execution performance 
statistics (profiling data). The realizor would then periodically re-realise the program, with 
improvements based on previously recorded statistics. When this cycle reaches the stage where 
re-realisation produces no further improvements, the cycle is automatically stopped. 

-----------------------------------------------------------------------------------------------
## Garbage Collection

Garbage collection is the automatic reclamation of memory that is no longer being used during a 
program’s execution. 

Typical Ada implementations provide little or no garbage collection, and rely on the programmer 
using explicit memory management techniques.  

I plan for ECLAT to support advanced garbage collection, with:

 * a classical reachability algorithm to establish allocated objects that are eligible for 
   reclamation;
  
 * separate sub-heaps for fixed-size small objects, which do not suffer from fragmentation and 
   allow one-hop pointers;
  
 * generational sub-heaps, reducing the amount of object copying required for defragmentation; 

 * automatically ascertaining every pool whose usage is confined to one task, so that locking 
   can be avoided for access to the pool;
   
 * mechanisms to control garbage collection in various detailed ways;
 
 * probably lots of other features as well.

This wouldn’t prevent the development of Ada software that avoided the use of garbage 
collection, but it would provide the option. 

-----------------------------------------------------------------------------------------------
## Modules, Shared Code, and Plug-Ins

ECLAT will build ‘modules’ from libraries. Each module is a pseudo-code (PXC) file (with the 
file extension `.pxc`). 

The usual kind of module built will be a ‘program’ module, which corresponds to a main program. 
However, ‘auxiliary’ modules can be built, which expose a set of entrypoints (subprograms) to
be called by other modules. In fact, the only thing that is different about a program module is 
that it has one entrypoint, named `main` (which expects parameters for program arguments, 
environment variables, standard files, and so on, and returns an exit code integer). 

The set of modules that make up a complete program all have a static set of dependency 
relationships with one another. 

Existing platforms (operating systems) have their own mechanisms for shared code: on 
Microsoft(R) Windows(R), it is Dynamic Link Loadable (DLL) files; on Linux/elf it is `.so` 
(shared object) files; there are similar facilities on other platforms. ECLAT will allow both
main executable programs as well as auxiliary execution files to be targeted. 

There will be implementation-defined pragmas and aspects, as well as the use of the `Import` 
and `Export` pragmas and aspects, to determine whether code is making a program module, or 
making or consuming an auxiliary module, or or making or consuming an auxiliary execution file, 
and establishing the details. COM may also be directly supported in a similar way. 

The realizor will generate a primary executable (file) by gathering the downward 
closure of all the modules it depends on, directly or indirectly, and translating them all 
together as a whole into native machine code. 

Certain standard modules will be expected to be available, including modules that contain the 
various different versions of run time support code. 

ECLAT will provide two different mechanisms to enable a program that has already been built to 
be extended by code that itself has already been built. One mechanism uses a module that is optional, but can be added into the program at realisation time; such a module is called a ‘static plug-in’. The other mechanism allows an auxiliary execution file to be loaded during the program’s execution; such a file is called a ‘dynamic plug-in’. Both mechanisms are based on the 
addition of types within the class of a type already defined in the program. 

-----------------------------------------------------------------------------------------------
## Services

I intend to eventually implement the full Distributed Systems Annex (DSA), with at least one 
implementation of the PCS. See the Ada Reference Manual, Annex E, for details. 

The DSA defines how an Ada program can have multiple ‘partitions’. Any one instance of the 
program can run different partitions on different computers. 

In addition to this, I intend to introduce a framework for intra-program services, and 
an implementation of it. This would allow multiple instances of a program (each running in its 
own process) to interact with a service, which is part of the program (at the source text 
level) but runs in a separate process (potentially on a different computer). 

This framework would include use of the security framework (see below). 

There is also planned a whole big project, named Nyota, for supporting the provision or 
consumption of a web service (e.g. HTTP/SOAP/XML based), RESTful web API service (e.g. using 
JSON), and perhaps other kinds of services. 

-----------------------------------------------------------------------------------------------
## Configuration

I’m going to introduce a configuration framework -- something that standard Ada currently lacks 
-- and some implementations:

* read a YAML file;
* use environment variables (perhaps);
* use program arguments (interpreting options etc) (perhaps).

This framework will be used by ECLAT. 

Possible further implementations:

* read a JSON file;
* read an XML file;
* a service that stores data in a network-shared database;
* access an LDAP service;
* access an ACAP service.

There might be a tool to generate program argument interpretation (configuration) from a docopt 
file, see <http://docopt.org>. Perhaps with some limitations. 

-----------------------------------------------------------------------------------------------
## Logging, Auditing, and Inter-Process Event Passing

I’m also going to introduce a logging, auditing, and event-passing framework for Ada, at some 
point.

An initial logging implementation will allow events to be appended to a file, using default 
serialisation. There’ll be an extensible tool that can:

* print the events
* dump the events into a YAML file
* copy the events into a database via ODBC

The framework will have specialisations supporting:

* logging exception instances;
* logging plain text messages.

ECLAT will have a hook which will allow unhandled exceptions to be logged.

-----------------------------------------------------------------------------------------------
## Transactions

I’m going to introduce a transaction management infrastructure for Ada, probably based on 
X/Open (OASIS) Distributed Transaction Standard (DTS), also known as ‘XA’. 

This would be based on a two-phase commit protocol that can support distributed transactions. 

-----------------------------------------------------------------------------------------------
## External Program Execution

I’ll also define a framework for the execution of external programs and the control of 
executing instances (processes). 

This framework will identify every accessible external program by an abstract URI -- which 
could be a URN, a URL, or a simile of a file path, or any other arbitrary string -- and the 
realizor’s configuration can associate these URIs with actual executable files (or other 
executable program entities). A special configuration provider can account for the paths in the 
`PATH` environment variable, but all the programs in these paths will only be seen (and thus 
executable) statically at realisation time. 

The idea is that the framework will allow (but not require) all the available programs to be 
included by the realizor into one monolithic executable (potentially a gigantic one, 
presumably). This is to permit some optimisations, and to permit some of the security checks to 
be done statically. 

The framework will use the security model (see below).

The framework will be modelled as if it were a service, but it could be implemented by calling 
into the underlying platform rather than as a service as such. 

-----------------------------------------------------------------------------------------------
## Security Model

Just a few words on this, then.

Each module is associated, by the realizor’s configuration, with a set of tokens called 
‘authorities’. The authorities form a hierarchy (an acyclic tree); there is one ‘top’ 
authority; every other authority has one authority that is its owner. The downward closure of 
an authority and all authorities it owns or has coverage over are called the ‘coverage’ of the 
authority.

The closure of the coverages of the set of authorities associated with a module are called its 
‘ambit’. Every time a module calls a method of a service, it ‘cites’ one authority. The 
realizor checks that the authority is in its ambit; if the authority was cited statically, the 
realizor can perform this check statically. The service can find out what the cited authority 
was for every incoming invocation of any of its methods. 

Objects, called ‘guardians’, in the class of an abstract tagged type declared by the framework, 
can be used by services to check whether a specific action by a specific cited authority 
(subject) on a specific object is permitted or not, and possibly for other qualifications of 
the action (e.g. auditing requirements). 

There will be actual guardians supporting:
 * mandatory security (labels)
 * discretionary security (ACLs)
 * role-based security
 * attribute-based security
 * possibly other security models

To start with, we’ll have a dummy guardian that just says ‘Yes’ to all requests. The important 
thing is to get the framework in place, and encourage all software to have the appropriate 
calls to ascertain and enforce permissions checks. 

