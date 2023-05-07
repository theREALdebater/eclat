-----------------------------------------------------------------------------------------------
# Experimental Compiler Library And Tools (ECLAT)

Author: Nick Roberts
Date: May 2021

__ECLAT__ is a compiler for multiple programming languages, targeting multiple instruction set 
architectures and platforms (operating systems). 

ECLAT is in three parts:

* the compiler
* the builder
* the Realizor

The compiler compiles source text into libraries. The builder builds pseudo-code modules from 
libraries. The Realizor `realises` modules into directly executable machine code. 



-----------------------------------------------------------------------------------------------
## Programming Languages

The programming languages currently supported by ECLAT are:

* Ada
* C
* C++

There will hopefully be many more languages supported in the future. 

For C and C++, the emphasis is on flexibility. The primary purpose of supporting these 
languages (at least initially) is to port existing code with minimum effort. The aim is for 
conformity with (in order): the standards for C and C++; POSIX C; GNU C and C++. 

Thankfully, all Ada implementations adhere to the standard very closely, so there is only one 
thing to aim for. On the other hand, maximum compatibility with [GNAT][1] implementation-
defined featues. At some point I'll make a tool that converts a tree of GNAT project files into 
the equivalent ECLAT configuration. 



-----------------------------------------------------------------------------------------------
## Targets

The [targets](../pxcr/targets.md) currently supported are: 

 * AMD64, Microsoft(R) Windows(R) 10 and Windows Server 2016/2019, console
 
More targets will be supported in the future. 



-----------------------------------------------------------------------------------------------
## Effective System {#effsys}

The _effective system_ is the computer system from the point of view of an AdaOS-based program. 

On a hosted platform, the [executable image](../pxcr/images.md) emitted by the Realizor is a
native executable program of the hosted platform, and is called the _host program_. 

On a hosted platform, therefore, the effective system means the process which executes the host
program. 

It is as if this process forms a little microcosm, within which it is possible to have multiple
inner AdaOS programs and almost all the other artefacts of an entire computer system. 

On the AdaOS Native platform, _the effective system_ actually means the whole computer system
itself. 











-----------------------------------------------------------------------------------------------
## How Does It Work?

The compiler compiles source text (plain text files) to add units to an ECLAT library. The
builder builds units in a library (an effective library q.v.) into modules (binary files) 
containing pseudo code that is architecture-independent. The Realizor generates executable 
files, containing native machine code, from the pseudo-code modules. 

Currently I am calling the pseudo-code 'PXC'. 

There is a [command-line program](../tools/eclat.md) that will act as a kind of 'make' program 
but with the ECLAT compiler and builder. There is another [command-line 
program](../tools/pxcr.md) which contains the Realizor. 

There are several saved states, which contain configuration and working data:

 * for the ECLAT service, which contains information needed to compile and build modules; 
 
 * for the Realizor service, which contains the information needed to realise modules into 
   executable images; 

 * for the run time system, which contains information about assemblies; 

 * for the Tethys service, which contains information about programs and service programs. 

Configuration is currently performed by command-line programs. For example, .....

In future, there will be more sophisticated ways of using and maintaining this data. 

The reason why 'ECLAT' mentions *libraries* and tools is because the whole of ECLAT's 
functionality will be exposed as an ECLAT library (itself named `ECLAT`). This will allow 
tools, services, and application programs to have some or all of ECLAT's functionality built in 
without resorting to clunky execution of external programs. 

Ada defines what a library unit is, and what its name is. It is possible for one source file to 
contain multiple library units (but it is not possible for any one library unit to be in more 
than one source file).

For C and C++, ECLAT will build a module in lieu of an object file. Note that the way ECLAT 
works is, in certain ways, different to the standard C/C++ model. There are no object files as 
such, only modules. This is sufficient to enable existing software to be readily ported. Shared 
object files (DLLs) can be linked in by the Realizor, but this has certain security 
consequences.  



-----------------------------------------------------------------------------------------------
## Libraries

.....



-----------------------------------------------------------------------------------------------
## AdaOS

The [AdaOS Project](project.md) ........

.....



.....

The [run time system](../rts/rts.md) libraries accompanying ECLAT provide a wide range of low-
level functionality exposed in a standardised way, as a tree of libraries whose root (library) 
is named `adaos` and whose code is all written in Ada and forms a hierarchy based on the root 
package named `AdaOS`. This tree is called the _RTS tree_.

On [hosted platforms](../pxcr/targets.md#plat), the RTS tree creates a medium-thick binding to 
the underlying host operating system. On the AdaOS Native platform, the RTS tree effectively 
*is* the operating system (executional platform). 

.....



-----------------------------------------------------------------------------------------------
## Debugging and Monitoring

Pseudo-code (PXC) supports the ability to encode debugging information (e.g. symbol data). 

There will also be a general-purpose [monitoring](../eclat/monitoring.md) framework---whereby 
intervening code can be automatically inserted at 'flowpoints'---which will facilitate the 
addition of debugging functions.

ECLAT's exception mechanism will use the debugging information, if available, to enrich 
exception information. 

The monitoring framework also supports a variety of other purposes, for example 
[profiling](../eclat/profiling.md). 



-----------------------------------------------------------------------------------------------
## Optimisation

The Realizor offers three _realization modes_ in which it translates the pseudo-code (PXC) in a 
set of modules into executabble (native) code: _debug mode_; _profiling mode_; _final mode_. 

In debug mode, debugging information and functionality is included and no optimisation is 
performed. In profiling and final mode, all debugging information and functionality is removed 
and maximum optimisation is performed. 

Application programs are  distributed in the form of PXC modules (built in release mode). Each 
platform (operating system) has its own Realizor (service) to convert these into native machine 
code that can directly be executed by the platform. 

The Realizor performs optimisation on the PXC pseudo-code in profiling mode, and automatically 
generates machine code that records execution performance statistics (profiling data). The 
Realizor then periodically re-realises, with improvements based on previously recorded 
statistics. When this cycle reaches the stage where re-realisation produces no further 
improvements, the cycle is automatically stopped and the one last re-realisation is performed 
in final mode. 

In final mode, the machine code that records execution performance statistics is not generated.



-----------------------------------------------------------------------------------------------
## Modules, Shared Code, and Plug-Ins

ECLAT will build �modules� from libraries. Each module is a pseudo-code (PXC) file (with the 
file extension `.pxc`). 

The usual kind of module built will be a �program� module, which corresponds to a main program. 
However, �auxiliary� modules can be built, which expose a set of entrypoints (subprograms) to
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
making or consuming an auxiliary module, or making or consuming an auxiliary execution file, 
and establishing the details. COM may also be directly supported in a similar way. 

The realizor will generate a primary executable (file) by gathering the downward 
closure of all the modules it depends on, directly or indirectly, and translating them all 
together as a whole into native machine code. 

Certain standard modules will be expected to be available, including modules that contain the 
various different versions of run time support code. 

ECLAT will provide two different mechanisms to enable a program that has already been built to 
be extended by code that itself has already been built. One mechanism uses a module that is 
optional, but can be added into the program at realisation time; such a module is called a 
�static plug-in�. The other mechanism allows an auxiliary execution file to be loaded during 
the program�s execution; such a file is called a �dynamic plug-in�. Both mechanisms are based 
on the addition of types within the class of a type already defined in the program. 



-----------------------------------------------------------------------------------------------
## Services

I intend to eventually implement the full Distributed Systems Annex (DSA), with at least one 
implementation of the PCS. See the Ada Reference Manual, Annex E, for details. 

The DSA defines how an Ada program can have multiple �assemblies�. Any one instance of the 
program can run different assemblies on different computers. 

In addition to this, I intend to introduce a framework for intra-program services, and 
an implementation of it. This would allow multiple instances of a program (each running in its 
own process) to interact with a service, which is part of the program (at the source text 
level) but runs in a separate process (potentially on a different computer). 

This framework would include use of the security framework (see below). 

There is also planned a whole big project, named Nyota, for supporting the provision or 
consumption of a native service (based on TCP/IP), web service (e.g. HTTP/SOAP/XML based), 
RESTful web API service (e.g. using JSON), and perhaps other kinds of services. 



-----------------------------------------------------------------------------------------------
## Configuration

I�m going to introduce a configuration framework -- something that standard Ada currently lacks 
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

I�m also going to introduce a logging, auditing, and event-passing framework for Ada, at some 
point.

An initial logging implementation will allow events to be appended to a file, using default 
serialisation. There�ll be an extensible tool that can:

* filter events (selecting only some from all available)
* consolidate events (e.g. summing up a numeric value)
* print events (as filtered and/or consolidated)
* dump the events into a YAML file
* copy the events into a database via ODBC

The framework will have specialisations supporting:

* logging exception instances;
* logging plain text messages.

ECLAT will have a hook which will allow unhandled exceptions to be logged.

-----------------------------------------------------------------------------------------------
## Transactions

I�m going to introduce a transaction management infrastructure for Ada, probably based on 
X/Open (OASIS) Distributed Transaction Standard (DTS), also known as �XA�. 

This would be based on a two-phase commit protocol that can support distributed transactions. 

-----------------------------------------------------------------------------------------------
## External Program Execution

I�ll also define a framework for the execution of external programs and the control of 
executing instances (processes). 

This framework will identify every accessible external program by an abstract URI -- which 
could be a URN, a URL, or a simile of a file path, or any other arbitrary string -- and the 
realizor�s configuration can associate these URIs with actual executable files (or other 
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

Each module is associated, by the realizor�s configuration, with a set of tokens called 
�authorities�. The authorities form a hierarchy (an acyclic tree); there is one �top� 
authority; every other authority has one authority that is its owner. The downward closure of 
an authority and all authorities it owns or has coverage over are called the �coverage� of the 
authority.

The closure of the coverages of the set of authorities associated with a module are called its 
�ambit�. Every time a module calls a method of a service, it �cites� one authority. The 
realizor checks that the authority is in its ambit; if the authority was cited statically, the 
realizor can perform this check statically. The service can find out what the cited authority 
was for every incoming invocation of any of its methods. 

Objects, called �guardians�, in the class of an abstract tagged type declared by the framework, 
can be used by services to check whether a specific action by a specific cited authority 
(subject) on a specific object is permitted or not, and possibly for other qualifications of 
the action (e.g. auditing requirements). 

There will be actual guardians supporting:
 * mandatory security (labels)
 * discretionary security (ACLs)
 * role-based security
 * attribute-based security
 * possibly other security models

To start with, we�ll have a dummy guardian that just says �Yes� to all requests. The important 
thing is to get the framework in place, and encourage all software to have the appropriate 
calls to ascertain and enforce permissions checks. 



-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## Methods and Properties {#meth} {#prop}

In Ada, the equivalent of a class is the [tagged type][3]. 

Types, including tagged types, have a set of _primitive operations_ that are inherited by types
derived from it. 

Throughout this documentation, as a convenient convention, the term _method_ is adopted to mean
a primitive operation, and the term _property_ is adopted to mean a value associated with a
type that one can 'get' from an object of the type and that one can 'set' for an object of the
type. 

The convention that is widely used in Ada is that the _getter_ of a property `P` is a function
named `P`, and the _setter_ of the property is a procedure named `Set_P`. 

For example, for a property named `Height`: 

```ada

function Height (Figure: in Geometric_Figure) return Length_in_Metres;

procedure Set_Height (Figure: in out Geometric_Figure; Value: in Length_in_Metres);
```


????? and C?




A property's value might be a collection (a set, sequence, or other multiplicity of values). In
this case, each value in the collection should be uniquely identified (within the collection)
by a key value or tuple (a combination of two values, three values, four values, etc.), and the
getter and setter of the property have the key as a parameter (or, if its a tuple, that many
parameters), so that individual values in the collection can be accessed. 

For example, supposing we consider a geometric figure to have a collection of sides, and we 
identify each sides by a positive integer, we might have a 'length' property like this: 

```ada

function Length (Figure: in Geometric_Figure; Side: in Positive) return Length_in_Metres;

procedure Set_Length (Figure: in out Geometric_Figure; 
                      Side:   in     Positive; 
                      Value:  in     Length_in_Metres);
```

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
## Plugins {#plugins}

.....




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## References

[1]: <https://www.adacore.com/community> "AdaCore GNAT Community"

[2]: <?????> "AdaOS"

[3]: <?????> "RM 3.? Tagged Types"

