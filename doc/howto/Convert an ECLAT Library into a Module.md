-----------------------------------------------------------------------------------------------
# How to Convert an ECLAT Library into a Module



.....




-----------------------------------------------------------------------------------------------
## {#}

There are two levels at which one ECLAT library, X, can make use of another fragment of
software, Y: 

 (a) Y can be in the form of a [contributory library](?????), which X can [include](?????) on.  

 (b) Y can be in the form of a module accompanied by a [stub library](?????). X can depend on the stub
     library, which in turn [imports](?????) a set of subprograms from the module. 

Option (a) is easier. It is therefore recommended to use option (a) if there are no significant
problems in doing so. 

Option (b) is less simple, but it can be necessary sometimes, and sometimes it can have
significant advantages: 

 * You may wish to avoid the overhead that a full contributory library adds to the [building](?????) of
   a library that depends on it. Using a module may reduce build times significantly. 

 * You may wish to make it possible for one of multiple alternative modules to be selected for
   a particular [realisation](?????). 

 * You might need to avoid the danger of having multiple parts of the same [effective library](?????)
   depending on different versions of the same contributory library. 

 * Using a module makes it possible to almost completely separate the tools, processes, and
   computers used to build the implementation (i.e. the module) from the libraries that are to
   depend on the implementation. 



-----------------------------------------------------------------------------------------------
## {#}





-----------------------------------------------------------------------------------------------
## {#}




A _candidate subprogram_ is a subprogram (procedure or function) that is intended to be
implemented in the new module. Every candidate subprogram must be on the surface of the
library. A subprogram is not on the surface of the library it is declared in if nothing
declared outside that library could possibly see (and therefore call) the subprogram. 

An _import completion_ is a body declaration of a subprogram that defines the aspect `Import`
(as `True`) instead of having `is` followed by the implementation. Examples can be found below.
The aspect `External_Name` is often also defined, as the name that the corresponding module
element is to have. 

The _export aspects_ of a subprogram are:

 * `Export`, which must always be defined (as `True`); 

 * `External_Name`, defining the name of the corresponding module element. 






-----------------------------------------------------------------------------------------------
## {#}





-----------------------------------------------------------------------------------------------
## Example {#ex}

The best way to explain the process is by example. 

We're going to assume that the library we wish to convert into a module, developed by the
Faculty of Geological Sciences (FGS) of the Axiomatic Corporation of the Milwaukee Environment
(ACME), is named `ACME.FGS.SDAA`, where SDAA stands for 'Structural Damage Analysis and
Assessment'. 

ACME have been in negotiations with a number of other geological research centres around the
world, and an agreement has been made that they will develop their own libraries that are
compatible with `SDAA`, but have their own implementations. They have formed a small company
named 'GeoNosce' to serve as a forum and publisher for related standards.

In order to allow substitution of one implementation with another at the realisation level, all
parties have agreed to make their implementation available as a compatible module. 

The essence of how to convert the library into a module is:

 1. Create a new library that is a copy of `ACME.FGS.SDAA`, named `ACME.FGS.ACME_SDAA`. 

 2. Within the original library `ACME.FGS.SDAA`, replace every body declaration of a candidate
    subprogram with an export completion. Remove any dependencies on libraries that are no
    longer needed. 

 3. Within the copy library (`ACME.FGS.ACME_SDAA`), add export aspects to every candidate
    subprogram. 

 4. Reconfigure the copy library to build a module. 

The original library `ACME.FGS.SDAA` has now become the _stub library_ for the
module. 

ACME then gift an exact copy of the stub library `ACME.FGS.SDAA`, named `GeoNosce.SDAA`, to
GeoNosce, for use by anyone who might want to build libraries that depend on SDAA. 

The copy library `ACME.FGS.ACME_SDAA` is the library that builds (ACME's implementation of) the
module, and is called the _module library_. 

A library that depends on `ACME.FGS.SDAA`, could be built just the same as when `ACME.FGS.SDAA`
was not a stub library. 

The difference is that, at realisation time, the module (built from the module library
`ACME.FGS.ACME_SDAA`) would have to be configured into the realisation, so that the imports of
the `ACME.FGS.SDAA` stub library can be satisfied by the exports of the module. 

Another company, named 'Stend' let's say, also a member of GeoNosce, could create their own
implementation of the module. The module library that builds the module might be named
`Stend.Stend_SDAA`. 

At realisation time, either the module `ACME.FGS.ACME_SDAA` or `Stend.Stend_SDAA`, or some
other compatible module, could have to be configured into the realisation. 

In time, ACME would change all references to `ACME.FGS.SDAA` to reference `GeoNosce.SDAA`
instead, so that `ACME.FGS.SDAA` could be discarded, and `GeoNosce.SDAA` becomes the stub
library that *all* members of GeoNosce use. 

ECLAT will automatically create a skeletal [module class definition](../pxcr/mcd.md) file for
the stub library. This file will need to be manually updated if there is any change in the
exports from the stub library. It is recommended that the MCD file is filled out with proper
descriptions and other information. This MCD file (or a copy of it) will be needed by the build
configuration of any module library. 



-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}



If a contributory library itself depends upon other (contributory) libraries, this can create
version conflicts that might occasionally be difficult to resolve. 

An example may help to illustrate this. 

Suppose we have a library A that depends on two other libraries B1 and B2. Then suppose library B1 
depends on library C version 1.4, and library B2 depends on library C version 1.5. 

Library A cannot be compiled. ECLAT will complain that the effective library cannot have two
different versions of the same library (library C). 

Let's suppose that the differences between C 1.4 and C 1.5 are not trivial, and that changing
B1 to use C 1.5 or changing B2 to use C 1.4 are both infeasible (with the available time or
resources). 

One possible way to resolve this conundrum is to turn either B1 or B2 into a module. 

Let us assume we will change B1. 

One of the essential conditions that must be fulfilled, for this approach to work, is that the
subprograms (procedures and functions) that will be implemented in the new module are the only
things that depend on C 1.4. By removing them from library B1 (which will become a stub
library), the dependency of B1 on C can also be removed. 







-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}





