-----------------------------------------------------------------------------------------------
# Libraries and Compilation




There are two kinds of ECLAT library: 

 * An _Ada ECLAT library_

 * A _C ECLAT library_

Both libraries produce PXC modules, but an Ada library can only contain units arising from the
compilation of Ada source text, whereas a C library can only contain units from the compilation
of C source text. 




Certain basic characteristics remain 





_wrapped_; _unwrapped_.




-----------------------------------------------------------------------------------------------
## Library Units

A _library unit_ is .......

Compiling Ada source text, C source text, and C++ source text, .....


### Ada 




### C 

A _translation unit_ is a source file (usually a file with the `.c` extension) which has had 
all the source text inserted into it from expanding the `#include` directive and has then been 
pre-processed and then compiled. 

Each translation unit, if it compiles successfully, is inserted into the library as a _C 
library unit_. 

A C library unit is like an Ada package or a C++ namespace, but it is definitively neither of 
those. It's name is the full file path of the source file. 

......









### C++













-----------------------------------------------------------------------------------------------
## Unwrapped Libraries

Only an unwrapped library can be changed, by compiling source text which may cause library 
settings or library units to be added or replaced (or dropped). Every time the library is 
changed, its version is updated. 

An unwrapped library is kept as a set of files within one directory. When a (re)compilation 
occurs, a new file is added containing new library units and units that supersede those in 
previously generated files. Thus, the library can be readily reverted to a previous version. 
There are also files that keep an index to the source files, ASIS data, and so on. 






-----------------------------------------------------------------------------------------------
## Wrapped Libraries

A wrapped library is all stored in one file (with the extension `.elw`). It is assigned a 
universally unique name, which is a URN. 

A wrapped library cannot be modified. The [`eclat` command-line tool](../tools/eclat.md) allows 
an unwrapped library to be converted into a wrapped libary. All unused and inaccessible units 
are removed from the wrapped library; all references to the source files are removed. This is 
the only way to create a wrapped library (except for the generation of stub libraries, 
mentioned a little further on). 

Wrapped libraries can be readily distributed as a resource, for example using 
[ALDUS](../aldus/aldus.md). 



-----------------------------------------------------------------------------------------------
## Effective Library

Any library is permitted to depend on one or more wrapped libraries. They, in turn, could 
depend on other wrapped libraries, and so on to form a tree of dependencies. The resulting 
_effective library_ comprises the visible units of all the libraries in this tree, and there 
must not be any name clashes. 

Every dependency specifies the [compatibility version](../intro/versions.md#comp) of the 
wrapped library it depends on. 



-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## Library Name

Every library is identified by a name, which is an [ECLAT entity name](../config/names.md). 

......



-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## Source Files

The source files of a library are defined by a saved state item with the path:

    eclat:
       libraries:
          -
             id: L
             sources:
                S: [P1, P2, ...]
                
    
where `L` is the name of the library and `S` is the name of the programming language. The value 
of this item is a file path pattern list with paths `P1`, `P2`, etc. 

If the path is relative, ......

The programming language must be one of: `ada`; `c`; `cpp`.

The default file path pattern lists for these languages are as follows: 

| Language | `S`    | Default file path pattern list                          |
| -------- | ------ | ------------------------------------------------------- |
| Ada      | `ada`  | `["*.ada", "*.adb", "*.ads"]`                           |
| C        | `c`    | `["*.c", "*.h"]`                                        |
| C++      | `cpp`  | `["*.cpp", "*.hpp", "*.cxx", "*.hxx", "*.cc", "*.hh"]`  |

For example:

    eclat:
       libraries:
          -
             id: acme.greenhouse.common
             sources:
                ada: ["src/**/*.ada"]

This configures the source text files of the library `acme.greenhouse.common` to be all files 
with the extension `.ada` within the directory subtree rooted at the `src` directory (within 

??????the same directory that the configuration file is in). 






A library maintains a list of the (absolute) path of every source file that has 
contributed to it:

 * When the library is first compiled, and when it is recompiled from scratch, the list is 
   deleted, the configured source files are searched for, and the files found are put into a 
   new list. 
   
 * Otherwise, when the library is recompiled, the configured source files are searched for any
   new files not already in the list, and added to it. Files are never removed from the list. 








-----------------------------------------------------------------------------------------------
## Library Directory

The directory in which the library files of a library are stored are defined by a saved state  
item with the path: 

    eclat/libraries(L)/unwrapped_directory
    
where `L` is the name of the library. The value of the item is a directory path. 

When a library is first compiled, if the configured library directory does not already exist it 
is automatically created (and some initial files placed inside it). 

The default value is a directory whose name is the same as the library name, within the 
ECLAT libraries base directory. 



-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## Compilation

There are two _compilation modes_: 

 * from-scratch 
 
 * incremental 
 
......


### From-Scratch Compilation Mode

In from-scratch compilation mode, ......

......


### Incremental Compilation Mode

......




-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## Variants {#variants}

.....

A library can have two or more _variants_. Each different variant is distinguished, within a 
library, by a name that must be a legal Ada simple name. 

There is always one variant, named `Default`, that is the _default variant_ of the library. 

Each variant behaves, in all respects other than those described in this subsection, as if it 
were a completely separate library. 

However, the purpose of having multiple variants in one library (rather then simply having 
actually separate libraries) is that the library can share the source code files for some 
(usually most) of its library units. 

One of the particular purposes of variants is to facilitate [unit testing]()



The variants of a library all have the same version number.





A source text file can be assigned to a variant by the pragma `Library_Variant`. This pragma 
must be the first text in the file, other than white space and comments. 

If a source text file has no `Library_Variant` pragma, it is assumed to be in the `Default` 
variant. It is permitted (but redundant) to explicitly assign a source text file with the 
`Default` variant. 






The configuration pragma `Declare_Library_Variant` declares a variant, by name, of the library. 

The variant `Default` does not need to be declared (and must not be). It always exists. 

For example:

    pragma Declare_Library_Variant (Unit_Testing);





A command-line tool is provided, named [`libutils`](../tools/libutils.md), which can 'split' a 
library. Splitting a library makes separate copies  of the source text file tree, each copy 
corresponding a different variant, as defined by the `Declare_Library_Variant` and 
`Library_Variant` pragmas. Library splitting may be useful for use with Ada compilers other 
than ECLAT. 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 






.....

 * the full paths of all the contributing source text files; 

 * the last-modified date and time of each source text file; 

 * fully qualified name of every library unit; 

 * which source text file contains which library unit's specification and body; 

 * what kind of unit each library unit is (package, procedure, function, task, etc.); 
 
 * for each library unit, whether it is generic or not; 
 
 * ??????

......








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
## Model

The structure of data inside a library file .....






Every declared entity is a generic unit if it is a kind of entity that can be a generic unit: ......





Every variable or parameter is a component of a tagged type .....





Every subprogram, as well as being generic, is a primitive operation of a tagged type ......








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










