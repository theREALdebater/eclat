-----------------------------------------------------------------------------------------------
# Command-Line Tool: `eclat`

The main command-line tool provided with ECLAT is named `eclat` (the file name is `eclat.exe` 
on Windows).  

The `eclat` program can perform multiple functions:

 * compile source files into a library;

 * build a module from a library; 
 
 * manage libraries;
 
 * manage the ECLAT Service saved state. 

This tool depends on the [ECLAT Service](../services/eclat.md). 

On a [hosted platform](?????), the ECLAT Service is started up, used, and then shut down as a 
part of the execution of the `eclat` program. This requires that the `ECLAT_SVC` environment 
variable is visible to the `eclat` program and set to the full path name of the ECLAT Service 
saved state file. 

On the AdaOS platform, the ECLAT Service must be started or startable and accessible to the 
(principal running the) `eclat` program.  



-----------------------------------------------------------------------------------------------
## AdaShell

The `eclat` tool hosts the [AdaShell](../../../AdaShell/doc/intro.md) scripting language, and 
adds a significant number of its own commands. Most of the functionality of ECLAT can be 
tapped into using these commands. 

For the remainder of this document, when reference is made to commands, this means the commands 
provided by ECLAT (*not* the host operating system's shell). 

.....





-----------------------------------------------------------------------------------------------
## Current Working Library {#cwl}

The commands of the `eclat` tool almost all make reference to a _current working library_, or 
just _CWL_, .....

The current working library is: 

 1. if the environment variable `ECLAT_CWL` is set, the library whose name is in this 
    environment variable; 
    
 2. ?????

.....







### Fallback Library

If the environment variable `ECLAT_CWL` is not set, or if it is set (exists and is not blank), but its value does 
not match the name of a library known to the ECLAT Service, or the CWL is deleted, then a _fallback_ library is chosen as the CWL.

The way the fallback is chosen is as follows: 

 1. If there is only one library (remaining) `L`, then `L` is chosen; otherwise

 2. If there is exactly one library `L` which has a source text path which includes at least 
    one of the files in the current directory, then `L` is chosen; otherwise 
  
 3. If the (base) name of the current directory matches the name of a library `L`, the `L` is 
    chosen; otherwise
  
 4. .....


.....






-----------------------------------------------------------------------------------------------
## Arguments

When executed without any normal (non-option) arguments, the `eclat` tool goes into 
_interactive mode_. 

In interactive mode, `eclat` intialises itself, which includes starting the [ECLAT 
Service](../services/eclat.md), and then begins AdaShell command-line interaction with the 
user. 

If there is at least one normal argument, then each normal argument is assumed to be the path 
(absolute or relative to the current directory) of an AdaShell script file. The script files 
are read and executed, in argument order, in _script mode_. 

The option `--script` or `-s` interacts with the user ?????


### Interactive Mode

.....

Commands are read from `Standard_Input` using `Get`. There will be, therefore, little or no 
line editing facilities. 

It is recommended that a command of any complexity is put into a text file, using a suitable 
text editor program. 

A file named `F` containing commands can be executed with the command:

    . F


### Script Mode

.....

If any of the script files exits abnormally (with a non-zero exit code), then `eclat` itself 
immediately terminates (any remaining scripts are not run), and `eclat` returns the same exit 
code. Otherwise, after all scripts have finished successfully, `eclat` finishes successfully 
(with exit code 0). 



-----------------------------------------------------------------------------------------------
## Commands

.....



Commands that are described as 'printing' something write text into `Standard_Output`. 

Errors and warnings are written into `Standard_Error`. 

For all of the commands that simply print out some value, a bit of explanatory (decorative) 
text is also output. In script mode, just the bare value is output. 


### Help

    help

Prints a list of all the ECLAT AdaShell commands available, and a one-line description of each 
one. 

.....


### Show Current Working Library

    library
    lib

Prints the name of the [CWL](#cwl).


### Set Current Working Library

    library L
    lib L
    
where `L` is the name of an existing library. 

Sets the [CWL](#cwl) to the library `L`.

......


### Create Library

   create-library L
   mklib L
   
Creates a new, empty library named `L`, and sets the CWL to `L`.

.....


### Delete Library

    delete-library L
    rmlib L
    
Deletes the library `L` .....

.....


### Compile Current Working Library

    compile-from-scratch
    compile
    com

Performs a compilation (or recompilation) of the CWL. 

Unless the `compile-from-scratch` command is used, a [minimal recompilation](#minimal) is 
performed. Otherwise a full compilation is performed. 

......


### Compile Specific Library

    compile-from-scratch `L`
    compile `L`
    com `L`

Performs a compilation (or recompilation) of one or more specific libraries. 

May be followed by the name of one or more libraries. If no library name is given, the 
[CWL](#cwl) is assumed (as the one and only library to be compiled). 

For each library, unless the `compile-from-scratch` command is used, a [minimal 
recompilation](#minimal) is performed. Otherwise a full compilation is performed. 


### List Products

   products
   prods
   
Prints a list of the names of the products of the CWL. 


### Show Current Product

    product
    prod
    
Prints the name of the current product .....


### Set Current Product

    product P
    prod P

Sets the current product to `P`. The CWL must already have a product named `P`. 
    
    
### Create Product

    create-product P
    mkprod P

Creates a new product named `P` in the CWL.

.......


### Delete Product

    delete-product P
    rmprod P

Deletes product `P` from the CWL.

.......


### Build Specific Product

    make P
    
Builds product `P` of the CWL. 

The `make` command may be followed by the name of one or more products. If not, and a CWL is 
set, all the products of the CWL are built. 

The command proceeds as follows:

 1. The CWL is compiled; 
 
 2. If there are no errors then, for each product specified, the product is built. 

?????If no product names are specified and the CWL has no current builds, a 
[default product](?????),
whose name is the same as the library, is created and built. 

?????If one or more product names are specified, for each of those names that does not already 
exist as a product of the CWL, a new product of the CWL is created, as a default build with 
the given name, and then built. 

.......


### Build Current Product

    make

If a CWL is set, all products of the CWL are built. 


### List Modules

    modules
    mods
    
Prints a list of the names of the modules of the current product. 

.....


### Show Current Module

    module
    mod
    
In interactive mode, prints the 

In script mode, prints just the name of the current module (of the current product). 


### Set Current Module

    module M
    mod M
    
Sets the current module (of the current product) to `M`. 

.....


### Add New Module

    module M create
    
Creates a new module named `M` of the current product.

.....


### Delete Module

    module M delete
    
Removes the module named `M` from the current product.

.....


### Create Stub Library for Current Module

    create-stub-library
    mkstub
    
Creates a new library and populates it with source text that can be compiled into a [stub 
library](../eclat/building.md#stublibs) for the current module. 

.....

 1. Build the current product; 
 
 2. If there are no errors, generate the stub library.
 
Generating a stub library comprises:

 1. Create the new library, as with the command `create-library` (see below about the name); 
 
 2. .....

.....


### ``

.....





### `fulfils`





### `needs`







### Add Source Files

    add S N P...

Adds one or more source file specifications to the current library ......

Must be followed with a language and one or more source file specifications.

The language must be one of: `ada`; `c`; `cpp`. (`cpp` stands for C++.)

If `ada` is specified, it is an error for the library to not be an Ada library.

Similarly, if `cpp` is specified, it is an error for the library to not be a C++ library, and 
if `c` is specified, it is an error for the library to not be a C library.

Each source file specification must be a file path pattern. Each pattern is used to search 
for source files; all of the source files found are used to create a new library, which is 
compiled, built, realised, and executed. 

The source files are all added under the name `N`, which must be different to all other source 
file specifications in the same library. 

.....


### Remove Source Files

    remove N
    
Removes all the files in the source file specification associated with the name `N`. 

......


### List Library Information

    list-libraries
    LL

Outputs details of all the libraries. 

May be followed by one or more name patterns .....

?????options May be followed by a _column spec_ argument; if a column 
spec is given, that may then be followed by a _sort spec_ argument.

The column spec argument will be a sequence of column letters:

| Letter | Column                            |
| ------ | --------------------------------- |
| N      | Name                              |
| V      | Current version                   |
| D      | Date/time last compiled/updated   |
| P      | Path to directory or file         |

The sort spec is the same except each column letter may be preceded by either `+`, for 
ascending, or `-` for descending. If a letter is not preceded by `+` or `-`, then ascending is 
assumed. 

Both specs are case-insensitive. The defaults are `NVD` and `+N`. Punctuation (e.g. `,`) may be 
put into the specs, and will be ignored. 

.....


### `versions` or `vers`

May be followed by the name of one or more libraries. If no library name is specified, the 
CWL is assumed. 

For each library, outputs all the versions available of each given library. If the given 
library is wrapped, there will be only one version. If it is unwrapped, only the versions 
compiled since the library was last consolidated (plus the most recently compiled version) 
will be available. 


### `version` or `ver`

Must be followed by one version number. 

May then be followed by the name of a library. If this is omitted, the CWL is assumed. 

Sets the version of the library to the given version number. 

It is an error if the (named) library is a wrapped-only library. 


### `dependencies` or `deps` or `tree`

May be followed by the name of one or more libraries. If no library name is specified, the 
CWL is assumed. 

Outputs all the libraries that the given library 
depends upon, and the version spec for each. For each base library there is also an indication 
of whether that library is itself registered (in the ECLAT Service), and if so what the 
matching version of it is. 

If the sub-command `tree` is used the whole tree of dependencies is output (the dependencies of 
the dependencies, the dependencies of the dependencies of the dependencies, and so on). 


### `revert`

Must be followed by the name of a library and then the target version. 

Deletes all the unit versions that are of a version past the target version, and undeletes the 
unit version that match the target version. Error if the target version cannot be found. 

This has the effect as if all compilations (or any other changes) after the target version had 
never happened. If the library has been consolidated since the target version was created 
(compiled), the target version will not exist (it will have been removed), so this command will 
fail. 


### `consolidate`

Rewrites all the units, stored (in a binary format) in multiple files, into a single file with 
deleted (versions of) units removed, and rewrites the index file correspondingly. 

After many recompilations, a library can end up being stored in a large number of files which 
contain mostly superseded (and deleted) unit versions. This wastage can all be removed by consolidation, at the expense of losing the ability to revert 

.....


### Make Wrapped Library

    wrap

Makes a wrapped library (in one file with the extension `elw`) from the current version of the 
current library. 

.....


### `reference` or `ref`

Must be followed by the names of two libraries, the first is the _current working library_ (_CWL_) and the second is 
the _base library_. 

Makes the CWL include the base library, if it doesn't already. No error if it 
does already. Circularity is detected and results in an error. Similarly, any clashes of unit 
names are detected, and results in an error. 


### `refcheck`

Exactly the same as `reference`, except that no dependency is actually added. This sub-command 
can be used to check if there is any circularity or if there are unit name clashes. 

.....


### `unreference` or `unref`

Must be followed by the names of two libraries, the first is the _current working library_ (_CWL_) and the second is 
the _base library_. 

Makes the CWL cease to include the base library. No error if it doesn't already. 

.....


### ``

.....


### ``

.....


### `rename` 

Must be followed by the name of an existing library and then the new name for it. If the new 
name matches the existing name, does nothing (no error). Otherwise, error if new name matches 
any existing library. 


### `relocate`

Must be followed by the name of an existing library and then the path of a directory for its 
files to be moved to. 

.....

A link is created (or amended) in the default library directory pointing to the new directory. 


### `unrelocate`

Must be followed by the name of an existing library. The library is relocated back into the 
default library directory. If it is in .....


### `copy`

Must be followed by the name of an existing library and then a name for the copy. Error if name 
for the copy matches any existing library. The copy is (initially) located in the default 
library directory. 

The new library is unwrapped and effectively consolidated, as the copy is done by copying the 
units of the most recent version of the library. 

However the version number of the new library is reset (to 0.0.0.0). 


### `copy-into`

.....


### `delete`

Must be followed by the name of an existing library. Deletes all versions of the library, both 
its file or files and its registration in the ECLAT Service. 


### ``

.....


### `create-library`

Creates a new library and registers it in the ECLAT Service ..... 

Must be followed by a name for the new library and then by a path to the library's directory. 

The new library is unwrapped and empty. 

.....

The name may be followed by a path to the library's directory. If this path is omitted, a 
default path .....

.....


### `add-library`

Registers an existing library (file or directory) in the ECLAT Service ..... 

Must be followed by a name for the new library and then by a path to the library's directory. 

.....

The name may be followed by a path to the library's directory (if it is unwrapped) or file (if 
it is wrapped). If this path is omitted, a default path .....

.....






### `remove-library`

Removes a library from registration with the ECLAT Service, without deleting any directories or files. 

.....


### ``

.....


### ``

.....


### ``

.....


### ``

.....


### ``

.....


### ``

.....


### ``

.....


### ``

.....


### ``

.....


### ``

.....


### ``

.....


### ``

.....


### `script-config`

Writes out an `eclat` AdaShell script file that is aimed at recreating the current ECLAT 
configuration for the CWL. 

The script contains a sequence of commands that should, starting from a newly created library 
(which must then be made the CWL, for example by setting the environment variable `ECLAT_CWL`), 
configure the library the same as the current CWL is already configured. 

It is important to understand that such a script cannot be guaranteed to perfectly recreate the configuration, for a number of reasons: 

 * it is possible for the initial (default) configuration of a newly created library changes in the future; 
 
 * .....

.....


### help

.....


### version

.....












The following options are recognised.


### Configuration Files Option

The `--config` option (short form `-c`) takes a parameter that is a list of directory path 
patterns .....


### From Scratch Option

The `--from-scratch` switch option causes the `eclat` program to compile libraries fully, 
rather than the default [minimal compilation](#minimal). 


### Do Not Compile Option

.....


### Do Not Build Option

.....


### Library Option

?????When the `eclat` program is executed to compile source files into a library, the name of the 
library can be supplied as the value of the `--library` option. If this option is omitted, the 
name of the library is assumed to be the name of the current directory. If the current 
directory has no name (for example, because it is the `/` root directory) or its name is not a 
syntactically valid library name, the compilation fails with a fatal error. 




-----------------------------------------------------------------------------------------------
## Minimal Recompilation {#minimal}

By default, ...... _minimal compilation_, ......

..........




-----------------------------------------------------------------------------------------------
## Full Compilation {#full}

Full compilation .....

delete the list of source files 

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









