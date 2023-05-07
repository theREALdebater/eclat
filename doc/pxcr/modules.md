-----------------------------------------------------------------------------------------------
# Modules

A _module_ normally contains procedural instructions in an architecture-independent _extended 
pseudo-code_ called [PXC](pxc.md). They can contain platform and architecture-specific machine 
code and data, but that is a special case. 

The executable contains only architecture-specific, platform and architecture-specific machine 
code and data, that can run on a computer which fulfils the requirements of the architecture 
and platform. 

A Realizor module is analogous to a traditional module, such as an `.OBJ`, `.DLL`, `.o`, or 
`.so` file. The difference is that a Realizor module (normally) contains only pseudo-code, 
whilst the traditional ones contain only real machine code. 

.....

A module file contains a binary serialisation of the following information:

 * the module's [name](#name); 
 
 * the module's [version](#ver); 
 
 * a set of [module elements](#elem); 

 * 
 
.....



-----------------------------------------------------------------------------------------------
## Module Names {#name}

Every module needs to be uniquely identified ......

[ECLAT Entity Name](../intro/names.md)

......



-----------------------------------------------------------------------------------------------
## Module Versions {#ver}

[version](../intro/versions.md)

.....



-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 








-----------------------------------------------------------------------------------------------
## Module Elements (#elem)

A module can contain the following kinds of _module element_:

 * subroutine (executable)
 
 * constant data (readable)
 
 * data area (read-write)
 
A module element can be _exported_ from its module, and those exports can be _imported_ into 
other modules. 

When a module imports an element from another module, it gets a memory address to (a copy of) 
that element, so it can either execute it (if it is a subroutine) or read it (if it is 
constant data) or access it (read from it or write into it, if it is a data area). 

Every module element is identified by an 

[ECLAT entity name](../intro/names.md), 


which must be 
unique within any [product](../eclat/building.md#prod) that module might (ever) be included in. 

An element that is exported is called an _export_, 

Within the extended pseudo-code (PXC) of a subroutine:

 * a specific instruction, called a _call reference_, can invoke a subroutine element by name; 
 
 * a specific instruction, called a _data reference_, can access (a part of) a data element by
   name. 
 
Collectively, call references and data references are termed _references_. If a reference in a 
certain module refers to (the name of) an export of a different module, the reference is termed 
an _import_. 



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
## Module Classes {#class}

A _module class_ is an [ECLAT entity name](../intro/names.md) that defines a coherent set of 
exports.

For each export, the module class defines:

 * the export's kind (subroutine, constant data, or data area); 
 
 * if the export is data, its structure; 
 
 * if the export is a subroutine, its parameters; 
 
 * the export's [convention](?????); 
 
 * the export's meaning. 
 
.....

Everything that anyone may need to know about a module class must be encapsulated in a _module 
class definition_, or [MCD](mcd.md). 

.....



-----------------------------------------------------------------------------------------------
## Module Dependencies {#deps}

.....
A [module class](#class) defines a set of exports by name.

A module which has all these exports (and those exports all work together in a way that 
accords with the intentions of the class) is said to _fulfil_ that class. 

A module can fulfil two or more module classes, but this is unusual in practice. On the other 
hand, every module must fulfil at least one module class. 

A module can be configured to _require_ one or more module classes. This is said to be a
_requirement_. 

When a module requires a specific module class, it will import at least one of the exports 
defined by the class (and expects that those exports all work together in a way that accords 
with the intentions of the class). 

A requirement can either be _specific_ or _general_.

A specific requirement specifies the module which is to fulfil the requirement. On the other 
hand, a general requirement is to be fulfilled by any module that is able to. 

.....

A module `M1` is _dependent_ on another module `M2` if either:

 * `M1` has a general requirement for module class `C` and `M2` fulfils `C` (and there is no 
   other module in the same image which fulfils `C`, which would be an error); or 
   
 * `M1` has a specific requirement for module class `C` to be fulfilled by `M2` and `M2` does 
   indeed fulfil `C` (it is an error if it doesn't). 

.....

Commands of the [`eclat`](../tools/eclat.md) tool can be used to configure the fulfilments and 
requirements of builds. 

?????Build `B` can be made the current build by executing the command:

?????    build B

?????The output module of the current build can be configured to fulfil module class `C` by 
?????executing the command:

?????    fulfils C


Module `M` can be configured to fulfil module class `C` by putting:

    for M'Fulfils use (C);

    
?????The output module of the current build can be configured with a general requirement for module
????? class `C` by executing the command: 


Module `M` can be configured to have a general rquirement for module class `C` by putting:

    for M'Needs use (C);


?????    needs F
    
?????The output module of the current build can be configured with a specific requirement for 
?????module class `C` to be fulfilled by module `M2` by executing the command:

?????    needs F from M2

.....




When the [module initialisation procedure](#init) of a particular module is executed, it can 
expect only those modules upon which it depends to have already been initialised, ......



the RTS itself to be 
initialised, so the initialisation procedure cannot expect any other programs (including 
services) to be running. 







Any dependency circularity is detected and is an error. 

It is an error for a module to have a requirement for module class `C` and to fulfil `C` at the 
same time (because this would be nonsense). 

It is an error for any module to have a general requirement for module class `C` and  the 
number of other modules fulfilling `C` to be anything other than one. This kind of error is 
often solved by changing the requirement to be specific. 

It is, naturally, an error for any module `M1` to have a specific requirement for module 
class `C` to be fulfilled by module `M2` where module `M2` does not actually fulfil `C`.

.....



-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## System Module {#sys}

The _system module_ is a pseudo-module that is generated by the Realizor as it performs a realisation, but which
is linked into the [excutable image](images.md). It contains the following exports:

 * The [module initialisation procedure array](#mipa); 

 * .....











-----------------------------------------------------------------------------------------------
## Module Initialisation {#init}

Every module may (and usually does) have one special export, its _module initialisation 
procedure_. The name of the export is 

    M.init
    
where `M` is the name of the module. This name cannot be used by any other element. 

If a module does not have this export, initialisation of the module is a null procedure (does
nothing). 

A module initialisation procedure has no parameters. There is a type declaration in the 
package `AdaOS.Modules`: 

    type Initialization_Procedure is access procedure;

This export is not mentioned in any [module class descriptor](mcd.md). 

.....

Within an [executional instance](instances.md), the initialisation procedure of a module should 
be called only once, and it should be called before any of the other subroutine exports of the 
module are called. 

A module initialisation procedure can set up (the values of) one or more global variables for
the use of other subroutines in the module, and it can set up global variables that are
exported. 

.....

### Module Initialisation Procedure Array {#mipa}

The Realizor generates export, the _module initialisation procedure array_ (or _MIPA_), in the
[system module](#sys). 

This export has the module element name `System.Modules`, and is read-only data. 

The following constant is visibly declared in the package `AdaOS.Modules`:

```ada

Module_Inititialization_Procedures: 
   constant array (Positive range <>) of constant Initialization_Procedure
   with 
      Import,
      Convention => PXC,
      External_Name => "system.modules";
```

.....





-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 



For each import, the following information is included:

 * the name (of the import)
 
 * whether it is for a call or data
 
 * an import ID

For each export, the following information is included:

 * the name (of the import)
 
 * whether it is for a call or data
 
 * a segment of PXC constant data or of instruction code

Instruction code can of two types:

 * pseudo-code (PXC)
 
 * native code
 
If it is of native code, then it comprises a set of target IDs, and for each target, a sequence 
of 64-bit words containing binary machine code suitable for the target. 



-----------------------------------------------------------------------------------------------
## Module File Encoding

A module file is a binary file which has a format that is defined on three levels:

 1. At the highest level, the file contains: a _file header_, followed by a series of _file 
    blocks_, followed by a _file trailer_. 
    
 2. Each block comprises: a _block header_, followed by a _payload_, followed by a _block 
    trailer. 
    
 3. Each payload contains data that depends on the type of block. 

The (entire) contents of the file is a sequence of _quadwords_, or _quads_. Each quad is 64 
bits in size, and those bits have an order that is defined by the storage medium and the 
architecture (in practice they are either 'big-endian' or 'little-endian'). 

The current version of the module file format is 1.0. 


### File Header

The file header has a variable size, in quads, that has a minimum of 8. The first 8 quads have 
a format that is defined here (the remainder, if any, is for future versions of the file 
format). 

The defined header format is:

| Quad | Bits    | Name                 | Description
| ---- | ------- | -------------------- | -----------------------------------------------------
|    0 |  0 - 31 | Signature            | Constant value to verify that this is a module file.
|    0 | 32 - 47 | Format_Version       | 
|      | 48 - 63 |                      | 
|      |         |                      | 
|      |         |                      | 
|      |         |                      | 
|      |         |                      | 
|      |         |                      | 
|      |         |                      | 
|      |         |                      | 
|      |         |                      | 
|      |         |                      | 
|      |         |                      | 
|      |         |                      | 
|      |         |                      | 




-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## Generated Modules {#genmod}

Some modules, instead of being .....








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





