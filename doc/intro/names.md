-----------------------------------------------------------------------------------------------
# Names

..... _entity name_ .....

An entity name can be either a [universal name](#univ) or a [scoped name](#scop). 

Syntactically, an entity name must conform to the same rules as an Ada identifier, except that
it may also contain the `.` period (dot, point, full stop) character with restrictions similar
to the use of the `_` underscore:

 * the name may not start or end with a `.` period; 
 
 * a `.` may not be adjacent to an `_` underscore or another `.` period. 
 
Names are not case-sensitive. 

Please avoid using names beginning with:

    AdaOS.
    Ada.
    System.
    
as these are reserved for use by the [AdaOS Project](../intro/project.md). 











-----------------------------------------------------------------------------------------------
# Universal Names {#univ}

Various entities are primarily identified by a _universal name_. 

The following categories of entities that are entities that have a universal name:

 * [libraries](../eclat/libraries.md)
 
 * [modules](../pxcr/modules.md)
 
 * [module classes](../pxcr/modules.md#class)
 
 * [module elements](../pxcr/modules.md#elem)
 
 * [plugin classes](../pxcr/plugins.md#class)
 
 * [event channels](?????)
 
 * [executable images](?????)

 * memory [segments](../pxcr/memory.md#seg)
 
Each of the above categories of entity implicitly has its own separate namespace for universal names. 

Each name needs to be permanently unique, across the world, within its category. 

?????so it is quite 
possible, for example, for an assembly to have the same name as a module. In fact, it is usual 
for a module to have the same name as its corresponding assembly (this is how the Program Helper 
creates assembly modules).  -- just equate them?

.....



-----------------------------------------------------------------------------------------------
## Scoped Names {#scop}

.....

..... begin with

    System.

The entities that have a scoped name are:

| Entity                                            | Scope                                 |
| ------------------------------------------------- | ------------------------------------- |
| [authorities](?????)                              | [effective system](intro.md#effsys)   |
| [assemblies](../eclat/building.md)                | ?????
| [programs](?????) (including service programs)    | 
| [installations](packaging.md#inst)                | [effective system](intro.md#effsys)   |
| [services](../rts/services.md) (incl svc groups)  | [compartment](../rts/compart.md)      |

 



### Authorities

Within the authorities scope, the name `System.Top` is reserved for the [top
authority](../security/authorities.md#top). 


### Memory Segments



### Assemblies


### Programs



### Services





.....



-----------------------------------------------------------------------------------------------
## Advice on Names

Generally, it is recommended that you choose ECLAT entity names according to the following 
three-tiered pattern: 

    who.project.entity

where:

 * `who` identifies the organisation (or individual) having responsibility for the entity; 
 
 * `project` identifies a larger grouping of related entities within the organisation; 
 
 * `entity` identifies the entity within the project. 


?????### Assemblies and Program Assemblies

?????Since an assembly is always (conceptually) part of a program, it is recommended that a 
?????assembly name is kept simple. 

?????Similarly, an assembly is usually (conceptually) associated one-to-one with an assembly. 
?????In these cases, the assembly is automatically named (with a prefix that is the program's 
?????name, then a `.` dot, and then the assembly's name). 


?????### Module Elements

?????A module element's name, by convention, has an extra  `.call` or `.data` appended, according to 
whether it is a subroutine or a data item. 


-----------------------------------------------------------------------------------------------
## Example Names



### Wayfarer

A completely fictional example of a module element's name might be:

    JNSE.Wayfarer.HPNFX.Get_Status

where 'JNSE' is a space exploration company, 'Wayfarer' is one of their spacecraft, and 
'HPNFX' is one of the science modules on board that spacecraft. 

Of course, in some cases it may be appropriate to have a four-tiered or five-tiered pattern. In 
the above we are using a four-tiered structure, 

?????with the addition of the conventional `.call` 
for a module element that is a subroutine. 

It is conventional to use 'title casing' in entity names, but this is only a convention. The
example name could equally have been either:

    jnse.wayfarer.hpnfx.get_status
    
or:

    JNSE.WAYFARER.HPNFX.GET_STATUS

.....


### .....






-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




