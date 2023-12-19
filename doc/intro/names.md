-----------------------------------------------------------------------------------------------
# Names

..... _entity name_ .....

An entity name can be either a [universal name](#univ) or a [scoped name](#scop). 







-----------------------------------------------------------------------------------------------
## Syntax

Syntactically, an entity name must conform to the following rules:

 * The name may contain letters (in Unicode category Ll, Lm, Lo, Lt, or Lu). 

 * The name must start with a letter (in Unicode category Ll, Lo, Lt, or Lu). 

 * The name may contain `.` periods (full stop, dot, point), and `-` hyphens, and `_`
   underscores, but none of these is allowed to be next to another one, and may not be at the
   beginning or the end of the name. 

For the comparison of two names, differences in upper or lower case are ignored, and hyphens
and underscores are considered equal. 

Please avoid using names beginning with:

    AdaOS.
    Ada.
    System.
    
as these are reserved for use by the [AdaOS Project](../intro/project.md). 



-----------------------------------------------------------------------------------------------
# Universal Names {#univ}

Various entities are primarily identified by a _universal name_. 

The entities that have a universal name are:

 * XML namespaces used by AdaOS; 

 * AdaOS domains; 

 * Objects; 

 * AdaOS Authorities; 

 * AdaOS [event channels](../events/events.md#chan), by convention; 

 * AdaOS Services; 

 * AdaOS Programs; 
 
 * ECLAT [libraries](../eclat/libraries.md); 

 * ECLAT logical segments; 
 
 * PXC [modules](../pxcr/modules.md); 
 
 * PXC [module classes](../pxcr/modules.md#class); 
 
 * PXC [module elements](../pxcr/modules.md#elem); 
 
 * PXC [plugin classes](../pxcr/plugins.md#class); 
 
 * [Executable segment or bootable images](?????); 

 * Kantan packages. 

Each of the above entities implicitly has its own separate sub-namespace for universal names,
and each sub-namespace has its own syntax. 

Each name needs to be permanently unique, across the world, within its category. 

?????so it is quite 
possible, for example, for a partition to have the same name as a module. In fact, it is usual 
for a module to have the same name as its corresponding partition (this is how the Program Helper 
creates partition modules).  -- just equate them?


### Versions of Entities with a Universal Name

Most universal names have a [compatibility version](../intro/versions.md#comp) part, at the
end, in the form: 

    :A.B

where `A` is the major version number, and `B` is the minor version number. 

If two universal names are the same except that they have different compatibility version
values, then they are different universal names. They denote two different versions of what is
(otherwise) the same entity. 

Only domains and objects do not have a version part. 


.....


### Advice on Universal Names

Generally, it is recommended that you choose universal names according to the following
three-tiered pattern: 

    who.project.entity

where:

 * `who` identifies the organisation (or individual) having responsibility for the entity; 
 
 * `project` identifies a larger grouping of related entities within the organisation; 
 
 * `entity` identifies the entity within the project. 


### Organisations and Individuals



????? Do we need this section?


.....




The following prefixes to all universal name are reserved:

|
|
| `adaos.`        | The AdaOS Project
| `sys.`          | .....


-----------------------------------------------------------------------------------------------
## XML Namespaces



    NAME:VER





-----------------------------------------------------------------------------------------------
## Domains



    L1:L2:L3:L4



 * `L1` is the name of the level-1 (the top level) group of domains.

 * `L2` is the name of the level-2 group of domains within the identified level-1 group.

 * `L3` is the name of the level-3 group of domains within the identified level-2 group.

 * `L4` is the name of the domain within the identified level-3 group.







For example, the domain for computers owned and operated by the AdaOS Project for project
purposes are all of the form: 

    AdaOS:Project:Main:NAME

where `NAME` is the name of the individual computer. 



-----------------------------------------------------------------------------------------------
## Objects



    L1:L2:L3:L4:OID

where `L1:L2:L3:L4` identifies the domain of the object, and `OID` is an integer that
represents the value of the object's identifier. 



-----------------------------------------------------------------------------------------------
## Authorities

An authority name is universal, and represents one single authority. Thus authorities are universal. .....

However, the opaque token that is used within an [effective system](../intro/intro.md#effsys)
to identify an authority is only unique within the system. .....

Within an effective system whose (universal) name and version is S, there is always an authority names `S.System.Top` is reserved for the [top
authority](../security/authorities.md#top). 



    L1:L2:L3:L4:AUTH:VER

where `L1:L2:L3:L4` identifies the domain of the authority, `AUTH` is the name of the
authority, and `VER` is the compatibility version of the authority. 

Note that a mapping from the names of authorities to the opaque token values is part of system
configuration. 



-----------------------------------------------------------------------------------------------
## Event Channels



    NAME:VER




-----------------------------------------------------------------------------------------------
## Services



    NAME:VER




-----------------------------------------------------------------------------------------------
## Programs



    NAME:VER




-----------------------------------------------------------------------------------------------
## Libraries



    NAME:VER




-----------------------------------------------------------------------------------------------
## Logical Segments



    NAME:VER




-----------------------------------------------------------------------------------------------
## Modules



    NAME:VER





-----------------------------------------------------------------------------------------------
## Module Classes



    NAME:VER




-----------------------------------------------------------------------------------------------
## Module Elements



    NAME:VER




-----------------------------------------------------------------------------------------------
## Plugin Classes



    NAME:VER




-----------------------------------------------------------------------------------------------
## Executable Images



    TYPE:NAME:VER





-----------------------------------------------------------------------------------------------
## Kantan Packages



    NAME:VER







-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## Scoped Names {#scop}

A _scoped name_ is a name that is only meaningful, and unique, within a specific _scope_.

A scope is an instance of some other entity. 

.....

The entities that have a domain scoped name are:

| Entity                                           | Scope                                   |
| ------------------------------------------------ | --------------------------------------- |
| [partitions](../eclat/building.md)               | ?????
| [products](../eclat/building.md#prod)            | [ECLAT library](../eclat/libraries.md)  |   
| [installations](packaging.md#inst)               | [effective system](intro.md#effsys)     |
| [services](../rts/services.md) (incl svc groups) | [compartment](../rts/compart.md)        |

 



### Memory Segments



### Partitions




### Products




### Programs

.....

For programs that are intended to be used as command-line tools, the name of the program should
be the name of the intended command. 

For example, familiar Unix-based commands, such as `ls`, `cp`, `grep`, `sed` and so on would
all have the same names in AdaOS, if they were implemented as separate programs.

Aside: In fact, they are not implemented as separate programs. They are either built-ins or
added by plugins, for the [AdaOS shell](?????). 

For [service programs](../services/servprog.md), .....



### Services





.....





-----------------------------------------------------------------------------------------------
## 









-----------------------------------------------------------------------------------------------
## URNs

A universal name can be expressed as a [Uniform Resource Name (URN)][1], and should be if it
used in any XML file (regardless of schema). 

Every [universal name](#univ) has a _name prefix_ which identifies the entity of the name. 

| Prefix          | Entities                                   |
| --------------- | ------------------------------------------ |
| `adaos-ns`      | XML namespace used by AdaOS                |
| `adaos-dom`     | AdaOS Domain                               |
| `adaos-oid`     | Object (within an AdaOS domain)            |
| `adaos-auth`    | AdaOS Authority                            |
| `adaos-chan`    | AdaOS event channel                        |
| `adaos-svc`     | AdaOS Service                              |
| `adaos-prg`     | AdaOS Program                              |
| `eclat-lib`     | ECLAT library                              |
| `eclat-seg`     | ECLAT logical segment                      |
| `pxc-mod`       | PXC module                                 |
| `pxc-class`     | PXC module class                           |
| `pxc-elmt`      | PXC module element                         |
| `pxc-plugin`    | PXC plugin class                           |
| `bin`           | Executable segment or bootable image       |
| `kantan-pkg`    | Kantan package                             |
| ` ` |  |
| ` ` |  |

| Prefix    | Entities                                |
| --------- | --------------------------------------- |
| `ns`      | XML namespace used by AdaOS             |
| `dom`     | AdaOS Domain                            |
| `oid`     | Object (within an AdaOS domain)         |
| `auth`    | AdaOS Authority                         |
| `chan`    | AdaOS event channel                     |
| `svc`     | AdaOS Service                           |
| `prg`     | AdaOS Program                           |
| `lib`     | ECLAT library                           |
| `seg`     | ECLAT logical segment                   |
| `mod`     | PXC module                              |
| `class`   | PXC module class                        |
| `elmt`    | PXC module element                      |
| `plugin`  | PXC plugin class                        |
| `bin`     | Executable segment or bootable image    |
| `pkg`     | Kantan package                          |
| ` ` |  |
| ` ` |  |

The prefixes are all conventionally expressed in lower case letters. The prefixes are, however,
case-insensitive. 




The namespace (NID) `adaos` has been [registered][2] with IANA. 

The format of a universal name expressed as 

    urn:adaos:PREFIX:BARE

where `PREFIX` is the prefix in the table above, and `BARE` is the universal name. 

.....




On other operating systems and platforms, file names often have a suffix called a 'file type'
or 'file extension' which serves a similar purpose for files. 



-----------------------------------------------------------------------------------------------
## Example Names



### Wayfarer

A completely fictional example of a module element's name might be:

    JNSE.Wayfarer.HPNFX.Get_Status:1.0

where 'JNSE' is a space exploration company, 'Wayfarer' is one of their spacecraft, and 'HPNFX'
is one of the science modules on board that spacecraft. The compatibility version of the
element is 1.0. 

Of course, in some cases it may be appropriate to have a four-tiered or five-tiered pattern. In
the above we are using a four-tiered structure. 

It is conventional to use 'title casing' in entity names, but this is only a convention.
Similarly, it is conventional to use an `_` underscore to separate words, but this character is
equated to the use of a `-` hyphen. So the example name could equally have been either: 

    jnse.wayfarer.hpnfx.get-status:1.0
    
or:

    JNSE.WAYFARER.HPNFX.GET_STATUS:1.0

.....

The URN of this name could be any of: 

    urn:elmt:JNSE.Wayfarer.HPNFX.Get_Status:1.0
    urn:elmt:jnse.wayfarer.hpnfx.get-status:1.0
    URN:ELMT:JNSE.WAYFARER.HPNFX.GET_STATUS:1.0



### .....






-----------------------------------------------------------------------------------------------
## References

[1]: 
   <https://www.rfc-editor.org/rfc/rfc8141.html> 
   "Uniform Resource Names (URNs)"

[2]: 
   <https://www.iana.org/assignments/urn-namespaces/urn-namespaces.xml> 
   "Uniform Resource Names (URN) Namespaces"



