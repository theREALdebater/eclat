-----------------------------------------------------------------------------------------------
# Paths

A _path_, in general, is a sequence of _nodes_. Each node is usually an identification of a
[system object](objects.md) by _name_, but there are a few special node notations that can
identify an object other than by name. 

For all the nodes except the last node in the sequence, the node is (or should be) an
identification of a [directory](containers.md#dir). A directory is a container of other system
objects. Each object it contains is identified, within the directory, by a unique name. 

For all the nodes except the first one, each node is a member of the node preceding it in the
sequence. 

.....

There are three kinds of path: 

 * a _native object path_
 
 * a _host filesystem path_
 
 * a _universal resource locator_ (or _URL_)


### Native Object Paths

This document primarily describes paths as they relate to the AdaOS Native
[platform](../pxcr/targets.md#plat). In particular, nodes identify system objects (rather than
just files or resources). To be pedantic, nodes identify saved states. 

...........


### Host Filesystem Paths

For a host [platform](../pxcr/targets.md#plat), e.g. Linux or Microsoft Windows, the nodes
almost always identify files. These operating systems have some special files and pseudo-files,
but in essence a path identifies a file. 

...........


### Universal Resource Locators (URLs)

For a URL, a path identifies a [resource](../services/garnerers.md#genrsc), which is like a
file, but a bit more abstract. For example, a resource's content might be generated dynamically
at the moment of GET-ting it. 

The syntax rules for URL paths are slightly different, and defined in a series of [RFCs](?????).

...........




...........




-----------------------------------------------------------------------------------------------
## Node Names {#node}



A _node name_ is a string that uniquely identifies a node within a set of nodes, such as, for
example, the members of a [directory](containers.md#dir). 

.....

In the package `AdaOS`, the following declaration is made: 

```ada

type Node_Name is new Wide_Wide_String;
```

.....

Currently, this type is defined as derived from the type `Standard.Wide_Wide_String`, which in
turn is essentially based on the Universal Character Set (UCS) defined by ISO-10636, which is
itself, in practice, based on the [Unicode](http://www.unicode.org) industry standard. 

However, this type is defined as a separate type (rather than just a subtype), because it may
well make sense in some situations to define it differently. 

See [Path Strings and Syntax](#syn) for more on this. 

There are conversion functions between node names and the standard strings, as well as between
path strings and node names: 

```ada

function To_String (Node: in Node_Name) return String;
function To_Wide_String (Node: in Node_Name) return Wide_String;
function To_Wide_Wide_String (Node: in Node_Name) return Wide_Wide_String;

function To_Node_Name (Value: in String) return Node_Name;
function To_Node_Name (Value: in Wide_String) return Node_Name;
function To_Node_Name (Value: in Wide_Wide_String) return Node_Name;

function To_Node_Name (Path: in Path_String) return Node_Name;
function To_Path_String (Node: in Node_Name) return Path_String;
```

These are a bit wordy but make it easy to deal with node strings in conjunction with path
strings and standard strings. 

.....

There is no (practical) limit on the length of (number of characters in) a node name. 

For the normalisation of names, the only requirement is that 

[Unicode 'KC'](http://www.unicode.org/reports/tr15/)

A node name must comprise at least one character. 

?????The characters must all belong to the Basic 
Multilingual Plain (code points 0 to 655535) of the [Universal Character Set][3]. 

The only characters not allowed are: 

 * the control characters (code points 0 to 31 and 128 to 160), because their effect when being 
   output to display devices, and in other circumstances, could be undesirable; 
   
 * code points FFFE and FFFF in hexadecimal (65534 and 65535), because these have special 
   purposes to do with character set identification and verification; 
 
 * '/' forward slash (or solidus) and `\` backslash, because they are used as path separators. 
 
The characters of a node name must always be specified in [Unicode Normalization Form D][4]. 

For the purposes of comparing two node names, every upper case Roman letter is considered 
equivalent to its lower case counterpart, and the `_` underscore character is considered 
equivalent to the space character (code point 32). 

The `_` underscore character and space character (code point 32) are allowed in a node name,
but:

 * the name must not begin with or end with a space or `_` underscore; 
 
 * a space or `_` underscore is not allowed to be adjacent to another space or `_` underscore. 
 
The `.` dot (or period) character (code point ?????) is allowed in a node name, but:

 * the name must not end with a `.` dot; 
 
 * a `.` dot is not allowed to be adjacent to another dot;  
 
 * the node names `.` and `..` are not allowed, as they have special meanings (and also because 
   they would violate the other rules). 
 
The following characters are allowed, but it is suggested they are never used:

 * `:` colon and `;` semicolon, because they are used as path separators in some environment 
   variables and other places; 
 
 * `<` less than, `>` greater than, `|` vertical bar (or pipe), `?` question mark, `*` 
   asterisk, `'` single quote, and `"` double-quote, because they are used in shell languages
   and their presence in node names can cause confusion and readily causes bugs in scripts.  
   
The following names should not be used as node names, whether followed by a `.` dot and any
extension or not: `CON`; `PRN`; `AUX`; `NUL`; `COM1`; `COM2`; `COM3`; `COM4`; `COM5`; `COM6`;
`COM7`; `COM8`; `COM9`; `LPT1`; `LPT2`; `LPT3`; `LPT4` ; `LPT5`; `LPT6`; `LPT7`; `LPT8`;
`LPT9`. These names were historically used in MS-DOS and related software, but successor
operating systems and platforms sometimes still treat them specially. 

It is inadvisable to use a name that is a single letter or begins with a digit or a `-` hyphen.
These may be treated specially by some kinds of software. 



-----------------------------------------------------------------------------------------------
## Path Strings and Syntax {#syn}



.....

A path can be expressed as a single string, which is called a _path string_, in which the nodes
are all present, separated from each other by a specific syntax. 

.....

In the package `AdaOS`, the following declaration is made: 

```ada

type Path_String is new Wide_Wide_String;
```

Currently, this type is defined as derived from the type `Standard.Wide_Wide_String`, which in
turn is essentially based on the Universal Character Set (UCS) defined by ISO-10636, which is
itself, in practice, based on the [Unicode](http://www.unicode.org) industry standard. 

However, this type is defined as a separate type (rather than just a subtype), because it may
well make sense in some situations to define it differently. 

For example, some deployments that use AdaOS might be to (by today's standards) small computers
with limited memory. It may be much more appropriate for `Path_String` and `Node_String`, see
[Node Names](#name), to be defined as the smaller `Standard.String`. In practice, typically,
`Standard.String` will be 8 bits in size, whereas `Standard.Wide_Wide_String` will be 32. 

.....

There are conversion functions between path strings and the standard strings: 

```ada

function To_String (Path: in Path_String) return String;
function To_Wide_String (Path: in Path_String) return Wide_String;
function To_Wide_Wide_String (Path: in Path_String) return Wide_Wide_String;

function To_Path_String (Value: in String) return Path_String;
function To_Path_String (Value: in Wide_String) return Path_String;
function To_Path_String (Value: in Wide_Wide_String) return Path_String;
```

These are a bit wordy but make it easy to deal with standard strings in conjunction with path
strings. 


### Path Separators {#pathsep}

A _path separator_ is 

.....

the `"\"` backslash character on Windows (and MS-DOS and CP/M) or the
`"/"` solidus (or forward slash) on nearly all other platforms. It is usually a single
character, but it might be two or more. 





.....


### Path Vectors {#vect}

A _path vector_ is a vector (a dynamically resizable array) of node names that is a way of
representing a path. 

Path vectors are often used as a convenient way of processing paths. 

The following package is visibly declared in the package `AdaOS`:

```ada

package Path_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, Node_Name);
```

The type `Path_Vectors.Vector` represents a path expressed as a sequence of 
[node names](#node). 

.....

There is no (practical) limit on the length of (number of nodes in) a path. 

.....

The following functions are visibly declared in the package `AdaOS`:

```ada

function To_Vector (Path:      in Path_String; 
                    Separator: in Path_String := "/") return Path_Vectors.Vector;

function To_Path_String (Vector:    in Path_Vectors.Vector; 
                         Separator: in Path_String := "/") return Path_String;
```

The function `To_Vector` divides up any path, as a string, into a sequence of its constituent
node names. No attempt is made to interpret or transform any of the node names in the path,
except that each node name is trimmed of spaces at both ends. An empty string returns a vector
with one node name in it; that node name will be the empty string. More generally, an empty
node name (that has no characters) will produce a node name in the vector; that node name will
be the empty string. 

The function `To_Path_String` concatenates the node names in a path vector to form a single
path string. An empty vector returns an empty string. 

For both these functions, the `Separator` can be specified, but defaults to `"/"` as per the
norm for path syntax. The separator is a string, so it can be longer than just one character if
needed. The separator is not permitted to be a space or have any spaces in it. 

.....



-----------------------------------------------------------------------------------------------
## Absolute and Relative Paths {#absrel}

There is a long-standing concept, popularised by the Unix operating system, of _absolute 
paths_ and _relative paths_. 

These concepts are tied to the concepts of: there being a single _root directory_, which never
changes, for an entire file (or object) hierarchy; there being a 
[current working directory](#cwd) or _CWD_, which is (the root directory itself or) one of
the directories in the root directory's hierarchy, and can change dynamically. 

An absolute path is a path that is relative to the root directory. 

A relative path is a path that is relative to the CWD. 

A path is marked as absolute by beginning with a path separator character. The path is relative
otherwise. 

For example, this is an absolute path:

    /foo/bar/hum

Whereas this is a relative path:

    bar/hum



-----------------------------------------------------------------------------------------------
## URLs {#url}

The syntax for a universal resource locator is .....

However, the [URL garnerer](../services/garnerers.md#url) has a slightly more lax syntax:

 * the space character is permitted in a URL, in which case the resolver itself effectively
   translates it into the `%20` form that it should have; 

 * .....



-----------------------------------------------------------------------------------------------
## Host Filesystem Paths

The syntax for a host file system path is defined by the host platform or operating system. 

.........



-----------------------------------------------------------------------------------------------
## Path Mappings

.....


### URLs and Native Object Paths

?????There is a mapping to and from URLs that have the `file:` schema (prefix) and a native 
 path . 

On the AdaOS Native [platform](../pxcr/targets.md#plat), the URL schema, `file` .....

...........

In fact, an AdaOS Native path prefixed with nothing will be similar to the same path prefixed
with: 

    file:/
    
Note that these prefixes are always case-insensitive, as is any AdaOS Native path.


### Host Filesystem and Compartment Member Paths



?????we don't have compartment member names any more



.........

There is always a one-to-one mapping between any absolute host filesystem path and an
equivalent compartment member path. 

The stock compartment member `host` is always case-insensitive. For the the remainder of the 
equivalent compartment member path, a name is case sensitive or insensitive according to 
whether the corresponding name in the host filesystem path is. 

.........



### Backslash

The `\` backslash character in any path is always automatically converted into a `/` forward
slash (oblique, solidus) character. 

No escape mechanism is provided for this substitution, per se, but in a URL path the form

    %5C

could be used. 



-----------------------------------------------------------------------------------------------
## Environment Variable Substitutions

.........



    ${V}

or, alternatively:

    %V%

will be replaced by the value of the environment variable `V`, 

The first form (`${V}`) is preferred and recommended. 

A `%%` will be replaced by a single `%` and a `$$` will be replaced by a single `$` sign. These
substitutions allow the `%V%` and `${V}` substitutions to be prevented. 

........




After environment variables have been substituted by their values, then all other mappings (as
described in this section) are subsequently applied. 

.....




-----------------------------------------------------------------------------------------------
## Current Working Directory {#cwd}

A [compartment](../adaos/compart.md#cwd) maintains a property `Current_Directory` which is its
_current working directory_, or _CWD_. 

This is an absolute path of a [directory](../objects/containers.md#dir) to which the
compartment has access [permission](../security/security.md#perm). 

Any use by the (program executing within the) compartment of a 
[relative path](../objects/paths.md#absrel) is a way to specify a path relative to the CWD, .....

.....




### Shell Commands

[Allegra](?????) defines commands related to the CWD and the working directory
stack. 

Allegra (not the compartment) maintains a _working directory stack_, which is a vector (a
dynamically resizing array) of the saved paths of working directories. 

Here are the main commands, and a brief description of what they do: 

    pwd
    
Prints the absolute path of the CWD onto the current output. 

    cd P

Changes the CWD to path `P` (absolute or relative). 

    push P
    
Pushes (appends) the current path of the CWD onto (end of) the working directory stack, and
then changes the CWD to path `P`. 

    pop
    
Pops (fetches and then deletes) a path off the top (end) of the working directory stack and
changes the CWD to that path. 
    







### Ada

.....

```ada
with Ada.Text_IO, AdaOS.Instances;
use  Ada.Text_IO, AdaOS.Instances;
procedure Main
is
   CWD: access Object_Directory := Task_Instance.Compartment.Current_Directory;
begin
   CWD.Engage;
   Put_Line (CWD.Path);
   CWD.Disengage;
exception
   when others =>
      if CWD /= null then CWD.Disengage; end if;
      raise;
end;
```

.....

```ada
with Ada.Text_IO, Ada.Directories;
procedure Main
is
   Ada.Text_IO.Put_Line (Ada.Directories.Current_Directory);
end;
```



.....

```ada



```

.....

```ada



```









-----------------------------------------------------------------------------------------------
## Temporary Files Directory {#temp}

The environment variable `TMPDIR` contains the full, absolute path of the _temporary files
directory_. 

The environment variable `TMPDIR` contains the [absolute full path](../objects/paths.md) of a
directory that can be used to create named system objects. 

When this environment variable is set, the temporary directory is changed to be the directory
indicated by resolving the value to which the variable is set. If the value cannot be resolved
to an object that is a directory, and to which the compartment has full permission, the
exception `AdaOS.Usage_Error` is propagated (and the variable is not changed). 

The environment variables `TEMP`, `TEMPDIR`, and `TMP` are links to `TMPDIR` and so are an an
alias of it. Any of these variables can be retrieved and set. When any one is set to a
particular value, all will be automatically set to that value. 

On a [hosted platform](../pxcr/targets.md#plat), what the .....

The value of this variable is typically:

| Platform        | Directory                               |
| --------------- | --------------------------------------- |
| Windows         | `C:\Users\%USER%\AppData\Roaming`       |
| Linux/FHS       | `/tmp`                                  |
| AdaOS Native    | `/tmp`                                  |

where `%USER%` is the value of the environment variable `USER`, and `${HOME}` is the value of
the environment variable `HOME`. These values may vary. 


### Function `New_Temporary_Directory`

For convenience, the package `AdaOS.Objects` contains the following visible
declarations: 

```ada
function New_Temporary_Directory (
   Ambit:       in     Security.Security_Ambit;
   Prefix:      in     Wide_Wide_String := "";
   Controller:  in out Transaction_Controller'Class := Task_Transaction;
   Compartment: in out Program_Compartment'Class := Task_Instance.Compartment.all)
return
   not null access Object_Directory'Class;

function New_Temporary_Directory (
   Prefix:      in     Wide_Wide_String := "";
   Controller:  in out Transaction_Controller'Class := Task_Transaction;
   Compartment: in out Program_Compartment'Class := Task_Instance.Compartment.all)
return
   not null access Object_Directory'Class;
```

The function `New_Temporary_Directory` creates a new general-purpose directory object and
returns (an access value referencing) the new object. 

Full permissions on the new object are granted to all the authorities in the given `Ambit`. 

The overloading without an `Ambit` parameter assumes the ambit of the given `Compartment`,
which itself has a default of the calling task's compartment. 

The new directory object must be engaged before any of its other operations can be carried out,
and it *MUST* be disengaged afterwards. 

Each new directory is a member of the temporary files director, with a randomly generated
name---prefixed by the given `Prefix`---that is guaranteed to be unique within the temporary
files directory. 


### Function `Create_Temporary_Directory`

For (even more :-) convenience, the non-standard extension package `Ada.Directories.Temporary`
contains the following visible declarations: 

```ada
function Create_Temporary_Directory (Prefix: in String := "") return String;
function Create_Temporary_Directory (Prefix: in Wide_String := "") return Wide_String;
function Create_Temporary_Directory (Prefix: in Wide_Wide_String := "") return Wide_Wide_String;
```

The function `Create_Temporary_Directory` creates a new general-purpose directory and returns
its full absolute path.

Internally, this function calls `AdaOS.Objects.Create_Temporary_Directory` (the overloading
without the `Ambit` parameter) with all the defaults for its parameters. 


### Deletion of Temporary Directories

Whichever part of a program it is that creates a temporary directory should delete the
directory when finished with it. 

If the creation of the directory is under the control of a [transaction](../intro/trans.md),
and the transaction is abandoned, then the directory will be automatically deleted. 

Otherwise, the program needs to ensure that the directory gets deleted in the case of an
exception truncating normal execution. 


### Purging

The AdaOS system maintenance Kronos job, loads the Allegra script
`/adaos/local/system-maintenance.allegra`, and runs its `perform-system-maintenance` procedure
regularly (every 5 minutes, by default). 

By default, the `perform-system-maintenance` procedure calls a procedure named
`purge-temp-directory`, which by default is defined as follows:

```allegra

procedure 'purge-temp-directory' is
   for f in (list "/tmp/*") loop
      if (((f) last-modified) < ((clock) - (1 * (seconds-per-day)))) then
         delete (f)
      end if
   end loop
end procedure
```

This version of the procedure simply deletes any subdirectory that is older than one day. 

You can modify this procedure (indeed, the whole script) to suit your preferences or
requirements. Whenever `/adaos/local/system-maintenance.allegra` is modified, Kronos
automatically reloads it. 



-----------------------------------------------------------------------------------------------
### Crash-Dump Files Directory {#dump}

.....


### Purging

The AdaOS system maintenance Kronos job, loads the Allegra script
`/adaos/local/system-maintenance.allegra`, and runs its `perform-system-maintenance` procedure
regularly (every 5 minutes, by default). 

By default, the `perform-system-maintenance` procedure calls a procedure named
`purge-dump-directory`, which by default is defined as follows:

```allegra

procedure 'purge-dump-directory' is
   for f in (list "/adaos/dump/*") loop
      if (((f) last-modified) < ((clock) - (7 * (seconds-per-day)))) then
         delete (f)
      end if
   end loop
end procedure
```

This version of the procedure simply deletes any subdirectory that is older than one week. 

You can modify this procedure (indeed, the whole script) to suit your preferences or
requirements. Whenever `/adaos/local/system-maintenance.allegra` is modified, Kronos
automatically reloads it. 



-----------------------------------------------------------------------------------------------
## Home Directory {#home}

Every [principal](../security/security.md#princ) has a directory associated with it (or him or 
her) called the principal's _home directory_, which contains all (or nearly all) the data 
associated with that principal. 


### Linux

On the Linux platform, the environment variable `HOME` contains the full absolute path of the
home directory of the current user. 


### Windows

On the Microsoft Windows platform, the environment variables `HOMEDRIVE` and `HOMEPATH` are
used instead of `HOME`. `HOMEDRIVE` contains the drive letter of the home directory, followed
by a `:` colon character. `HOMEPATH` contains the remainder of the absolute path of the home
directory (including the initial `\` backslash character). 

......


### AdaOS Native

On the AdaOS Native platform, the environment variable `HOME` contains the full absolute path
of the home directory of the owner of the calling task's [compartment](../adaos/compart.md). 

It should be noted that the value of `HOME` will always be `/`, in reality. 











### ?????

[Program Shared Directory](#progshar)

[Module Shared Directory](#modshar)












-----------------------------------------------------------------------------------------------
## System Root Directory {#sysroot}




### Linux



### Windows

There is a specially declared abstract type representing compartments of the 
[Windows platform](?????) .....

The package `AdaOS.Compartments.Microsoft_Windows` includes the following visible declarations: 

```ada

type Windows_Compartment is abstract limited new Program_Compartment with private;

type Storage_Drive_Letter is new Character range 'A' .. 'Z';

function System_Drive (Compartment: in Windows_Compartment) 
return 
   Storage_Drive_Letter is abstract;

function Current_Drive (Compartment: in Windows_Compartment) 
return 
   Storage_Drive_Letter is abstract;

.....

function Root_Directory (Compartment: in Windows_Compartment;
                         Drive:       in Storage_Drive_Letter) 
return 
   access Object_Directory'Class;

overriding
function Root_Directory (Compartment: in Windows_Compartment) 
return 
   access Object_Directory'Class;
```

The function `Root_Directory` has an overloading that takes an in-parameter, `Drive`, indicating the
drive ..... The overloading without the `Drive` parameter assumes the system drive. 


### AdaOS Native

..... the system root directory is always the same object as the home directory .....







-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## Installation Directories {#instdir}

..... [package installation](#inst) .....

.....

These locations are stored in [environment variables](../adaos/envvars.md), so that programs do
not need to have the locations hard coded. Programs should normally use the values in the
environment variables, and not make assumptions. 


### Directory for Package Installation specific data

The environment variable `APPDATA` contains the full, absolute path of the directory for a
package installation to [store files](../kantan/kantan.md#work). 


### Directory for Package Installation data specific to System

The variable `LOCALAPPDATA` contains the full, absolute path of the directory for a package
installation to store files [specific to the installation and a computer
system](../kantan/kantan.md#local). 


### Directory for Package Installation Data

The variable `PROGRAMDATA` contains the full, absolute path of the directory for files to be
stored which are [specific to a package, but not specific to any installation of the
package](../kantan/kantan.md#data). 



-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 
















-----------------------------------------------------------------------------------------------
## Examples

.....


### Example 1

The URL:

    file:/host/mnt/C/Program Files/Mozilla Firefox/update-settings.ini

is the exact equivalent of the component member path:

    /host/mnt/C/Program Files/Mozilla Firefox/update-settings.ini
    
and this is the exact equivalent of the Windows host filesystem path:

    C:\Program Files\Mozilla Firefox\update-settings.ini

Note that the URL could have been:

    file://localhost/host/mnt/C/Program Files/Mozilla Firefox/update-settings.ini
    
or:

    file:///host/mnt/C/Program Files/Mozilla Firefox/update-settings.ini

If the computer's host name were `dunroamin.abc.xy`, the URL could have been:

    file://dunroamin.abc.xy/host/mnt/C/Program Files/Mozilla Firefox/update-settings.ini

It could also have been:

    file://dunroamin.abc.xy/host/mnt/C/Program%20Files/Mozilla%20Firefox/update-settings.ini

which conforms with the requirements of the RFCs for a URI. Spaces are permitted in a URL 
being interpreted by the resource resolver service. 




?????

### Example 2

The URL:

    file:/cmpt/top

is the exact equivalent of the component member path:

    /cmpt/top
    
and has no equivalent host filesystem path. 





    
### Example 3

The URL: 

    http://www.ada-auth.org/standards/rm12_w_tc1/RM.pdf
    
will fetch a file (a PDF) from the Ada Information Clearinghouse website, and has no equivalent
component member path and (probably) has no equivalent host filesystem path. 

Note that the resource resolver service does not itself have the ability to download files 
using the HTTP protocol, or any [URI scheme][2] other than the `file` scheme. In this example, 
the resolver wold have had to had the `http` scheme registered. Whichever service registered 
that scheme would be used by the resolver to actually fetch the file. 



-----------------------------------------------------------------------------------------------
## Host Filesystems {#hostfs}

On a [hosted platform](rts.md#platforms), the [compartment](../rts/compart.md) of an 
[executional instance](instances.md) always has one stock member which represents the _host 
filesystem_ of the host operating system, named `host`. 


### Microsoft Windows

On the Microsoft Windows platform, `host` is a directory containing:

 * the mapped drives, `C:`, `D:`, etc.
 
 * connected network volumes
 
These are all members of a directory named `mnt` (an abbreviation of 'mount', because, in the 
terminology of many older operating systems, it contains 'mount points' for file storage 
volumes). 

The drives and volumes are mapped as follows (where `p` is a path):

 * a local path `d:\p` is mapped to `/host/mnt/d/p` (where `d` is any drive letter from `A` to `Z`)
 
 * a UNC path `\\v\p` or `\\?\v\p` is mapped to `/host/mnt/v/p` (where `v` is a network volume name)
 
 * a path beginning `\\.\z` is mapped to `/host/dev/z` (where `z` is the name of a Windows device)
 
For example:

| Windows Host Filename Path  | Environment Object Path        |
| --------------------------- | ------------------------------ |
| `C:`                        | `/host/mnt/C`                  |
| `D:\Wimble`                 | `/host/mnt/D/Wimble`           |
| `\\Foobar`                  | `/host/mnt/Foobar`             |
| `\\HUM\bug`                 | `/host/mnt/HUM/bug`            |
| `\\?\HUM\bug`               | `/host/mnt/HUM/bug`            |
| `\\.\LPT3`                  | `/host/dev/LPT3`               |
| `\\.\PhysicalDisk2`         | `/host/dev/PhysicalDisk2`      |
| `\\.\CdRom1`                | `/host/dev/CdRom1`             |
| ``     | ``         |
| ``     | ``         |
| ``     | ``         |
| ``     | ``         |





### Linux
 
On the Linux platforms, `host` is a directory whose contents match the [File Hierarchy 
Standard][1] version 2.3. 

.......

For example:

| Linux Host Filename Path    | Environment Object Path        |
| --------------------------- | ------------------------------ |
| /usr/bin/ls                 | /host/usr/bin/ls


### AdaOS Native

For the AdaOS Native platform, there is no host filesystem. Instead, there is only the home
directory, which is the effective root for an executing program (a compartment) and represents
the whole world (of external entities) to the program. 



..........





-----------------------------------------------------------------------------------------------
## 



-----------------------------------------------------------------------------------------------
## ??????

On [hosted platforms](../pxcr/targets.md#plat), programs must assume that files (and other 
objects) accessed by path (name) are organised differently to AdaOS. Programs may therefore 
need to have mechanisms enabling the paths to be configured. Possible mechanisms include: 
putting paths in [environment variables](../rts/envvars.md); putting (all of or parts of) the 
paths into [saved states](objects.md#state), .....

It is desirable to avoid the over-proliferation of environment variables, since the mechanisms 
to organise them tend to be very simple, and they all have to be copied (in some way) every 
time a new [compartment](../rts/compart.md) is created. 

Saved states have many advantages, but they have the disadvantage that (in general) special 
tools are needed to retrieve and change values. 





???????For ease of software portability, a technique that programs can use is to refer to paths within 
a directory named `adaos` within the principal/home directory, which actually acts as the root 
of an AdaOS Directory Hierarchy on both AdaOS and non-AdaOS platforms. On hosted platforms, 
many of the members within it may need to be links to where the corresponding objects really 
exist, and certain members are likely to be omitted because they are irrelevant or cannot be 
supported. 



-----------------------------------------------------------------------------------------------
## ECLAT Installation Locations

......

The _system installer_ is the first thing that needs to be run/installed in order to start 
installing an ECLAT development system onto a computer. 

[Kantan](../kantan/kantan.md)

......


### Windows

On Windows, the system installer is a Windows Installer Package file, which has the `.msi` file 
extension. 

e.g. `adaos-pxcr-win-amd64-1.0.23.8097.msi`

When installed, the system installer does the following: 

 1. creates the folder `%APPDATA%\adaos`, and extracts all the installation files into it; 
 
 2. appends `;%APPDATA%\adaos\bin` to the `PATH` environment variable; 
 
 3. creates the environment variable `ADAOS` with the value `%APPDATA%\adaos`; 

 4. realises Kantan (to put `kantan.exe` into the `bin` directory); 
 
 5. installs the ECLAT package (to create and populate the directory `static\adaos-eclat`); 
 
 6. .......


......

The files extracted, in step 1, into the `adaos` directory are:

    bin\pxcr.exe
    static\adaos-kantan-1.0.65.1032\adaos-kantan-1.0.mod
    landing\adaos.eclat.pkg,1.0.17.40956,kantan-pkg.zip
    .....



......


### Linux: dpkg (Debian and derivatives)

The source package description file ..... `??????.dsc` ........

The source package tarball file ..... `adaos-eclat_1.0.orig.tar.gz`  ......

The binary packages ..... `??????.deb` ........


### Linux: rpm (openSUSE)

...... `???????.rpm` .......


### Linux: rpm (CentOS)

...... `???????.rpm` .......


### Linux: pacman (Arch Linux)



### Linux: Portage (Gentoo)






.....





### AdaOS Native












-----------------------------------------------------------------------------------------------
## 



-----------------------------------------------------------------------------------------------
# References

[1]: <https://datatracker.ietf.org/doc/html/rfc8089> "RFC 8089: The "file" URI Scheme"

[2]: <https://www.iana.org/assignments/uri-schemes/uri-schemes.xhtml> "Uniform Resource 
     Identifier (URI) Schemes"
     
[3]: <https://www.iso.org/standard/39921.html> "International Organization for Standardization, 
     'Information Technology - Universal Multiple-Octet Coded Character Set (UCS)', ISO/IEC 
     10646:2003, December 2003."

[4]: <http://www.unicode.org/reports/tr15/> "Unicode Normalization Forms"