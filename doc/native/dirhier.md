-----------------------------------------------------------------------------------------------
# Native Directory Hierarchy

For the [AdaOS Native](native.md) [platform](../pxcr/targets.md#plat), there is a standard tree
of directories and their members. 

This tree dictates, for almost all named system objects: 

 * the name of each object; 
 
 * the directory in which each object resides. 

For any particular [compartment](../adaos/compart.md), this hierarchy is always rooted at the
compartment owner's home directory. 

The standard hierarchy is named the __Native Directory Hierarchy__ of AdaOS. 

.....

?????The rest of this document describes the objects in the Native Directory Hierarchy belonging to
a specific [principal](../security/security.md#princ). The expression 'the principal' is used
in this document to mean the user or role to which the hierarchy belongs. 

The names of all the members of most of the standard AdaOS directories are case-insensitive.
However, the directories which represents non-AdaOS filesystems (and their inferior
directories) will be case-sensitive if the filesystem is. It is therefore strongly recommended
to use the standard names of objects exactly as presented in this documented, including casing. 


-----------------------------------------------------------------------------------------------
## FHS and Microsoft Windows

The Native Directory Hierarchy is designed to be fully compatible with the [Filesystem Hierarchy
Standard][1] and the conventional names and locations of files and directories in the Microsoft Windows
operating system. 

No AdaOS package installation is permitted to have a _reserved name_. 

A reserved name is one of the root directory names specified by the FHS or that is used by
Microsoft Windows for special purposes. 

The FHS root directories are:

    bin, boot, dev, etc, home, lib, lib<x>, media, mnt
    opt, proc, root, run, sbin, srv, sys, tmp, usr, var

where `<x>` can be anything. 

See [Temporary Files](#tmp) for more about `tmp` within the Native Directory Hierarchy. 

The reserved Windows root directories are: 

    /mnt/<m>

where `<m>` is a single letter (case-insensitive).

The reserved Windows filenames are:

    CON, PRN, AUX, NUL, COM1, COM2, COM3, COM4, COM5, COM6, COM7, COM8, COM9, LPT1, LPT2, LPT3,
    LPT4, LPT5, LPT6, LPT7, LPT8, LPT9, CLOCK

The package manager [Kantan](../kantan/kantan.md) will refuse to allow an installation to be
created that has any of these reserved names. 

[Cygwin][2] reserved root directories are: 

    cygdrive

The idea is that any part of, or the whole of, FHS and Windows can be implemented along with
the Native Directory Hierarchy with no name clashes. Software can make use of files and
directories in the FHS, or the Native Directory Hierarchy, or Windows, or (hypothetically) all
three. 


### Devices

Objects that represent various devices, pseudo-devices, and things that are like devices, are
available as subdirectories of `/dev`. 

The various [services](../services/services.md) which implement these objects automatically add
these members to the directory `/dev`. These objects may sometimes come about from the
configuration of the services, but otherwise there is no need for any explicit configuration to
make them appear. 

.....

Some typical devices are: 

| `con`     | Currently active [console](../native/consoles.md)                              |
| ` ` |  |
| ` ` |  |
| ` ` |  |
| ` ` |  |
| ` ` |  |
| ` ` |  |
| ` ` |  |
| ` ` |  |
| ` ` |  |
| ` ` |  |
| ` ` |  |
| ` ` |  |
| ` ` |  |
| ` ` |  |


### Mount Points and Remote Directories

Filesystems and remote directories (shares) are available as subdirectories of `/mnt`, as are a
variety of other objects, collectively known as _mount points_. 

The various [services](../services/services.md) which implement these objects automatically add
these members to the directory `/mnt`. These objects may sometimes come about from the
configuration of the services, but otherwise there is no need for an 'fstab' (file system
table) file or equivalent. 

A path of the form: 

    //foo/bar/hum

is automatically interpreted as:

    /mnt/foo/bar/hum

As long as the filesytem, remote directory, share, or whatever it is named `foo` is available
as a member of `/mnt`, the `//` prefix can be used. In the NHS, the `//` prefix is just a
shorthand, but its use is not deprecated. 


### Windows Paths

.....

| In Windows        | NHS Equivalent               |
| ----------------- | ---------------------------- |
| `C:`              | `/mnt/c`                     |
| `C:`              | `/cygdrive/c`                |
| `C:\Users\zaphod` | `/mnt/c/Users/zaphod`        |
| `C:\Users\zaphod` | `/cygdrive/c/Users/zaphod`   |
| `C:\Users\zaphod` | `/adaos/users/zaphod`        |
| `\\Bram-X42\Docs` | `//Bram-X42/Docs`            |
| `\\Bram-X42\Docs` | `/mnt/Bram-X42/Docs`         |
| `` | `` |
| `` | `` |
| `` | `` |

Note how any directory directly within the `%SystemDrive%\Users` Windows directory
automatically has a link to it, of the same name, in the NHS `/adaos/users` directory. 

For compatibility with [Cygwin][2], NHS additionally defines the
root directory `cygdrive` as a way to access .....

The directory `/proc/cygdrive` is a link to `/cygdrive`. 

The Ada package `AdaOS.Windows` contains the visible declaration: 

```ada

function To_Native_Path (Windows_Path: in Wide_String) return Path_String;
```

The function `To_Native_Path` converts the given Windows path into the (most directly)
equivalent NHS path. If the Windows path cannot be converted into an NHS path then the
exception `?????` is propagated. 



-----------------------------------------------------------------------------------------------
## File Name Extensions

.....



The general rule is that objects which are not files do not have any file name extension at
all. 




-----------------------------------------------------------------------------------------------
## Ersatz Root

The way files and other (system) objects are organised on different platforms or operating 
systems affects software accessing these files (objects) by path (name). 

On non-AdaOS operating systems, there is always an absolute root directory for files (or one
for each attached storage device). This root is (these roots are) always fixed for all users of
the computer (system). 

On the AdaOS platform, on the other hand, there is no concept of a root directory for a 
computer (system), as such. Instead, every single principal of the system has a home 
directory, and the whole 'world' for any one principal consists of everything inside their 
home directory tree. 

It is possible for objects outside the home tree to be indirectly accessed, by having a link 
to an outside object. However, it is is not possible for the principal to make that link, 
unless they are copying a link they already have. Only a superior principal (a principal above 
them in the hierarchy) could make such a link in the first place. 

A system always has one principal, the top user, who is at the top of the hierarchy of 
principals, and so the home directory of the top user serves as an ersatz root for the system. 

Nevertheless, it is essential to remember that, in AdaOS, an absolute path, when resolved by a 
program running under a specific principal, is always actually relative to the home directory 
of the principal. 

Having said this, the home directory of the [top user](../security/security.md#topuser) is
effectively the system root, because all objects, including all all other principals, are
within it. 


### Comparison with Windows

Under Microsoft Windows (and all CP/M, MP/M, and MS-DOS derived operating systems) there is, in
effect, a separate root directory for each drive or network volume.

The path (including drive) of the system root is always in the environment variable
`SYSTEMROOT`, and the path (including drive) of the currently logged-in user's home directory
is in the environment variable `USERPROFILE`. 

File and directory names in Windows NTFS are case-insensitive (the same as AdaOS). 


### Comparison with Linux

On Unix-based platforms, the system root of the computer has absolute path name `/`, and the 
path of the home directory of a user is an absolute path in the environment variable `HOME`.

Many shells and other programs will resolve the pseudo-name `~` to be the home directory (as if
it were replaced by `$HOME`). For compatibility, AdaOS does the same. 

In many Unix-derived operating systems there is a 'root trap' concept, but it is rather a
bolt-on extra, and the underlying Unix-based infrastructure that has built up over many years
does not comfortably fit it. 

AdaOS Native has this approach ab ovo. The home directory has the path `/`. The system root is
normally inaccessible to all but the top user (for whom it is `/` anyway). Each user (every
principal, in fact) is effectively in a 'root trap'. 

File and directory names in most Linux filesystems are case sensitive (unlike AdaOS Native). 



-----------------------------------------------------------------------------------------------
## Home Directory {#home}

.....

The home directory is expected to (directly) contain the following directories: 

| Name            | Object                               | Members are normally of type      |
| --------------- | ------------------------------------ | --------------------------------- |
| `/tmp`          | Temporary directory                  | `System_Object'Class`             |
| `/adaos/dump`   | Program crash dumps                  | `System_Object'Class`             |
| `/adaos/acct`   | Accounts                             | `System_Account'Class`            |
| `/adaos/land`   | Landing place for downloaded files   | `System_Object'Class`             |
| `/adaos/local`  | System data                          | `System_Object'Class`             |
| `/adaos/roles`  | Roles                                | `Security_Role'Class`             |
| `/adaos/users`  | Users                                | `Security_User'Class`             |
| `/adaos/work`   | Non-package specific original files  | `System_Object'Class`             |

The home directory is expected to contain the following computed link directories (which only
contain links to other objects): 

| Name            | Object                               | Link targets are normally of type |
| --------------- | ------------------------------------ | --------------------------------- |
| `/adaos/cmpt`   | Compartments (running programs)      | `Program_Compartment'Class`       |
| `/adaos/princ`  | Principals                           | `Security_Principal'Class`        |
| `/adaos/prog`   | Available programs                   | `System_Program'Class`            |
| `/adaos/serv`   | Available stock services             | `System_Service'Class`            |

The home directory will contain a directory for every package installation. The name of the
directory will be the name of the installation. For example: 

| Name            | Product                              | Product's Purpose                 |
| --------------- | ------------------------------------ | --------------------------------- |
| `/kantan`       | [Kantan](../kantan/kantan.md)        | Installable packages              |
| `/sotor`        | [Sotor](../sotor/sotor.md)           | File and data backup              |
| `/quis`         | [Quis](../security/quis.md)          | Logging in                        |
| `/` |  |  |
| `/` |  |  |
| `/` |  |  |
| `/` |  |  |
| `/` |  |  |
| `/` |  |  |

The [stock services directory](../services/services.md#stock), `/adaos/serv`, contains .....

| Name         | Service                        | Type                              |
| ------------ | ------------------------------ | --------------------------------- |
| `chan`       | System event broker            | `Event_Broker'Class`              |
| ` ` |  | ` ` |
| ` ` |  | ` ` |
| ` ` |  | ` ` |
| ` ` |  | ` ` |
| ` ` |  | ` ` |
| ` ` |  | ` ` |
| ` ` |  | ` ` |
| ` ` |  | ` ` |
| ` ` |  | ` ` |
| ` ` |  | ` ` |
| ` ` |  | ` ` |
| ` ` |  | ` ` |
| ` ` |  | ` ` |
| ` ` |  | ` ` |
| ` ` |  | ` ` |

.....


Note how all of these paths begin with `/` because the 'root' directory, in AdaOS Native, *is*
the home directory. 



-----------------------------------------------------------------------------------------------
## Temporary Files {#tmp}

The directory `tmp` can be used by (the executional instances of) a compartment to contain 
[temporary files](../objects/paths.md#temp). 



-----------------------------------------------------------------------------------------------
## Dump Directories

The directory `/adaos/dump` can be used by (the executional instances of) a compartment to
write [crash-dump files](../objects/paths.md#dump) into. 



-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## Users, Roles, and Principals

The `/adaos/user` directory contains a member for every direct sub-user of the principal. 

.....

The `/adaos/role` directory contains a member for every (direct) role of the principal. 

.....

The `/adaos/princ` directory is synthetic: it contains a member for every member of both `user`
and `role`. This is possible because it is not permitted for a member of `role` to have the
same name as a member of `user` and vice versa. 

.....




-----------------------------------------------------------------------------------------------
## System Data

The `/adaos/local` directory contains all the objects (files) that are installed .....

It is likely that only the top user will have this directory. Usually, a few administrative
users or roles will have a link (of the same name) to it. 

Within the directory `/adaos/local/boot` is a directory for every boot image suitable for
booting the computer. 

.....






-----------------------------------------------------------------------------------------------
## Landing Pad

The Landing Pad directory `/adaos/land` is the place for downloaded files and files obtained
from external sources. Normally such files are left in place within this directory, but they
should all be considered non-original (can be re-obtained at any time) and thus subject to
being deleted (or emptied and reset) at any moment. 

.....

Downloaded files include downloaded (or otherwise obtained) Kantan package content files .....

.....






-----------------------------------------------------------------------------------------------
## General Work

The General Work directory, named `/adaos/work`, contains original files that are not
specifically associated with any one package installation. 




The work directories (and therefore all the members inside them) have full access permissions 
granted to the principal. 

.....






-----------------------------------------------------------------------------------------------
## Kantan

???????? needed here?

The `/kantan/pkg` directory contains Kantan package content files unwrapped (unzipped) .....


### Packages

The `/kantan/pkg` directory contains Kantan package content files unwrapped (unzipped) .....

The members of this directory will be [package content root directories](?????) of all the unwrapped
packages. 

Each package content root directory's name is the same as the package's canonical name. For example, 
the path of a package named `acme.peakprod.frobdel` would be: 

    /kantan/pkg/acme.peakprod.frobdel

The package content root directories have read-only access permission granted to the principal. 

.....




-----------------------------------------------------------------------------------------------
## Backup

???????? needed here?

.....

Every time Sotor initiates a backup cycle, every package (more precisely, every Sotor plugin) .....

.....




-----------------------------------------------------------------------------------------------
## Quis

???????? needed here?




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
## References

[1]: <https://en.wikipedia.org/wiki/Filesystem_Hierarchy_Standard> 
    "Filesystem Hierarchy Standard"







