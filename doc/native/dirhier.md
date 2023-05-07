-----------------------------------------------------------------------------------------------
# Native Directory Hierarchy

For the [AdaOS Native](native.md) [platform](../pxcr/targets.md#plat), there is a standard tree
of directories and their members. 

This tree dictates, for almost all named system objects: 

 * the name of each object; 
 
 * the directory in which each object resides. 

For any particular [compartment](../adaos/compart.md), this hierarchy is always rooted at the
compartment's home directory. 

The standard hierarchy is named the __Native Directory Hierarchy__ of AdaOS. 

.....

?????The rest of this document describes the objects in the Native Directory Hierarchy belonging to
a specific [principal](../security/security.md#princ). The expression 'the principal' is used
in this document to mean the user or role to which the hierarchy belongs. 

The names of all the members of most of the standard AdaOS directories are case-insensitive.
However, the directories which represents non-AdaOS filesystems (and their inferior
directories) will be case-sensitive if the filesystem is. It is therefore strongly recommended
to use the standard names of objects exactly as presented in this documented, including casing. 


### FHS and Microsoft Windows

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

[Kantan](?????) will refuse to allow an installation to be created that has any of these
reserved names. 

The idea is that any part of, or the whole of, FHS and Windows can be implemented along with
the Native Directory Hierarchy with no name clashes. Software can make use of files and
directories in the FHS, or the Native Directory Hierarchy, or Windows, or (hypothetically) all
three. 


### Windows Paths

.....

| In Windows        | AdaOS Equivalent      |
| ----------------- | --------------------- |
| `C:`              | `/mnt/c`              |
| `C:\Users\zaphod` | `/mnt/c/Users/zaphod` |
| `\\Bram-X42\Docs` | `/mnt/Bram-X42/Docs`  |
| `` | `` |
| `` | `` |
| `` | `` |



### Cygwin

.....

For compatibility with [Cygwin][2], the Native Directory Hierarchy additionally defines the
root directory `cygdrive` as a way to access .....

| In Windows        | AdaOS Equivalent              |
| ----------------- | ----------------------------- |
| `C:`              | `/cygdrive/c`                 |
| `C:\Users\zaphod` | `/cygdrive/c/Users/zaphod`    |



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
## Home Directory

.....

The home directory is expected to (directly) contain the following directories: 

| Name            | Object                               | Members are normally of type      |
| --------------- | ------------------------------------ | --------------------------------- |
| `/adaos/chan`   | System event broker                  | `Event_Broker'Class`              |
| `/adaos/user`   | Users                                | `'Class`   |
| `/adaos/role`   | Roles                                | `'Class`   |
| `/adaos/acct`   | Accounts                             | `System_Account'Class`            |
| `/tmp`          | Temporary directory                  | `System_Object'Class`             |
| `/adaos/work`   | Non-package specific original files  | `System_Object'Class`             |
| `/adaos/land`   | Landing place for downloaded files   | `System_Object'Class`             |
| `/adaos/local`  | System data                          | `System_Object'Class`             |
| `/adaos/pkg`    | unwrapped packages                   | `Object_Directory'Class`          |

The home directory is expected to contain the following computed link directories (which only
contain links to other objects): 

| Name            | Object                               | Link targets are normally of type |
| --------------- | ------------------------------------ | --------------------------------- |
| `/adaos/assem`  | Available executional assemblies     | `Executional_Assembly'Class`      |
| `/adaos/princ`  | Principals                           | `'Class`   |
| `/adaos/prog`   | Available programs                   | `System_Program'Class`            |
| `/adaos/serv`   | Available services                   | `System_Service'Class`            |

The home directory will contain a directory for every package installation. The name of the
directory will be the name of the installation. For example: 

| Name            | Object                               | Members are normally of Type      |
| --------------- | ------------------------------------ | --------------------------------- |
| `/sotor/pend`   | pending (unprocessed) backup files   | `System_Object'Class`             |
| `/`       |  | `'Class` |
| `/`       |  | `'Class` |
| `/`       |  | `'Class` |
| `/`       |  | `'Class` |
| `/`       |  | `'Class` |
| `/`       |  | `'Class` |

.....


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
## Packages

The `/adaos/pkg` directory contains Kantan package content files unwrapped (unzipped) .....

The members of this directory will be [package content root directories](?????) of all the unwrapped
packages. 

Each package content root directory's name is the same as the package's canonical name. 

The package content root directories have read-only access permission granted to the principal. 

.....




-----------------------------------------------------------------------------------------------
## General Work

The General Work directory, named `/adaos/work`, contains original files that are not
specifically associated with any one package installation. 




The work directories (and therefore all the members inside them) have full access permissions 
granted to the principal. 

.....






-----------------------------------------------------------------------------------------------
## Backup

.....

Every time Sotor initiates a backup cycle, every package (more precisely, every Sotor plugin) .....

.....




-----------------------------------------------------------------------------------------------
## Temporary Files {#tmp}

The directory `tmp` can be used to create temporary files .....




Under AdaOS Native, there is no danger of name clashes nor of information leakage between
compartments. 

This is because, under AdaOS Native, all file operations are
[transactional](../database/trans.md). Since all the file operations of one compartment will be
in a separate transaction to those of another compartment, these operations will all be totally
separated from each other. 

Perversely, if the transaction is successfully committed, all the temporary files associated
with it are automatically deleted. However, if the transaction is aborted (including if a
commit fails), the temporary files are all _dumped_.

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







