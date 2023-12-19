-----------------------------------------------------------------------------------------------
# Files

Within the AdaOS world, a _stored file_ (or just _file_) is a 
[system object](../objects/objects.md) that represents a (conceptual or actual) storage place 
where a piece of data can be stored and later retrieved.

The term 'stored file' corresponds to the concept of an 'external file' in the Ada language
standard. In the AdaOS documentation, the word 'file' refers to a stored file, and the term
_Ada file_ is used for a 'file' in the terminology of the Ada language. 

.....




-----------------------------------------------------------------------------------------------
## Filesystems
 
A _filesystem_ is the technology (generally, a piece or family of software) for efficiently 
operating with files: saving data into them; retrieving data from them; organising files, 
typically by name arranged into some hierarchy of namespaces (directories). 

Traditionally, the data stored in one file is large; decades ago 'large' would mean many 
kilobytes, nowadays, 'large' can mean gigabytes, soon terabytes. Filesystems are designed with 
this assumption of large files in mind. 

However, the AdaOS concept of a file does not assume that. Some files will be small (a few 
bytes), whilst others large (many terabytes at least). 

Linux, and some other less well-known operating systems, has pioneered the concept of one 
computer system supporting multiple different filesystems running at the same time. The names 
(paths) of files are used (in combination with the concept of 'mount points') to select which 
filesystem is to be used. 

AdaOS cannot help but support this same concept of multiple filesystems, because any file is
(represented by) a system object, as are the filesystems that implement (the storage and
retrieval of) files. Since AdaOS system objects are polymorphic, they can have differences in
their interfaces and implementations that are dynamically selected. 

..........






-----------------------------------------------------------------------------------------------
## System Root {#sysroot}

It is assumed that every computer (system) has one filesystem that is always there, and which
forms the basis upon which all other system objects depend, including directories, files, and
other filesystems.

At the root of this filesystem is a particular directory, called the _system root_ directory of
the computer. 





..........





### Windows

Windows doesn't have a single system root, but rather it has drive letters designating all the
(effectively mounted) filesystems (or, to be precise, storage volumes), and a root directory
for each such drive letter.

It is conventional for the `C:` drive to be the 'system drive' (which typically contains the
`Windows` subdirectory, in which much of the most essential system files are kept), and so the
directory `C:\` (the root directory on the `C:` drive) serves as the system root for all
intents and purposes.

In fact, the system drive can always be ascertained from the `SYSTEMDRIVE` [environment
variable](../rts/envvars.md), whilst `OS` can be used to confirm that the host operating system
is Windows (it will have the value `Windows_NT`), and `SYSTEMROOT` gives the actual drive and
subdirectory that the core Windows files are in, e.g. `C:\WINDOWS`. Note that this is *not* the
system root in the AdaOS meaning. 

On the Windows platform, therefore, the `Current_Directory` property of a
[compartment](compart.md#sysroot) is: 

    %SYSTEMDRIVE%\

Typically, this will expand to: 

    C:\


### FHS (Linux)

On the Linux platform (or any FHS or Unix-based platform), the system root is always:

    /

In other words, it is the root directory of the (default filesystem of the) system. 

.....

The directory `/mnt` is the usual place for (non-default) filesystems to be _mounted_. 


### AdaOS Native

On the AdaOS Native platform, there isn't really any system root at all, as such. Instead, for
every [principal](../security/security.md#princ) there is, in effect, a kind of 'system' root
just for that principal. It just so happens that, since the 
[top user](../security/security.md#topuser) essentially owns the whole computer (system) and
everything in it, the system root of the top user serves as the system root of the entire 
computer. 

In almost all cases, a principal is a little microcosm, and the system root of the principal
serves as *the* system root perfectly well. Any [program compartment](compart.md) running under
a particular principal (i.e. that principal is the [owner](compart.md#owner) of the
compartment) sees the outside world purely from the perspective of that principal, and so the
'system' root of the principal *is* the system root to that program compartment. 

.....

Under AdaOS, the directory `/mnt` is, like FHS, the usual place for filesystems to exist.
However, since a filesystem is simply another system object, filesystems do not need to be
'mounted' as such. Instead, they simply exist. 

Normally, for each filesystem, a member will exist in `/mnt` which is one of:

 * a [placeholder](../objects/objects.md#ph) for the actual object (or a link to the object); 

 * a [link](../objects/objects.md#links) to a filesystem object; 

 * the filesystem object itself. 

Because filesystems are just system objects, they too can exist (in general) in any directory
anywhere. 

Whenever a placeholder is found, it is automatically replaced by the real object (as normal),
and it can be assumed that it will be at this moment the filesystem will be actually mounted
(prepared for usage) under the bonnet. But this will happen invisibly and automatically;
nothing outside the filesystem object need be concerned about it. The filesystem object will
generally be automatically shut down (and so dismounted in some sense) after a period of
inactivity. How this period of inactivity is will usually be configurable. As normal, when the
object is shut down, it will be replaced by a placeholder once again. 

..........




-----------------------------------------------------------------------------------------------
## Storage Units

One _storage unit_ is usually one of 8, 16, 32, or 64 bits of data. 

AdaOS adopts the following terminology:

| Bits | Name        |
| ---- | ----------- |
|    4 | nybble      |
|    8 | byte        |
|   16 | word        |
|   32 | long        |
|   64 | quad        |

This terminology is quite widely used, but is by no means universal or standard. In particular,
the spelling 'nybble' might be considered whimsical by some people, but I feel that it may
sometimes be useful to have a spelling that distinguishes it from the kind of nibble that a
fish may make of a baited hook, for example. 

If a storage unit is bigger than 8 bits (but its size is a multiple of 8), it also has 
_endianness_. The endianness of a storage unit is either _big endian_ or _little endian_. 

If it is big endian, the first byte of the storage unit, as stored in memory (and therefore, 
normally, as read from and written into some other storage medium) is the most significant 
byte of the storage unit. If little endian, the first byte is the least significant. 

This is not the only definition of endianness, and there is no definitive standard, but it is 
the meaning adopted by AdaOS (and the Ada language standard). 

It is assumed that different [machine architectures](../pxcr/targets.md#arch) will have 
different endianness. The Ada language allows the endianness of integer types to be specified; 
if the endianness of an integer type does not match the architecture, the compiler (or the 
Realizor, in the case of ECLAT) adds operations to reverse the endianness of objects of that 
type after data is loaded and before it is saved. 

.....



-----------------------------------------------------------------------------------------------
## Binary Files {#bin}

The term 'stored file' is used to encompass all the many different types of file. Each
different type of file stores different types of data. A particular file type might contain: 

 * just one instance of particular type of data; or 
 
 * a sequence of instances of one type of data; or 
 
 * a sequence of instances of many different types of data. 

For every file, however, the data will be stored in the form of a sequence of storage elements. 

When no distinction is being made between different file stypes, and its data is only being
thought of as a sequence of storage elements, then the file is termed a _binary file_. 

The concept of a binary file is the most basic type of file; all other file types are derived
from it. 

.....

The limited interface type `Stored_File`, declared in the package `AdaOS.Files`, represent a
stored file. This type is derived from `System_Object` (declared in the package
`AdaOS.Objects`), so it is a [system object](../objects/objects.md). 

.....




-----------------------------------------------------------------------------------------------
## Text Files {#text}

A _text file_ is a file that (conceptually or actually) contains a sequence of characters. 

........

The interface type `Text_Stored_File`, declared in the package `AdaOS.Files.Text`, 
represents text files. This type is derived from `Stored_File`. 

.........





-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## File Segmentation {#seg}

An AdaOS Native file is an array of _file segments_. Each file segment is identified within the file by
an integer, from 0 up to 9_223_372_036_854_775_807 (which is 2 to the power of 63, minus one)
inclusive. 

A file segment is an array of storage elements. Each storage element is identified (or
'addressed') within its segment by an integer from 0 up to 18,446,744,073,709,551,615 (which is
2 to the power of 64, minus one) inclusive. 

How the segments and their data is interpreted depends on the file type. The starting point for interpreting the data in a [Universal File Header](#ufh) file is the file's header. 

.....

The number of (non-empty) segments in a file is a property of the file, and can vary during the lifetime of the file. In the same way, the number of storage elements in any one segment can vary dynamically during the lifetime of the segment. A segment is permitted to have zero storage elements, but then the segment is considered to not exist; a segment is only considered to exist 

.....

Since most non-AdaOS file systems do not support segmentation, AdaOS will present them as 
having only segment number 0; all other segments will be empty. Any attempt to open any other 
segment will fail (with the exception 

????? 

being propagated). Programs should assume that a 
non-UFH file has only segment 0. 


### NTFS Streams

The Microsoft Windows NTFS filesystem enables a file to have multiple named 
[streams](https://www.ntfs.com/ntfs-multiple.htm). 

.....

A stream named `S` of a file whose path is `P` can be accessed using the extended path:

    P:S


### Apple HFS Plus Forks

The Apple Macintosh OS X HFS Plus filesystem enables a file to have any number of [forks]()

A fork named `F` of a file whose path is `P` can be accessed using the extended path:

    P/..F/rsrc


-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## Universal File Header {#ufh}

AdaOS defines a way for the data in files to be structured called the _Universal File Header_, 
or _UFH_. A file structured in this way is called a 'UFH file'. 

The UFH scheme assumes that the file has (is capable of having) multiple segments. 

All file types defined by AdaOS are UFH files. 


### File Header

Segment 0 contains the _file header_. 

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








