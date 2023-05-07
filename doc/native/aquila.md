-----------------------------------------------------------------------------------------------
# Aquila

__Aquila__ is a 'store system', which is the same as a filesystem, but without any file names
or directories. 

Aquila is the native storage format for the [AdaOS Native](native.md) platform. 






-----------------------------------------------------------------------------------------------
##  {#}

In Aquila, each 'file', which is termed a _store_, is identified within a volume by a number,
called a _store number_. Store numbers are rather like 'inode' numbers in Unix.  

Functionality such as directories and file names are layered on top of Aquila, so it does not
make sense for Aquila itself to deal in those. 





-----------------------------------------------------------------------------------------------
## Measures {#unit}

.....


### Adopted Nomenclature

.....

| Unit            | Abbrev.   | Number of Bits  |
| --------------- | --------- | --------------- |
| byte            | B         | 8               |
| word            | W         | 16              |
| long (word)     | L         | 32              |
| quad (word)     | Q         | 64              |

Most of the formats in this document have offsets and sizes in quads. 


### Standard Measures

.....




The power-of-ten derived units are: 

| Unit      | Abbrev.   | Number of Bytes |
| --------- | --------- | --------------- |
| kilobyte  | KB        | 10 ** 3         |
| megabyte  | MB        | 10 ** 6         |
| gigabyte  | GB        | 10 ** 9         |
| terabyte  | TB        | 10 ** 12        |
| petabyte  | PB        | 10 ** 15        |
| exabyte   | EB        | 10 ** 18        |

The binary power derived units are: 

| Unit      | Abbrev.   | Number of Bytes |
| --------- | --------- | --------------- |
| kibibyte  | KB        | 2 ** 10         |
| mebibyte  | MB        | 2 ** 20         |
| gibibyte  | GB        | 2 ** 30         |
| tebibyte  | TB        | 2 ** 40         |
| pebibyte  | PB        | 2 ** 50         |
| exbiyte   | EB        | 2 ** 60         |


### Storage Units

A storage device or volume is assumed to comprise a sequence of _storage units_.

Each storage unit is the same number of bits in size. Typically, this is one of: 8 (byte), 16
(word), 32 (long), or 64 (quad), but other sizes are possible. This unit is the same as the
storage unit defined by the Ada source text compiler/processor implementation. In the case of
the ECLAT-Realizor combination, this size is chosen by the Realizor, and it depends on the
[target architecture](../pxcr/targets.md#arch). 

????what about C?

There are no gaps (at least, conceptually) between the storage units, and they are _addressed_
by an index that starts at 0 for the first unit and goes up continuously to n-1, where n is the
number of storage units of the device or volume. 

.....


### Allocation Units

A [division](#div) or non-body [metablock](#meta) is divided into a sequence of _allocation
units_.

Each allocation unit is the same number of storage units in size. Typically, this is chosen to
be the same size as the page size of the target architecture. If it is not, a [special
mitigation](#mitig) must be used, which is inefficient. 

There are no gaps between the allocation units, and they are _addressed_ by an index that
starts at 0 for the first allocation unit and goes up continuously to n-1, where n is the
number of allocation units of the division or or non-body metablock. 



-----------------------------------------------------------------------------------------------
##  {#}




-----------------------------------------------------------------------------------------------
##  {#}




-----------------------------------------------------------------------------------------------
##  Partitions and Volumes {#vol}

.....

An _Aquila  volume_ has the following GUID partition type: 

    C5700486-BA1E-4938-8988-8786C9F8A64F

.....

The beginning of the volume 


### Volume Arrays

A _volume array_ is a set of volumes that act essentially as if they were one volume, called
the _effective volume_ of the array, but the different volumes in an array can be located on
different devices, disks or other storage mediums, or partitions. 

The addressing schemes within Aquila are all designed to allow different volumes in an array to
be moved around (to different partitions or disks, etc.) without needing to be internally
adjusted in any way. 



There are several different ways the volumes in an array can be combined to form a single
effective volume:

 * Concatenated Array

 * Simple Parallel Array

 * Hashed Parallel Array 3/4, 4/5, and others



The different volumes in any kind of array must all have the same [division](#div) size. 

The different volumes in a concatenated array can be different sizes, but for a parallel array
(of any kind) all the volumes must have the same size. 

Volumes arrays are intended to provide a simple and portable software-implemented alternative
to [RAID][1], but cannot fully compete with hardware RAID. 


### Concatenated Arrays




### Simple Parallel Arrays

.....


The array has a _striping level_ S and a _mirroring level_ M. The number of volumes in the
array must be S x M, which is N. 

    N = S x M

The volumes are numbered from 0 up to N-1. 

For every volume numbered v, its _striping number_, s, is the integer nearest but no greater
than v / M. 

    s = [lower] v / M

For every volume numbered v, its _mirroring number_, m, is the integer nearest but no greater
than v / M. 

    s = [lower] v / M


For example, if the striping level S = 3 and the mirroring level M = 2, then there must be 6
volumes in the array, and the striping number s and mirroring number m of each will be as
follows: 

| Volume | s      | m      |
| 
|      0 | 
|      1 | 
|      2 | 
|      3 | 
|      4 | 
|      5 | 








### Hashed Parallel Arrays





-----------------------------------------------------------------------------------------------
##  Divisions {#div}

Each [volume](#vol) is divided into a sequence of _divisions_. 

Each division in a volume array is the same number of allocation units in size. This is
normally between 1 and 1024 MiB, but can be smaller or larger in unusual circumstances. A
minimum size is dictated by the division header size (for the volume array) and a maximum size
is dictated by the allocation unit size (for the volume array). 



There are no gaps between the divisions, and they are _addressed_ within the volume by an index
that starts at 0 for the first allocation unit and goes up continuously to n-1, where n is the
number of allocation units of the division. 

Within a concatenated volume array, the array can be considered to be a sequence of divisions.
Within the array:

 * the first division (division number 0) of the first volume (volume number 0) is considered
   to be volume number 0 of the array; 

 * the last division (division number n0-1) of the first volume is considered to be volume
   number n0-1 of the array (where n0 is the number of divisions in the first volume); 

 * the first division (division number 0) of the second volume (volume number 1) is considered
   to be volume number n0 of the array;  

 * the last division (division number n1-1) of the second volume is considered to be volume
   number (n0+n1)-1 of the array (where n1 is the number of divisions in the second volume); 

 * the first division (division number 0) of the third volume (volume number 2) is considered
   to be volume number (n0+n1) of the array; 

 * the last division (division number n2-1) of the third volume is considered to be volume
   number (n0+n1+n2)-1 of the array (where n2 is the number of divisions in the third volume); 

 * and so on.








-----------------------------------------------------------------------------------------------
##  {#}




-----------------------------------------------------------------------------------------------
## Division Headers {#dh}

.....

The first ????? allocation units of every division contains a _division header_, which contains
metadata for the division, as well as for the volume (replicated in every division of the
volume), and for the volume array (replicated in every division of every volume of the array). 



| 
| 
| 
| 
| 
|           | Volume Metadata Header
|           | Volume Metadata Array 




-----------------------------------------------------------------------------------------------
##  {#}




-----------------------------------------------------------------------------------------------
##  {#}




-----------------------------------------------------------------------------------------------
##  {#}




-----------------------------------------------------------------------------------------------
## Volume Metadata {#}

The Volume Metadata Header has the following format: 

| Offset (B)   | Size (B)     | Name                  | Description                          |
| ------------ | ------------ | --------------------- | ------------------------------------ |
|              |              | 
|              |              | 
|              |              | 
|              |              | 
|              |              | 
|              |            1 | `Kind`
|              |              | `GUID`
|              |              | `Count`               | Number of volumes in the array       |
|              |              | 
|              |              | 
|              |              | 


### Array Type

The `Kind` is of the enumerated type `Volume_Array_Kind`, and 

| Name                  | 
| `Concatenated`        | 
| `Parallel_Simple`     | 
| `Parallel_3_4`        | 
| `Parallel_4_5`        | 
| `` 
| `` 
| `` 




-----------------------------------------------------------------------------------------------
## Volume Metadata Array {#}

The Volume Metadata Array comprises n elements, each of which has the following format: 

| Offset (B)   | Size (B)     | Name                  | Description                          |
| ------------ | ------------ | --------------------- | ------------------------------------ |
|              |           16 | `GUID`
|              |              | `Length`              | Number of divisions in the volume    |
|              |              | 
|              |              | 
|              |              | 

Each element corresponds to one of the volumes in the array. 

The order of the elements is significant: the first element in the VMA corresponds to volume 0
in the volume array; the second element in the VMA corresponds to volume 1 in the volume array;
and so on. 






### Length



For any kind of parallel array, the length must be the same for every volume in the array. 





-----------------------------------------------------------------------------------------------
##  {#}




-----------------------------------------------------------------------------------------------
##  {#}




-----------------------------------------------------------------------------------------------
##  {#}




-----------------------------------------------------------------------------------------------
##  {#}




-----------------------------------------------------------------------------------------------
##  {#}




-----------------------------------------------------------------------------------------------
##  {#}




-----------------------------------------------------------------------------------------------
##  {#}




-----------------------------------------------------------------------------------------------
##  {#}




-----------------------------------------------------------------------------------------------
##  {#}




-----------------------------------------------------------------------------------------------
##  {#}




-----------------------------------------------------------------------------------------------
##  {#}




-----------------------------------------------------------------------------------------------
##  {#}




-----------------------------------------------------------------------------------------------
##  {#}




-----------------------------------------------------------------------------------------------
##  {#}

[1]: <https://en.wikipedia.org/wiki/RAID> "Wikipedia: RAID"



