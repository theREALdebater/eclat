-----------------------------------------------------------------------------------------------
# Rearrangement (Defragmentation)

A segment (a contiguous stretch of memory storage elements) used to store the heterogeneous pool elements 
(objects) of a pool is likely to be initially used up by allocating pool elements to ascending 
addresses within the segment. As time progresses in the execution of the program, some of the 
pool elements will be deallocated, whilst others remain allocated. 

This results in 'holes' in the segment, and is termed _fragmentation_ [^1]. When allocating 
more pool elements, it is possible to arrive at the situation where a pool element cannot be 
allocated because there is no hole anywhere in the segment large enough, even though the total 
amount of free memory in the segment is enough. 

Fragmentation can lead to large amounts of memory being unusable, and some kind of 
_defragmentation_ technique is needed to avoid it (or mitigate it). 









...... abstract limited controlled `Rearrangeable_Pool`, declared in the package 

????? `AdaOS.Storage_Pools`, 

derived from `Root_Storage_pool`, .....







```ada
type Movement
is
   record
      From, To                 : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment                : Storage_Elements.Storage_Count
   end record;

type Movements is array (Positive range <>) of Movement;
```


The procedure `Rearrange` is declared as follows:

```ada
procedure Rearrange (Pool : in out Rearrangeable_Pool;
                     Work : in out Movements);
```

This procedure .....

..... `Work`  .....

For each element (of type `Movement`) of `Work`, the components `From`, 
`Size_In_Storage_Elements`, and `Alignment` must be set correctly. There should be a element in 
this array from every pool element that is to be potentially moved. (Normally, the array will 
have an element for every pool element in the pool.)

When the procedure returns, it will have set `To` for every element of the array. If the 
corresponding pool element was not moved , then `To` will be equal to `From`, otherwise it will 
have the new location of the pool element, and the caller will presumably need to make 
adjustments accordingly. 

For example, .....


[^1] Technically it is termed _external fragmentation_, but that does not concern us here. 

-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





