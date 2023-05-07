-----------------------------------------------------------------------------------------------
# Memory Management: Fixed Element Size Pools

A _fixed element size pool_ allocates exactly the same number of storage elements per pool 
element. 

This kind of pool is not rearrangeable. Because all its elements are the same size, there is 
never any need to rearrange them. 




-----------------------------------------------------------------------------------------------
##




The type `Fixed_Element_Pool`, declared in the package 

?????`AdaOS.Storage_Pools` .....

derived from the type `Root_Storage_Pool` (also declared in the package `AdaOS.Storage_Pools`)

.....

..... `Address` ..... `System` .....

..... `Storage_Count` ..... `AdaOS.Storage_Elements` ..... 

 ..... `Segment_Number`..... `AdaOS.Memory` .....

This type has five discriminants:

 * `Element_Size`, of type `Storage_Count`, which is the number of storage elements allocated 
   for every pool element; 

 * `Alignment`, also of type `Storage_Count`, which is the alignment required for allocation of
   all pool elements; 

 * `Segment`, of type `Segment_Number`, which is the segment within which pool elements are to 
   be stored; 

 * `Start`, of type `Address`, which is the start address (offset) of the part of the segment 
   within which pool elements are to be stored; 
 
 * 'Storage_Size`, of type `Storage_Count`, which is the number of storage elements of the part 
   of the segment within which pool elements are to be stored. 

Attempting to call `Allocate` with `Size_In_Storage_Elements` greater than `Element_Size`, or 
with `Aligment` different to the value of the discriminant `Aligment`, propagates the exception 
`Storage_Error`. 

Calling `Allocate` with `Size_In_Storage_Elements` less than `Element_Size` simply results in a 
certain number of storage elements being wasted for that pool element. (The number of storage 
elements wasted will be `Element_Size - Size_In_Storage_Elements`.) 












-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
## Initialisation




-----------------------------------------------------------------------------------------------
## Allocation






-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
## Initialisation






-----------------------------------------------------------------------------------------------
## Allocation






-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
##






