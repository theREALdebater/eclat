-----------------------------------------------------------------------------------------------
# Memory Management

.....



-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## Segments





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## Access Types (#deref)

A pool-specific access type .....




An intrinsic (non-remote) general access type contains two pieces of information as if with the 
following declarations: 

```ada
type Segment_Identifier is range 0 .. Max_Segment_Number;

type <access_type> 
is
   record
      Segment: Segment_Identifier;
      Offset: Address_Offset;
   end record;
```

To dereference an intrinsic access type, the Realizor generates code that, for example on the 
AMD64 architecture:

 1. looks up the segment offset from a table indexed by the `Segment`; 
 
 2. adds the `Offset` to the segment offset to get a 64-bit linear address. 









-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## Storage Management Configuration

.....

The configuration pragma `Storage_Management` is used to set the storage management policy for 
the library it is compiled into. 

..... storage management policy can be one of:

 * no reclamation or rearrangement (`None`)

 * reclamation, but no rearrangement (`Reclaim`)

 * reclamation and rearrangement (`Rearrange`)

If there is no pragma `Storage_Management`, the default setting of reclamation and 
rearrangement applies. 


-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





