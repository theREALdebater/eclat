-----------------------------------------------------------------------------------------------
# Object Identifier Allocation

.....






-----------------------------------------------------------------------------------------------
## Object Identifier Implementation

Under the bonnet, OIDs are implemented by AdaOS as unsigned 64-bit integers: 

```ada

type Object_Id is mod 2**64;
```

(This will not come as a shock or a surprise to anyone.)

The way OIDs are allocated uses the 'four seasons' algorithm, in combination with a three-tier
block allocation scheme. These are documented ......

The _allocation space_ of `Object_Id` is therefore 2 to the power of 64 (which is
18,446,744,073,709,551,616 for the curious). 





-----------------------------------------------------------------------------------------------
## Object Identifier Allocation {#oid}




-----------------------------------------------------------------------------------------------
## 



-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 







