-----------------------------------------------------------------------------------------------
# ECLAT Service

The __ECLAT Service__ provides functionality associated with:

 * library management (creating them, changing them, deleting them); 
 
 * compiling source text files to update a library; 
 
 * 
 
 * providing information about a library, including deep and detailed info about its units; 
 
 * 
 
 * 
 
 * 
 
 * building a library to generate (or regenerate) one or more module files; 
 
 * 
 
 * 
 
 * 
 
 * 
 
 * 
 
.....

The service opens and maintains a [saved state](#state). 

.....


-----------------------------------------------------------------------------------------------
## 

?????? The name of the ECLAT service is `adaos.eclat.svc`

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
## Saved State {#state}

The _ECLAT Service saved state_ contains almost all the data needed by the ECLAT Service to 
provide its functionality. 

......







Currently the approach to storing the ECLAT Service saved state on secondary storage is the simplest 
possible: all data in the saved state is kept in a record structure (with a lot of pointers); that
record structure is made fully serializable (using default `'Input` and `'Output`); when the 
ECLAT Service starts up it loads the whole saved state into memory (and locks the file); 
when the service shuts down, it saves the whole saved state back into a file (and unlocks it). 

Periodically (e.g. every five minutes), the service saves the saved state back into the file, as a 
precaution against uncontrolled termination. 



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





