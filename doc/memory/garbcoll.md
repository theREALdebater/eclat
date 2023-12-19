-----------------------------------------------------------------------------------------------
# Garbage Collection

During its execution, a program will (in almost every case) allocate extra memory for it to
use. For each such piece of allocated memory, there will come a time in the program's execution
when that memory will never be used by the program ever again. It is generally beneficial to
_reclaim_ that memory for re-use, by the same program instance or any other. 

_Garbage collection_ is the (processes and mechanisms of the) automatic reclamation of memory
that is no longer being used during a program's execution. 

Typical Ada implementations provide little or no garbage collection, and rely on the programmer 
using explicit memory management techniques.  

The Realizor supports advanced garbage collection, with:

 * a classical reachability algorithm to establish allocated objects that are eligible for 
   reclamation;
  
This does not prevent the development of Ada software that avoids the use of garbage
collection, but it provides the option for software to make use of garbage collection. 



-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 

 * separate sub-heaps for fixed-size small objects, which do not suffer from fragmentation and 
   allow one-hop pointers;
  
 * generational sub-heaps, reducing the amount of object copying required for defragmentation; 

 * automatically ascertaining every pool whose usage is confined to one task, so that locking 
   can be avoided for access to the pool;

 * performing activities in background tasks where appropriate; 
   
 * mechanisms to control garbage collection in various detailed ways.




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







