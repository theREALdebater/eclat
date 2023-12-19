-----------------------------------------------------------------------------------------------
# Tasks

According to the Von Neuman model, a computer knows how to execute any of a specific set of
simple _machine instructions_. The computer (central processing unit, or CPU) execute
instructions one at a time. Each instruction is encoded in memory, and a _program counter_
holds the address of the next instruction to execute. A few instructions can modify the
contents of the program counter, in which case the next instruction to be executed is at
whichever address was put into the program counter. Otherwise, when an instruction is executed,
the program counter is advanced to the next memory location. In this way, there is a 'thread'
of instruction execution.

A _task_ is an abstraction of such a thread of execution. 

A task is, more or less, the same as what is often termed a 'thread' in other operating
systems, and corresponds directly to a task in the Ada programming language. 

As such, [PXC](pxc.md) supports multiple tasks all executing (notionally, or actually) at the
same time. 

.....



-----------------------------------------------------------------------------------------------
## 

The PXC tasking model allocates a separate 

[stack](?????) 

to each a task. Although PXC does not
formally have any registers, the machine code that it is translated into will have a separate
(copy of) the set of working registers for each task.

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






