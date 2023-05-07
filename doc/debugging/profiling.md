-----------------------------------------------------------------------------------------------
# Profiling

ECLAT and the Realizor include a _profiling_ capability that is based on 
[monitoring](monitoring.md].

?????The [monitoring](monitoring.md) mechanism provided by ECLAT can be used to provide _profiling_ 
of the executions of a [assembly](../rts/assemblies.md). 

Broadly, profiling can mean many things, but here it is used to specifically mean the recording 
of the time taken executing each unit within the assembly. 

This profiling data can be used by the [Realizor](../pxcr/Realizor.md#profiling) to help to 
optimise (improve the efficiency of) the emitted machine code. 



-----------------------------------------------------------------------------------------------
## 

Every time the assembly is executed---see [Executional Instances](../rts/Instances.md)
---.....





.....





-----------------------------------------------------------------------------------------------
## ???

The Realizor is able to use files containing profiling data to help optimise the machine code 
it emits. 

Every _buffer period_, which is 10 seconds by default, the number of times each control point 
is handled is counted up. This data, a _profiling buffer_, is then written out into a data 
file, whilst (at the same time) the next buffer is being counted up. 






-----------------------------------------------------------------------------------------------
## ???






-----------------------------------------------------------------------------------------------
## ???






-----------------------------------------------------------------------------------------------
## ???






-----------------------------------------------------------------------------------------------
## ???






-----------------------------------------------------------------------------------------------
## Elision

After a profiling buffer has been written out into a file, it is retained, as the _comparison 
buffer_. 

When the next buffer is about to be written into a file, its contents are first compared with 
the comparison buffer. If (a comparison buffer exists and) the comparison results in a match, 
the buffer is discarded and not written into a file. 

This behaviour is termed _elision_, and is intended to prevent the writing of files that do not 
actually have value, because they are simply a repeat of previous files. 

A situation where a buffer is simply a repeat of a previous one is typically caused when a 
program is left executing but in a quiescent state. For example, a program may present a UI, 
but if the user doesn't actually interact with the program the only thing the program will be 
doing, in essence, is listening for incoming events. 

The comparison between buffers is heuristic. They are considered matching if their contents are 
'close'. .....

.....






-----------------------------------------------------------------------------------------------
## ???









