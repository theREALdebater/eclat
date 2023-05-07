-----------------------------------------------------------------------------------------------
# Executional Instances

An _executional instance_ is one execution of an [assembly](assemblies.md). 

Each host operating system will have its own definition of what constitutes a instance (or its 
equivalent). Very often the term 'process' is used for something similar to an instance. 

An instance is represented by a [system object](../objects/objects.md) of a type derived from
the abstract limited controlled private type `Executional_Instance`, which is declared in the
package `AdaOS.Execution`. 

.....



-----------------------------------------------------------------------------------------------
## .....

There is a function, `Task_Instance`, declared in the package `AdaOS.Instances`, which returns
a value of type `AdaOS.Instances.Instance_Access` (an access value referencing an executional
instance). 

This function can be called to obtain a reference to the calling task's instance, or to the
instance of any task whose ID is known. 



.....




-----------------------------------------------------------------------------------------------
##





```ada

function Compartment (Instance: not null access Executional_Instance) 
return 
   Compartment_Access is abstract; 
```

The function `Compartment` returns (an access value referencing) the compartment of the given
instance. 




-----------------------------------------------------------------------------------------------
## 

......









-----------------------------------------------------------------------------------------------
## Instance Execution {#exec}

When created, by making a call to the function `New_Instance` of a [program
assembly](assemblies.md) and then calling `Create` on the returned instance (system object),
the instance immediately begins executing, starting at its [assembly initialisation
procedure](assemblies.md#aip). 

A new [task](../pxcr/tasks.md), which will be the [environment task][1] of the new instance, is created, with its
[task owner](?????), [task authority](?????), ..... set to those of the new instance's compartment.

The environment task then: 

 1. Executes the module initialisation procedures each of each module in the assembly; 

 2. Executes the main procedure of the assembly, if there is one. 

All the tasks of the instance, including its environment task, can be 
[eligible](?????) for execution only if the instance's compartment's 
[execution mode](compart.md#mode) is `Running`. 

Whenever the mode is or becomes `Suspended`, ......

If the mode is or becomes `Completed`, all the tasks of the instance are .....





.....




-----------------------------------------------------------------------------------------------
##






-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
## References

[1]: <> "Ada Reference Manual: Environment Task"






