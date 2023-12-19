-----------------------------------------------------------------------------------------------
# Executional Instances

An _executional instance_ is one execution of an [partition](partitions.md). 

Each host operating system will have its own definition of what constitutes a instance (or its 
equivalent). Very often the term 'process' is used for something similar to an instance. 

An instance is represented by a [system object](../objects/objects.md) of a type derived from
the abstract limited controlled private type `Executional_Instance`, which is declared in the
package `AdaOS.Execution`. 

.....



-----------------------------------------------------------------------------------------------
## .....

There is a function, `Task_Instance`, declared in the package `AdaOS.Instances`, which
returns a value of type `access AdaOS.Instances.Executional_Instance'Class` (an access value
referencing an executional instance). 

This function can be called to obtain a reference to the calling task's instance, or to the
instance of any task whose ID is known. 



.....




-----------------------------------------------------------------------------------------------
## Compartment of an Instance

The function `Compartment` returns (an access value referencing) the [compartment](compart.md)
of the given instance. 

```ada

function Compartment (Instance: not null access Executional_Instance) 
return 
   Compartment_Access is abstract; 
```



-----------------------------------------------------------------------------------------------
## 

......




-----------------------------------------------------------------------------------------------
## 

......




-----------------------------------------------------------------------------------------------
## 

......




-----------------------------------------------------------------------------------------------
## 

......









-----------------------------------------------------------------------------------------------
## Instance Execution {#exec}

When created, by making a call to the function `New_Instance` of a [program
partition](partitions.md) and then calling `Create` on the returned instance (system object),
the instance's compartment's [execution mode](compart.md#mode) must be `Suspended`. If it is
not in the `Suspended` mode, the exception `AdaOS.Compartments.Status_Eror` is propagated.

When the compartment's mode is changed to `Running`, the new instance becomes eligible to start
execution, starting at its [partition initialisation procedure](partitions.md#aip). 

When an instance is created, a new [task](../pxcr/tasks.md), which will be the [environment
task][1] of the new instance, is created.

The environment task then: 

 1. Executes the module initialisation procedures each of each module in the partition; 

 2. Executes the main procedure of the partition, if there is one. 

All the tasks of the instance, including its environment task, are [eligible](../pxcr/tasks.md)
for execution only if the instance's compartment's execution mode is `Running`. 

Whenever the mode is or becomes `Suspended`, ......

If the mode is or becomes `Completed`, all the tasks of the instance are .....





.....




-----------------------------------------------------------------------------------------------
## Instance Completion {#end}

When an instance reaches the end of its execution---when its environment task is complete, in
the sense of [Ada Refence Manual section 7.6.1][1]---code implicitly inserted by the
[Realizor](../pxcr/realizor.md) does the following:

 1. Test whether there are no other instances in the compartment, using
    `Task_Instance.Compartment.Instances.Count = 1`.

 2. If there aren't any other instances, call `Task_Instance.Compartment.Stop`, to make
    the compartment enter `Completed` mode. This will automatically cause the final remaining
    instance object to be deleted. 

 3. Otherwise, call `Delete` on the instance object, to delete it (it will become vacant).
    Deleting the instance object triggers any remaining tasks of the instance to be aborted and
    deleted. 

 4. Finally, the environment task deletes itself, so the instance has no tasks. The amount of
    time between this and the deletion of the instance object causing the environment task to
    be deleted is not defined. Usually, it will only be a very short amount of time, but
    however long it takes, it is better that there are no pointless tasks in existence, using
    up system resources. 

.....




-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
## References

[1]: <> "Ada Reference Manual: Environment Task"






