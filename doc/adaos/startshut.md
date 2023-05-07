-----------------------------------------------------------------------------------------------
# Start-Up and Shutdown

Before a [compartment](compart.md) can execute in a stable, normal state of execution, it must
go through a _compartment start-up_ process. Likewise, when the compartment transitions from a
normal state of execution to a terminal state, it must go through a _compartment shutdown_
process. 

Every compartment is associated with the running of a program (an AdaOS-level program). For all
compartments, compartment start-up and shutdown essentially comprises running the program's
assemblies and terminating those assemblies' instances. 

In the case of the top compartment, compartment start-up and shutdown is synonymous with
_system start-up_ and _system shutdown_. 

System start-up and shutdown entails start-up and shutdown of the [effective
system](../intro/intro.md#effsys). 

On a hosted platform, [system start-up](../adaos/startshut.md) means beginning the execution of
the host program's process and then start-up of the top compartment. System shutdown means
shutdown of the top compartment and then termination of the host program's process. 

On the AdaOS Native platform, system start-up means start-up of the top compartment immediately
after [booting](../pxcr/booting.md) the computer, and system shutdown means shutdown of the top
compartment and the bringing the computer to a stopped state.

The 'stopped state' of a computer might mean being fully powered down (or ready to be powered
down), or at some S-level or similar level of partial power-down, or being (ready to be)
rebooted. 

.....



-----------------------------------------------------------------------------------------------
## Start-Up

.....









-----------------------------------------------------------------------------------------------
## Shutdown Deadline

The _shutdown deadline_ of a compartment is an absolute moment in time at which all
[executional instances](instances.md) of the compartment---in practice, any remaining instances which have not
already shut themselves down---are immediately killed and destroyed (without any warning or any
possibility of delay or evasion). 

When a compartment is shut down with a certain deadline, it automatically causes any and all of
its subcompartments to also be shut down, with the same deadline. 

The shutdown deadline of the top compartment is called the _system shutdown deadline_. 

There are three configurable periods of time called the _shutdown warning periods_ levels 1 to
3, or _W1_, _W2_, and _W3_ for short. They are expressed in seconds, but are real numbers, so
fractions of a second are possible. They all represent a period of time prior to the shutdown
deadline, and the condition W1 >= W2 >= W3 is enforced. 

This means that warning level 1 is given first, then level 2, and finally level 3. 

The default shutdown warning periods are:

| Period | Configuration Name       | Default
| ------ | ------------------------ | -------
| W1     | `shutdown-warning-1`     | 0.15
| W2     | `shutdown-warning-2`     | 0.10
| W3     | `shutdown-warning-3`     | 0.05

There are normally three levels of communication that precede the shutdown of a compartment:

 1. A compartment shutdown event is broadcast W1 seconds in advance of the deadline; 
 
 2. Services get a system shutdown service control call W2 seconds in advance of the deadline; 
 
 3. Executional instances receive a `SIGTERM` signal W3 seconds in advance of the deadline.
 
It is the [program controller](programs.md#progcont) that performs steps 1 and 2, and AdaOS
that performs step 3. 
 
The system shutdown event is broadcast immediately after system shutdown is initiated. 

This means that, by default, system shutdown takes a maximum of about 0.15 seconds to be 
completed. 

On the AdaOS platform, this will .....


### System Shutdown

For every program that is sent the ????? signal, 
the [program controller](programs.md#progcont) 
sends an event of the type `System_Shutdown`, declared in the package `AdaOS.??????`, 
to the event channel `??????.Shutdown` in the 

??????[system broker]() 

.....


W1 seconds prior to 
the 

?????instance shutdown deadline. 


### Services Shutdown

An event of type `Services_Shutdown`, declared in the package `AdaOS.Services`, is sent to the 
event channel `Services.Shutdown` in the 

??????[services broker](../services/services.md#sb) 

W2 seconds prior to 
the instance shutdown deadline. 

.....





### Shutdown Service Call

.....


### Shutdown Signal

A `SIGTERM` signal is sent to every to every instance that is not in the Terminated state. 

.....








