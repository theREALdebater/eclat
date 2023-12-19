-----------------------------------------------------------------------------------------------
# Endymion

__Endymion__ is an implementation of a [service shepherd](shepherds.md). 

The limited private type `Endymion_1_0`, derived from `Service_Shepherd` of the package
`AdaOS.Services.Controlled`, is declared in the package `AdaOS.Services.Controlled.Endymion`. 

Objects of this type implement a service shepherd for program-implemented controlled services
(services that are implemented by a service program). 

When a service program is run, its [compartment](../programs/compart.md) represents that
execution of the program. This compartment, when it exists, is called the _implementor_ of the
program's controlled services. 

A service program agent has a property `Services` a set of services that it is an agent for,
identified by object identifier. 

For each such service it also has the following properties:

 * The implementor of the service, which is null if the service is dormant (`Implementor`,
   read-only); 

 * The service program, identified by its object identifier, to be executed if the service is
   dormant and needs to be activated (`Program`); 

 * The program arguments to be passed into the service program if (the service is dormant and
   needs to be activated and therefore) the program needs to be run (`Arguments`). 

Endymion also:

 * implements a [program event broker](#progbrok) and a [services event broker](#svcbrok); 
 
 * initiates regular [purges](#purg); 
 
 * provides [program groups](#groups). 

The `shepherd` [stock service](services.md#stock) is currently implemented as an Endymion
object. 

-----------------------------------------------------------------------------------------------
## {#}






-----------------------------------------------------------------------------------------------
## {#}









-----------------------------------------------------------------------------------------------
## {#}






-----------------------------------------------------------------------------------------------
## {#}

........

The package `AdaOS.Services.Controlled` contains the following visible declarations: 

```ada

type Endymion_1_0 is limited new Service_Shepherd with private;

-- Unrestricted operations:

overriding
function Activate (Shepherd: not null access Endymion_1_0;
                   Service:  in Objects.Object_Id)
return
   access Controlled_Service'Class is abstract;

function Services (Shepherd: not null access Endymion_1_0)
return
   access Objects.Object_Set is abstract;

-- Privileged operations:

procedure Set_Services (Shepherd: not null access Endymion_1_0; 
                        Services:  access Objects.Object_Set) is abstract;

function Program (Shepherd: not null access Endymion_1_0;
                  Service:  in Objects.Object_Id)
return
   access Programs.System_Program'Class is abstract;

procedure Set_Program (Shepherd: not null access Endymion_1_0;
                       Service:  in Objects.Object_Id;
                       Program:  access Programs.System_Program'Class) is abstract;

function Arguments (Shepherd: not null access Service_ProEndymion_1_0gram_Agent;
                    Service:  in Objects.Object_Id)
return
   access constant Programs.Program_Argument_Array is abstract;

procedure Set_Arguments (Shepherd:   not null access Endymion_1_0;
                         Service:    in Objects.Object_Id;
                         Arguments:  access constant Programs.Program_Argument_Array)
is abstract;

function Implementor (Shepherd: not null access Endymion_1_0;
                      Service:  in Objects.Object_Id)
return
   access Programs.Program_Compartment'Class is abstract;
```

Remember that an Endymion object, as a system object, must be engaged before its other
operations can be successfully invoked. 




-----------------------------------------------------------------------------------------------
## {#}






-----------------------------------------------------------------------------------------------
## {#}






-----------------------------------------------------------------------------------------------
## {#}






-----------------------------------------------------------------------------------------------
## Endymion Configuration Helper {#helper}

?????

The _Endymion Configuration Helper_ is a [Realizor helper](..pxcr/helpers.md) which generates a 


[shepherd configuration module](?????) and 





?????

adds commands to the 
[Realizor command-line tool](../tools/pxcr.md) for the configuration of the programs and 
services of an executable image. 

The commands added to the Realizor command-line tool are:

 * `program` followed by the name of a program to be added, followed by the keyword `add`, 
   .......
 
 * `service` followed by the name of a controlled service and then the keyword `program` and 
   the name of its corresponding service program; 
   
These must both be in the context of an image .....

.....



-----------------------------------------------------------------------------------------------
## Program Event Broker (#progbrok)

Endymion implements a _program event broker_, 

[event broker](../events/events.md) and registers it with the 
[system event broker](../rts/rts.md#events); 

.....




-----------------------------------------------------------------------------------------------
## Services Event Broker (#svcbrok)

Endymion implements a _services event broker_, 

[event broker](../events/events.md) and registers it with the 
[system event broker](../rts/rts.md#events); 

.....




-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
## Purges {#purg}

.....








-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
## Program Groups {#groups}

A set of two or more programs can be controlled as if they were a single program by putting 
them into a _program group_. 

.....

If two programs named `A` and `B` are put into a group named `G`, then their names become `G/A` 
and `G/B` respectively. 

.....

A service group is represented by a service object. When referring to a group as a whole, its 
name is used. 

.....

It is possible for service groups to be put into a larger group. 

Suppose two services `A` and `B` are in group `X` and services `C` and `D` are in group 'Y'. 
The groups `X` and `Y` could be put into a super-group, named `Z` for example, in which case 
the full name of service `A` would be `Z/X/A`. 




-----------------------------------------------------------------------------------------------
## .....

In the future, Endymion will support intercommunication between programs running on different 
machines, by using an accompanying _network IPC_ service named `Syrinx`. 








-----------------------------------------------------------------------------------------------
## {#}






-----------------------------------------------------------------------------------------------
## {#}






-----------------------------------------------------------------------------------------------
## {#}






-----------------------------------------------------------------------------------------------
## {#}






-----------------------------------------------------------------------------------------------
## {#}






-----------------------------------------------------------------------------------------------
## {#}






