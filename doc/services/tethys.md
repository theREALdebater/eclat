-----------------------------------------------------------------------------------------------
# Tethys

__Tethys__ is an ECLAT [stock service](services.md#stock) which is a [program
controller](../adaos/programs.md#progcont). 

Tethys also:

 * implements a [program event broker](#progbrok) and a [services event broker](#svcbrok); 
 
 * initiates regular [purges](#purg); 
 
 * provides [program groups](#groups); 
 
 * 
 



Tethys is currently the default (and only) program controller for AdaOS. 



-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
## Tethys Configuration Helper {#helper}

The _Tethys Configuration Helper_ is a [Realizor helper](..pxcr/helpers.md) which generates a 
[program controller configuration module](../rts/programs.md#pccm) and adds commands to the 
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

Tethys implements a _program event broker_, 

[event broker](../events/events.md) and registers it with the [system 
event broker](../rts/rts.md#events); 

.....




-----------------------------------------------------------------------------------------------
## Services Event Broker (#svcbrok)

Tethys implements a _services event broker_, 

[event broker](../events/events.md) and registers it with the [system 
event broker](../rts/rts.md#events); 

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

In the future, Tethys will support intercommunication between programs running on different 
machines, by using an accompanying _network IPC_ service named `Syrinx`. 




















































-----------------------------------------------------------------------------------------------
## Implementation of Tethys

.....

The Tethys package body registers, using `AdaOS.Services.Register_Service`, a single service 
object, named 

????? `tethys.svc`

so that Tethys is always available ......


.....

```ada
package Tethys
is
   type Tethys_Service is new AdaOS.Services.System_Service with private;
   -- public operations of Tethys_Service
private
   -- private stuff
end;
```

.....

```ada
package body Tethys
is
   -- implementations of Tethys_Service operations
   Service: aliased Tethys_Service;
begin
   AdaOS.Services.Register_Service (Service'Access);
end;
```

.....



