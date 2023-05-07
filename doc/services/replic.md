-----------------------------------------------------------------------------------------------
# Service Replication

Some services can suffer from problems of scalability under certain circumstances: 

 * The execution of a service may require a lot of resources (processor time, memory, etc.),
   possibly because its methods are being called very frequently, resulting in the service
   slowing down because the resources its needs exceed what is available on the (single)
   computer it is running on. 

 * The execution of a service's method, on one computer, may block, until it has completed, the
   processing of other calls to the same method or to any other method of the same service. 

 * If a service fails, perhaps because of an unhandled exception, some of its state may be
   lost, and there may be a period of loss of service (while it is being restarted or fixed). 

These problems can be alleviated by running the same service multiple times simultaneously. 

As long as its service (or services) are capable of being _replicated_, it is possible to run a
[service program](servprog.md) multiple times simultaneously, on the same computer or on
multiple different computers. Each instance of the program is called a _replicant service
program_. Each service thus replicated is called a _replicant service_.

The set of replicants is called a _service battery_ (or just _battery_). 

To be capable of being replicated, a service must have some extra functionality beyond that of
a normal service. 



-----------------------------------------------------------------------------------------------
## Replication Controllers {#rc}

In order to ensure that a service battery appears, to programs which use it, to be a single
service, a [proxy](services.md#prox) service, called a _replication controller_ of the battery,
must be created. 

The replication controller has several duties: 

 * Accept calls to the same set of methods as the replicant services, and to make one or more
   calls in turn to one or more of the replicants, as well as to receive the response or
   responses from it or them, and pass them back to the original caller. 

 * Start up the replicant service programs in a co-ordinated manner. 
 
 * Ensure that the replicant services are are all [synchronised](#sync) whenever necessary. 
   
 * Manage the failure of one or more replicant services within the battery. 

 * Shut down the replicant service programs in a co-ordinated manner. 

 * Co-ordinate with its co-controllers, if any, see the section on [co-controllers](#cocon). 

More details on the above functionality follows. 



-----------------------------------------------------------------------------------------------
## Battery Event Channel {#chan}

Every battery must have a name, .....

Every battery has a corresponding [event channel](../events/events.md#chan) with the name: 

    svc/B

where `B` is the name of the battery.



.....



-----------------------------------------------------------------------------------------------
## Start-up and Shutdown of a Battery {#start}

A [replication controller](#rc) must be able to start and stop of all the replicants in the
battery that is under its control. 

If the replication controller is started, it then, itself, starts all the replicants. If the
replication controller is stopped, it then, itself, stops all the replicants. 

.....






-----------------------------------------------------------------------------------------------
## Active and Passive Replicants {#act}

.....








-----------------------------------------------------------------------------------------------
##  {#}

The replication (co-)controllers of a battery, and services of the battery, use the [battery
event channel](#chan) to communicate with each other. 

Replication (co-)controllers uses two strategies to control and monitor the health of the
services in the battery: 

 * Any service in the battery can send events that correspond to events such as service
   failure. Replication controllers can pick up these events and handle them. 

 * Replication controllers send out regular _heartbeat_ [messages](../events/messaging.md) to
   the services. Each service must quickly respond to each heartbeat message with an
   acknowledgement reply. If a heartbeat does not get a timely reply, the controller assumes
   the service has failed, and takes appropriate action. 


.....











-----------------------------------------------------------------------------------------------
## Updating and Reinstallation {#upd}

When one or more services or replication controllers needs to be updated, the requisite 

 [Kantan](../../../kantan/doc/Intro.md) 

manage the updating and reinstallation of the service
programs of a battery. 


to perform the required installation(s). 





[Replication controllers](#rc) must 



.....

 1. Send out a warning event. 

 2. Shut down all the service programs (and therefore all the services) of the battery, and
    wait for their shutdown to be confirmed. 

 3. 

 4. Restart all the service programs of the battery. 






-----------------------------------------------------------------------------------------------
## {#}





-----------------------------------------------------------------------------------------------
## {#}





-----------------------------------------------------------------------------------------------
## {#}













-----------------------------------------------------------------------------------------------
## Synchronisation {#sync}

One of the things that a replicant must be able to do is to _synchronise_ its state with
all the other active replicants 






[Daedalus](?????) provides special features supporting synchronisation .....

A [factor](?????) can be divided up (horizontally, i.e. row-by-row) into 'clumps'. If it is not
explicitly divided into clumps then the entire factor is considered to be one clump. 

Separate hidden tables hold a hash value computed for each clump. Synchronisation can be
efficiently achieved by comparing these hash values instead of comparing entire clumps. 

.....




-----------------------------------------------------------------------------------------------
## {#}





-----------------------------------------------------------------------------------------------
## {#}








-----------------------------------------------------------------------------------------------
## Nyota

The [Nyota](../tools/nyota.md) command-line tool can be used to generate the following Ada
source text code: 

 * skeleton for the service itself

 * complete implementation of the service program 

 * complete implementation of the replication controller service and its service program 

 * skeleton for a test harness program 

 * in the future, optionally, a web API (SOAP or REST) wrapper program 













Each method (primitive operation) of a service is assigned one of three _replication routing_
settings: 

 * _pan-replicant routing_

 * _single-replicant routing_

 * _custom routing_ 

A method is, by default assigned as pan-replicant if it is not explicitly assigned. 








### Pan Replicant Routing

When a call is made to a method with pan-replicant routing, the same call is routed, by the
replication controller, to the same method of *all* the currently active replicants. 

Thus, the method will be executed in each of the replicants, and will receive the same values
for all its input parameters. 

Each replicant is therefore expected to update its state (files, database, anything that is
persistent) identically, and is expected to return the same values in its output parameters. 

The replication controller gathers the return values of the output parameters from all of the
replicants, and checks that they are all equal. It waits for all of the replicants to finish
execution. If any response is different, or if any (or all) of the method invocations fails (by
propagating an exception), then .....

.....







### Single Replicant Routing

When a call is made to a method with single replicant routing, the call is routed, by the
replication controller, to the same method of any one of the currently active replicants, chosen
at random. 

.....

Single replicant routing is suitable for methods that do not (significantly) change the
service's state. Often a service has many 'query' methods that only return information derived
from the service's state, but which do not modify the state. Since each replicant has the same
state as all the others, it should be sufficient for any one of the replicants to handle this
kind of method. 




### Custom Routing 






-----------------------------------------------------------------------------------------------
## {#}





-----------------------------------------------------------------------------------------------
## Co-controllers {#cocon}

With the aim of achieving a higher level of scalability when it is required, it is possible to
have more than one replication controller in charge of a battery. 

In this case, the replication controllers are referred to as _co-controllers_ of each other. 

These co-controllers communicate with one another to co-ordinate their activities 

.....









-----------------------------------------------------------------------------------------------
## {#}

















-----------------------------------------------------------------------------------------------
## Replication Controller Configuration



.....




-----------------------------------------------------------------------------------------------
## {#}





-----------------------------------------------------------------------------------------------
## 





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





-----------------------------------------------------------------------------------------------
## {#}





-----------------------------------------------------------------------------------------------
## {#}





-----------------------------------------------------------------------------------------------
## {#}







