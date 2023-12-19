-----------------------------------------------------------------------------------------------
# Controlled Services and Service Programs

A _service agent_ is a system object that aids in the activation of services (specifically,
controlled services). 

A _controlled service_ is a [service](services.md) that has a service agent to activate it,
when necessary. 

A _program-implemented_ controlled service is a controlled service which is implemented by a
dedicated [program](../programs/programs.md) that is called a _service program_. 

A service program is dedicated to implementing one or more controlled services when it is 
running. Service programs do not normally do anything other than registering and implementing 
the services. 

It is unusual, in practice, for a service program to implement more than one service, and it is
typical for a service program to have the same name as the (one and ony) service it implements.  



-----------------------------------------------------------------------------------------------
## Controlled Service Metadata {#meta}

A _controlled service metadata_ object contains the [metadata](services.md#meta) associated
with a controlled service. 

The package `AdaOS.Services.Controlled` declares the abstract tagged non-limited type
`Controlled_Metadata`, which is derived from `Service_Metadata`. 

This type adds one further property: 


### Agent

The `Agent` property is (an access value referencing) the [service agent](#agent) which is able
to activate the controlled service if it is dormant, and in any event return (an access value
referencing) the service object. 



-----------------------------------------------------------------------------------------------
## 

A controlled service is represented by the limited interface type `Controlled_Service`, which
is derived from `System_Service` and declared in the package `AdaOS.Services.Controlled`. 

The package `AdaOS.Services.Controlled` contains the following visible declarations: 

```ada

function Program (Service: not null access Controlled_Service)
return
   access Programs.System_Program'Class is abstract;

function Arguments (Service: not null access Controlled_Service)
return
   access constant Programs.Program_Argument_Array is abstract;

function Implementor (Service: not null access Controlled_Service)
return
   access Programs.Program_Compartment'Class is abstract;



```



.....

If the service is dormant, the function `Implementor` will return `null`. 

.....



.....




-----------------------------------------------------------------------------------------------
## Service Programs {#prog}




The interface type `Service_Program`, declared in the package `AdaOS.Services`, represents a
service program. 

The most important extra property of a service program is named `Services_Controlled`, whose
value is a set of objects, each object being a service that the program controls (implements). 

The package `AdaOS.Services` contains the following visible declarations: 

```ada
function Controlled_Services (Program: in Service_Program) 
return
   Objects.Object_Set is abstract;

procedure Set_Controlled_Services (Program:  in out Service_Program; 
                                   Services: in     Objects.Object_Set) is abstract;
```

.....











-----------------------------------------------------------------------------------------------
##






-----------------------------------------------------------------------------------------------
## Service Program Metadata {#meta}

Every [shepherd](#shep) maintains [service program metadata](../objects/containers.md#meta) for
every service program it contains. 

The interface type `Service_Program_Metadata` is declared in the package `AdaOS.Services`, and
is derived from `AdaOS.Program.Program_Metadata`.

This type declares several [properties](../intro/intro.md#prop): 

 * [Object identifier](../objects/objects.md#oid) `OID` (inherited from `Member_Metadata` via `Program_Metadata`); 

 * `External_Tag` (inherited from `Member_Metadata` via `Program_Metadata`); 

 * service program mode

 * .....

Actual types derived from `Service_Program_Metadata` may, of course, add further properties and
methods. 

For example, if the shepherd is a directory (most will be), the metadata will
also have the property `Name`. 



-----------------------------------------------------------------------------------------------
## .....

.....




-----------------------------------------------------------------------------------------------
## Service Program Modes {#mode}

For each of its member service programs, a [shepherd](#shep) maintains a _service program
mode_. 

A member service program can be in one of five modes at any one time. 

The package `AdaOS.Services` contains a declaration of the enumerated type
`Service_Program_Mode` whose five values are as follows: 

| Mode         | Description                                                  |
| ------------ | ------------------------------------------------------------ |
| `Dormant`    | No implementor (compartment) exists                          |
| `Stopped`    | Can run, but is not currently running                        |
| `Starting`   | Getting towards `Running`, but not yet able to accept calls  |
| `Running`    | Running normally, able to accept and process calls           |
| `Retired`    | Failed too many times, cannot run                            |

The mode is said to be _good_ if it is `Dormant`, `Stopped`, `Starting`, or `Running`, and
_bad_ if it is `Retired`. 

These modes have a similarity similar to the [execution modes](../adaos/compart.md#mode) of a
compartment as well as the [basic state](../objects/objects.md#state) of objects, but they are
not quite the same.  

```ada
function Mode (Program: not null access Service_Program)
return
   Service_Program_Mode is abstract;
```

The function `Mode` returns the current service program mode of a service program. 


### Dormant {#dormant}

A service program is _dormant_ when no [implementor](#imp) exists, but the program is not
[retired](#retired). 

When a service program is dormant, if any of its controlled services is
[found](../objects/containers.md#find), the shepherd runs the service
program, by calling [`Start`](#start), and waits for the service's
[placeholder](../objects/containers.md#ph) to be replaced by a link to the service. Then, the
finding of the service can complete successfully. 

There may be a little bit of time in between the service program being in a particular mode and
its implementor actually being created or destroyed. 

The term 'dormant' is deliberately chosen to coincide with the concept of the dormancy of an
[object](../objects/objects.md#state). Whilst the dormancy of an object is not exactly the same
as the dormancy of a service program, the concepts are very similar. 

When a service program is added to a shepherd, it is initially in the dormant mode. 


### Stopped

If a service program is _stopped_, its controlled services are (nominally) not doing anything
and should not be called upon to do anything. 

Generally, any attempt to call upon a controlled service to do something when its service 
program is stopped should 
wait, for a certain period of time, for the service to get into the running mode. If this 
period of time expires without the service getting into the running mode, the attempt should 
fail (if it was a call to a subprogram, that subprogram should propagate the exception 

?????`Mode_Error` of `AdaOS.Services`). 

The service program may (temporarily) be in the process of stopping when it is in this mode.
The program might, therefore, be temporarily doing things while in the `Stopped` mode (for example, 
it might be finishing the
execution of things one of its services was called upon to do before it went into stopped
mode), but it will not start doing anything while in stopped mode. 


### Starting

If a service program is _starting_, it is temporarily getting itself into the `Running` mode.
Its services are still not yet ready to be called upon to do things. 

Generally, any attempt to call upon a controlled service to do something when it is in this
mode should wait, for a certain period of time, for the service to get into the `Running` mode.
If this period of time expires without the service getting into the running mode, the attempt
should fail (if it was a call to a subprogram, that subprogram should propagate the exception 

?????`Mode_Error` of `AdaOS.Services`). 


### Running

If a service program is _running_, it may be doing things, and its controlled services are
ready to be called upon to do things. 


### Retired

A service program is typically _retired_ when it has failed too many times. In this mode, the
program is (nominally) not doing anything and its controlled services should not be called upon
to do anything. In this mode, the program should not be allowed to be started. 

When a service program is retired, it should terminate itself or shut itself down, so that
there will, after a time, be no implementor. However, the program is termed retired rather than
dormant. 

Generally, if a service program is put into retired mode, it stays in that mode. A program
controller may provide a means to get programs out of retired mode, but that depends on the
service controller. 

Any attempt to call upon a controlled service to do something when its service program is in
this mode should immediately fail (if it were a call to a subprogram, that subprogram should
propagate the exception 

?????`Mode_Error` of `AdaOS.Services`).



-----------------------------------------------------------------------------------------------
## Service Program Start-up Strategies {#strat}

A [shepherd](#shep) is configured with all the service programs it is to control. 

Against each service program, a _start-up strategy_ is also configured, as well as possibly a
_start-up phase_. 

For any one service program, the start-up strategy can be one of the following: 

 * `Standby`
 
 * `On_Demand`
 
 * `Auto` 
    
When all the service programs the shepherd controls are either [dormant](#dormant) or
[retired](#retired), then the shepherd initiates [system shutdown](#shutdown). 

The _start-up phase_ of a member service program, if specified, must be an integer made up
solely of decimal digits `0` to `9`, whose value is between 1 and 99 (inclusive). It is only
relevant to the `Auto` strategy. If omitted, the default is 99. A phase is considered _earlier_
than phases which have a numerically higher value. 


### Standby {#standby}

If the start-up strategy of a service program is `Standby`, the program is not run unless
requested by calling its [`Start`](#start) method. 

.......

See [Example of Running a Program](../adaos/programs.md#ex1) for more details on how a program is started .....

........

This is the default start-up strategy of a program if a start-up strategy is not specified.



..........


### On Demand {#ondem}

If the start-up strategy of a service program is `On_Demand`, the program is started whenever
one of the services it controls is [found](../objects/containers.md#find) and the program is
[dormant](#dorm). 

The program is started by calling its [`Start`](#start) method. 

.........


### Auto {#auto}

If the start-up strategy of a program is `Auto`, the shepherd will start the program 
(once), as soon 
as the shepherd has completed its own initialisation 
and after all service programs of an earlier phase have started
 
 

?????and are awaiting a [signal](?????). 


. 

The program is started by calling its [`Start`](#start) method. 

If the program completes, it becomes dormant again. 

The `Auto` service programs of a particular phase are all run in parallel. 

........





-----------------------------------------------------------------------------------------------
## Automatic Service Program Restart {#restart}

If a service program, under the control of a particular [shepherd](#shep), fails, meaning that
it completes with an [exit code](../adaos/compart.md#exco) other than 0 (for success), the
shepherd can be expected to automatically attempt to restart the service program. 

This is termed _automatic restart_. 

However, it can be expected that the shepherd will impose restrictions on automatic restart. 

If, within a certain time period of configurable length (default 10 minutes), a service program
fails more than a certain configurable threshold number of times (default 10), the shepherd can
be expected to [retire](#retire) the service program. 

......



-----------------------------------------------------------------------------------------------
## {#}





-----------------------------------------------------------------------------------------------
## {#}





-----------------------------------------------------------------------------------------------
## Shepherd Configuration Modules {#shepcm}




????? Or just have a saved state file?




..... _program controller configuration module_ .....






The program controller configuration module class is named: 

    progcont.config
    
This module contains exports named: 

    progcont.programs
    progcont.services
    
......

This data provides configuration information for an executable image's program controller (e.g. 
Tethys). 

They could be imported like this: 

```ada
   Programs: array (Positive range <>) of constant ......
   with
      Import,
      External_Name => "progcont.programs";

   Services: array (Positive range <>) of constant ......
   with
      Import,
      External_Name => "progcont.services";
```

.....












-----------------------------------------------------------------------------------------------
## Service Program Control Operations

Every service program must implement the additional primitive operations of the interface type
`Service_Program`, called the _service program control operations_. 

..... 

The service program control operations are listed in the following table. 

| Operation | Transitions SP into mode    |
| --------- | --------------------------- |
| `Start`   | starting (and then running) |
| `Stop`    | stopped                     |
| `Retire`  | retired                     |
| `Sleep`   | dormant                     |

.....

Normally, a service program's control operations are only called upon (directly) by the
program's [shepherd](#shep). All software other than the shepherd should leave service program
control to be done automatically by the shepherd. 

A service program is restarted by calling `Stop` and then `Start`. This sequence should cause
the program to go through the complete cycle of disposing of its resources and then
initialising them again, as if it were being started for the first time. No service program
should cut this cycle short in any way; if it does, it must be guaranteed to have the same
effect as the full cycle. 


### Start {#start}

```ada
procedure Start (Program: not null access Service_Program);
```

The procedure `Start` transitions a service program from dormant or stopped to starting. 

If the program is already starting or running, calling `Start` (harmlessly) does nothing. 

If the program is retired, calling `Start` fails, with the exception `Mode_Error` (of
`AdaOS.Services`) being propagated. 

When dormant, the service program should create a new [compartment](../adaos/programs.md#exec),
which becomes the [implementor](#imp) of the service program. Once the compartment has been
started, it should go into `Starting` mode (a compartment mode, not a service program mode), in
which it will perform all of its normal initialisation. When the initialisation is complete,
both the compartment and the service program should transition into `Running` mode. 

When stopped, the program should go into starting mode. To do this, the program will perform
all of its normal initialisation. When the initialisation is complete, the program should
transition into the running mode. 

Starting a service program will not normally happen frequently, and is not speed critical.
However, the start-up process should, generally, aim to get the program into a state where it
is able to respond quickly and perform its duties efficiently. The program will be in running
mode when it is in this state. 

If the service program is in the middle of stopping when `Start` is called, the program should
complete its stopping procedures normally and then go into starting mode as normal. It is
unlikely to be advisable to try to optimise this situation, but if there is any such
optimisation there must be no danger of the program being destabilised by it. 


### Stop {#stop}

```ada
procedure Stop (Program: not null access Service_Program);
```

The procedure `Stop` immediately transitions the service program from starting or running to
the stopped mode. 

If the program is already in stopped mode, calling `Stop` (harmlessly) does nothing. 

If the program retired, calling `Stop` does nothing (in particular, it does not change the mode
of the program and it does not propagate any exception). 

If the program is in starting mode when `Stop` is called, it should immediately go into stopped
mode and never go into running mode in between. 

When going into stopped mode from either starting or running mode, the program should reduce
its resource usage to a minimum. Reducing (re-)initialisation time should not normally be
considered. Stopping a service program will not normally happen frequently, and is not speed
critical. 

......


### Retire {#retire}

```ada
procedure Retire (Program: not null access Service_Program);
```

The procedure `Retire` immediately transitions the service program from any other mode into
retired mode. 

.....

If the program is already retired, calling `Retire` (harmlessly) does nothing. 

If the program is in starting mode when `Retire` is called, it should immediately go into
retired mode and never go into running mode. 

When going into retired mode from either starting or running mode, the program's compartment
should be deleted (so that there is no longer any implementor). 

.....

If a service program is retired, it remains retired permanently. It can be unretired (returned
to normal, dormant mode) by deleting it from the shepherd and then adding it to the shepherd
again. 


### Sleep {#sleep}

```ada
procedure Sleep (Program: not null access Service_Program);
```

The procedure `Sleep` transitions the service program from any other mode, except retired, into
dormant mode. 

.....



-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## Service Program Shutdown (#shut)

.....

When a service program is in stopped mode, the program should 
respond to the .....

.....






-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## Service Program Startup Strategies (#startup)

.......

See [Program Startup Strategies](../adaos/programs.md#startup) for full details


### Service on Demand

If the startup strategy of a program is set to `service` (without the `auto` word after it), 
the program is run whenever one of its associated services is required and the program is not 
already running. This strategy is term _service on demand_. 




### Auto Service

If the startup strategy of a program is set to `service auto`, the program controller, as soon 
as it has completed its own initialisation (after system startup), will run the program 
(once). This strategy is termed _auto service_. 


.....










-----------------------------------------------------------------------------------------------
## 

























-----------------------------------------------------------------------------------------------
## Stock Services {#stock}


????? More succinct, and into Services.md



ECLAT is supplied with a number of _stock services_ that play an essential role in enabling 
programs to execute. In general, programs will assume that these services are available. 


### Tethys

[Tethys](tethys.md) is a program controller .....

The name of this service is: 

    adaos.services.tethys
    
Tethys does not have any associated service program. It is all included in a single [program 
partition](../rts/partitions.md) module, named: 

    adaos.init.tethys


### General Resource Name Resolvers

There are three [General Resource Name Resolvers](garnerers.md), services which can be given a 
path (string) and return a system object that is a general resource. 

The names of these services are:

| GRNR Service Name           | Name                                          |
| --------------------------- | ----------------------------------------------|
| `adaos.services.grnr.cmpt`  | [Compartment Member GRNR](garnerers.md#cmpt)  |
| `adaos.services.grnr.host`  | [Host Filesystem GRNR](garnerers.md#host)     |
| `adaos.services.grnr.url`   | [URL GRNR](garnerers.md#url)                  |

A general resource can be any kind of object, but often they are [files](../rts/files.md). 


### Event Forwarding

The [Event Broker](events.md) service enables partitions to be sent events raised by 
other partitions. 

This service is based on the ECLAT [Event](../events/events.md) infrastructure. 

A program that wishes to be notified of events in a certain event filter .....

The name of this service is:

    adaos.services.events
    
The associated service program is also named `adaos.events`.


### Logging

The [Logging](logging.md) Service collects and stores logging events 

This service is based on the ECLAT [Logging and Auditing](logging.md) infrastructure.

    adaos.services.logging

.....



### Auditing

The [Auditing](auditing.md) Service collects and stores logging events 

.....

This service is based on the ECLAT [Logging and Auditing](logging.md) infrastructure.

    adaos.services.auditing

.....


### Kronos

[Kronos](kronos.md) controls _jobs_, which encapsulates all the information needed to achieve 
a single, specific piece of work. 

Jobs are used by programs whenever they need to do something that does not need to be 
accomplished immediately ('in the background'). 

The name of this service is:

    adaos.services.kronos
    
The associated service program is named `adaos.programs.kronos`.


### Distributed Transaction Management

The [Distributed Transaction Management](transactions.md) service provides transaction objects 
that can coordinate the activities of multiple partitions under the control of a 
single transaction. 

.....










-----------------------------------------------------------------------------------------------
## Other Services

.......


### ECLAT Service

.......


### Realizor Service

.......


### Windows Files

......

...... is a [shim](#shims) service .....


### Linux Files

......

...... is a [shim](#shims) service .....














-----------------------------------------------------------------------------------------------
##






-----------------------------------------------------------------------------------------------
##







