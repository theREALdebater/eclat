-----------------------------------------------------------------------------------------------
# Compartments

A _program compartment_ is a [security](../security/security.md) and
[accounting](../security/accounting.md) boundary, which conceptually contains the system
resources which are accessible to running of a [program](programs.md)

The compartment contains a set of [executional instances](instances.md) corresponding to the
partitions of the program (when it was run). 

The compartment also represents many (or all) aspects of the 'outside world' to the instances. 

Compartments form a [hierarchy](../intro/hier.md). There is one [top compartment](#top). Every
other compartment has exactly one super-compartment immediately above it. Any compartment can
have any number of sub-compartments immediately below it (but need not have any). 

When a running program (one of its instances, to be precise), as represented by a compartment
C, runs another program, the compartment that is created as a result will be a sub-compartment
of compartment A. 

In many other operating systems the term 'process' is used to mean an executional instance of a
program or an execution of a whole program (other operating systems have no notion of one
program giving rise to multiple instances). 


### Compartment as a System Object {#obj}

A compartment is a [system object](../objects/objects.md). 

The limited interface type `Program_Compartment`, declared in the package `AdaOS.Compartments`,
represents a compartment. This type is derived from `System_Object`. 


### Priming {#prim}

A compartment is said to be _primed_ if its [execution mode](#mode) is `Suspended`, `Running`,
or `Completed`. It is _unprimed_ if its mode is `Unprimed` (and vacant if its mode is
`Vacant`). 

All the procedures for changing a compartment's property other than the [exit code](#exco) and
the mode-changing procedures, cannot be used if the compartment is primed; attempting to call
one of these procedures on a primed compartment propagates the exception
`AdaOS.Compartments.Mode_Error`. 

The procedure `Set_Exit_Code` cannot be used if the compartment's mode is `Completed`;
attempting to call this procedures on a completed component propagates the exception
`AdaOS.Compartments.Mode_Error`. 

Some of the properties are _required_. These are all of an access type, and so they start as
`null` when the compartment is create (by a call to `Create`). If a procedure is called to
transition the compartment into a primed mode, and one of the required properties has not been
set (to something other than null), then the exception `Mode_Error` is propagated. 

The procedure `New_Compartment` of the program being run to create the compartment will always
set the required properties. 

Attempting any operation, except for `Create` or `Load`, on a vacant compartment will cause
`Constraint_Error` to be propagated (because it will internally be attempting to dereference an
access value that is null). 



-----------------------------------------------------------------------------------------------
## Clustering, Migration, and Co-Compartments {#coco}

Since the AdaOS Native platform will, in the future, support some (or all) of the partitions of
a program being executed on different computer systems, the concepts of _clustering_,
_migration_, and _co-compartments_ will be implemented in the future. The relevant AdaOS
interfaces are therefore designed at the outset to support these anticipated future
developments. 

The concept behind co-compartments is based on the concept of a _cluster_. A set of computer
systems can be configured to be joined together as a cluster. The cluster operates, in general,
as if it were one computer system. 

AdaOS Native will, in the future, implement clustering. For hosted AdaOS, clustering is not
relevant; if the host platform does supports something like it, AdaOS will support those
features. 

One of the important features of clustering is that most [executional instances](instances.md)
can be _migrated_, during their execution, from one computer in the cluster to another, in
order to dynamically balance the 'load' (amount of resource usage) of each computer. 

To aid this migration, the instances arising from the execution of a program are all effectively 
within the same conceptual compartment. 

For every conceptual compartment in the cluster, there can be an actual compartment object in
each computer in the cluster. The actual compartment objects for any one conceptual compartment
are called _co-compartments_ of each other. 

On a computer system with no clustering, it is as if that computer is the one and only computer
in a cluster of one. Thus, each conceptual compartment is represented by one single actual
compartment object, in that computer (and there are no co-compartments as such). 

Co-compartments are intended to (help to) provide all the partitions with the same executional
environment (conceptual compartment) regardless of which different computer each is executing
on. 

In order to maintain the fiction that the co-compartments are all the same compartment, the
co-compartments will communicate with one another. Any change made to one co-compartment will
be automatically propagated out to the others, so that they all maintain the same state. 

However, for the sake of practicality, there will be a set of compartment
[properties](../intro/intro.md#prop) that are configured to be propagated, and all other
properties will not be propagated. 






The default will be for no properties to be propagated, so
this is an 'opt in' arrangement. 

????? opt-in for all props?





Each program will be configurable as to which compartment properties are propagated, as well as
which class of computer each partition is to be executed on. 

This can have an effect on the definitive semantics of partitions executing in different
compartments. 

.....



-----------------------------------------------------------------------------------------------
## 



-----------------------------------------------------------------------------------------------
## Top Compartment {#top}

When the [top program](programs.md#top) is created during the system's [boot
procedure](../pxcr/booting.md), a compartment is created (solely) for the instances of its
partitions to execute in. This compartment is termed the _top compartment_. 

The top compartment 

?????(and *only* the top compartment) 

has no super-compartment. Its owner is the [top user](../security/security.md#topuser). 

The top user's procedure `Initialize_Compartment` is used to set the properties of the top
compartment. 

The procedure `Initialize_Compartment` of the top user (not any other principal) uses values in
the boot image's [initialisation file](?????) to help it initialise many of the top
compartment's properties. 

The boot image's configuration file can be used to set values in the boot image's
initialisation file. 

The top compartment contains (system objects that represent) *all* the resources available to
the [effective system](../intro/intro.md#effsys). 

Note: It must do, since no other compartment could access something the top compartment cannot. 



-----------------------------------------------------------------------------------------------
## Compartment Properties {#prop}

A compartment has the following [properties](../intro/intro.md#prop): 

 * a reference to the [program](programs.md) whose execution the compartment represents,
   `Program` (required); 

 * the compartment's super-compartment, `Super` (required); 

 * one [principal](../security/security.md#princ) which is the compartment's [owner](#owner)
   and defines the compartment's identity, `Owner` (required); 

 * a set of the [executional instances](instances.md) of the compartment, `Instances`
   (read-only); 

 * an [execution mode](#mode), `Mode` (read-only, but with procedures to change mode); 

 * an [exit code](#stat), indicating whether the compartment has started or finished execution,
   and, if it has finished, what the outcome was (success or error), `Exit_Code`. 
 
 * an [ambit](../security/security.md#amb), `Ambit`; 
 
 * a default authority, `Default_Authority`; 
 
 * a set of [accounts](../security/accounting.md), `Accounts`; 
 
 * an optional [base transaction](../database/transactions.md#basetran), `Base_Transaction`; 

 * a [domain identifier](../objects/objects.md#oid), `Domain`; 

 * a [clock offset](time.md#off), `Clock_Offset`; 

 * a sequence of [program arguments](#arg) that provide a way of customising the running of a
   program, `Arguments` (required); 

 * a set of [predefined text streams](#strm) available to this compartment, `Text_Streams`; 
 
 * a set of [environment variables](#envvar), `Environment_Variables`; 
 
 * a set of sub-compartments of this compartment, `Compartments`; 
 
 * a [current working directory](#cwd), `Current_Directory`; 

 * the [system event broker](../events/events.md#sb), `Event_Channels`; 

 * a directory of [stock programs](#stkprg) available to this compartment (for running),
   `Programs`; 
 
 * a directory of [service services](#stksvc) containing the services available to this
   compartment, `Services`. 

All these properties of a compartment are described in the following sections. 

Many of these properties will not exist or be applicable or appropriate to a compartment, in
which case the property will have a null or empty value, and might propagate an exception if an
attempt is made to get it or set it. 


### Summary

Here is a summary of the properties of a compartment.

| Name                     | Description                    | (Designated) Type              |
| ------------------------ | ------------------------------ | ------------------------------ |
| `Ambit`                  | Ambit                          | `Security_Ambit`               |
| `Accounts`               | Accounts                       | `System_Ledger`                |
| `Arguments`              | Program arguments              | `Program_Argument_Array`       |
| `Base_Transaction`       | Base transaction controller    | `Transaction_Controller'Class` |
| `Clock_Offset`           | Clock offset                   | `Duration`                     |
| `Compartments`           | Sub-compartments               | `Program_Compartment'Class`    |
| `Current_Directory`      | Current working directory      | `Object_Directory'Class`       |
| `Default_Authority`      | Default authority              | `Security_Authority`           |
| `Domain`                 | Home domain identifier         | `Domain_Identifier`            |
| `Environment_Variables`  | Environment variables          | `Object_Directory'Class`       |
| `Event_Channels`         | System Event Broker            | `Event_Broker'Class`           |
| `Exit_Code`              | Program exit code              | `Program_Exit_Code`            |
| `Instances`              | Executional instances          | `Executional_Instance'Class`   |
| `Mode`                   | Compartment mode               | `Compartment_Mode`             |
| `Owner`                  | Compartment's owner            | `Security_Principal'Class`     |
| `Program`                | Program of this compartment    | `System_Program'Class`         |
| `Programs`               | Available programs             | `Program_Directory'Class`      |
| `Services`               | Available stock services       | `Service_Directory'Class`      |
| `Super`                  | The super-compartment          | `Program_Compartment'Class`    |
| `Text_Streams`           | Standard streams               | ` ` |

These are all described in the following sections.


### Windows 

.....

 * process identifier of the Windows process;

.....


### Linux

.....

 * process identifier of the Linux process;

.....





-----------------------------------------------------------------------------------------------
### Property Initialisation {#propinit}

Every compartment [property](#prop) is one of the following four kinds: 

 * A _read-only_ property `P` cannot be changed (directly); it has no setter (`Set_P`)
   procedure. 

 * A _required_ property must be set (to something other than null) before the compartment is
   primed, and is always initialised by the procedure `New_Compartment` of the program being
   run to create the compartment; 

 * An _inherited_ property is initialised, by the `Initialize_Compartment` procedure of the
   compartment's owner, usually by being copied from the compartment's super-compartment. 

 * A _non-inherited_ property is usually left null, blank, zero, or empty by the
   `Initialize_Compartment` procedure. 

The description of each property specifies which kind it is, but here is a summary. 

The following properties are read-only: `Compartments`; `Instances`; `Mode`. 

The following properties are required: `Arguments`; `Owner`; `Program`; `Super`; `Ambit`. 

The following properties are non-inherited: `Accounts`; `Exit_Code`.

All the other properties are inherited. 

`Arguments`, `Owner`, `Program`, `Super`, and `Ambit` are initialised by the procedure
`New_Compartment` of the program that is being run to create the compartment. 

`Compartments` is initially empty, and, for a compartment C, always returns the set of C's
sub-compartments, i.e. the compartments whose super-compartment is C. It will return empty if
`Super` has not been set (is null). 

`Instances` always returns the set of executional instances of the compartment, and will never
change. 

`Mode` is only changed by the mode-changing procedures. 

The `New_Compartment` method first sets the required properties of the compartment, and sets
all the other properties to null, blank, zero, or empty, and then calls the owner's procedure
`Initialize_Compartment` to initialise the inherited properties of the new compartment. 

`Exit_Code` is initialised to 0 (for success) when the compartment is created (and is not
changed by either the `New_Compartment` or `Initialize_Compartment` procedures). It can be set
to another value unless the compartment is in `Completed` (or `Vacant`) mode. 



-----------------------------------------------------------------------------------------------
## Compartment Program {#prog}

A compartment has the required [property](#prop) `Program`. 

If the procedure `New_Compartment` of a [program](programs.md#run) is called to create a
compartment, the procedure sets this property to the program. 

```ada
function Program (Compartment: in Program_Compartment) 
return
   access System_Program'Class is abstract;

procedure Set_Program (Compartment: in out Program_Compartment; 
                       Program:     access System_Program'Class) is abstract;
```

The function `Program` returns (an access value referencing) the program which is being run in
the compartment, or null if the program has not been set. 

The procedure `Set_Procedure` sets the compartment's program to `Program`, which can be null to
signify the program is not set. 



-----------------------------------------------------------------------------------------------
## Super-Compartment

A compartment has the required [property](#prop) `Super`. 

If the procedure `New_Compartment` of a [program](programs.md#run) is called to create a
compartment, the procedure sets this property. 

```ada
function Super (Compartment: in Program_Compartment) 
return
   access Program_Compartment'Class is abstract;

procedure Set_Super (Compartment: in out Program_Compartment; 
                     Super:       access Program_Compartment'Class) is abstract;
```

The function `Super` returns (an access value referencing) the super-compartment of the given
`Compartment``, or null if the super-compartment has not been set. 

The procedure `Set_Super` sets the super-compartment of the given `Compartment`. The given
`Super` can be null to signify the super-compartment is not set. 



-----------------------------------------------------------------------------------------------
## Compartment Owner {#owner}

Every compartment has a [principal](../security/security.md#princ)---a user or a
role---who/which is the compartment's _owner_. 

The required [property](#prop) `Owner` returns (an access value referencing) the compartment's
owner. 

A compartment's owner is specified as the `Owner` parameter of an overloading of the
`New_Compartment` method of [programs](programs.md#run). 

There is an overloading of the procedure `New_Compartment` that does not have an `Owner`
parameter. This overloading assumes the owner of the new compartment will be the same as that
of the specified (or default) super-compartment. 

The owner of the [top compartment](#top) is always the 
[top user](../security/security.md#topuser). 

A compartment cannot run without an owner. This is because the compartment cannot have an ambit
without an owner, and the compartment cannot do anything without an ambit. The `Owner` property
of compartments is therefore a required property. 

```ada
function Owner (Compartment: in Program_Compartment) 
return 
   not null access Security.Security_Principal'Class is abstract;
```

The function `Owner` returns (an access value referencing) the current owner of a compartment. 



-----------------------------------------------------------------------------------------------
## Executional Instances

A compartment maintains a read-only [property](#prop) which a the set of 
[executional instances](instances.md) 

Each instance corresponds to one of the [partitions](partitions.md) of the
[program](programs.md) whose execution gave rise to the compartment. 

.....



```ada
function Instance (Id: in System.RPC.Partition_Id) 
return
   access Execution.Executional_Instance'Class is abstract;
```



.....



-----------------------------------------------------------------------------------------------
## Execution Mode {#mode}

At any one time, a compartment can be in one of five _execution modes_, represented by the
`Mode` read-only [property](#prop). 

The package `AdaOS.Compartments` contains a visible declaration of the `Compartment_Mode`
enumerated type whose five values are as follows: 

| Mode            | Description                                                              |
| --------------- | ------------------------------------------------------------------------ |
| `Vacant`        | Has not been created or loaded, or after it has been saved or deleted    |
| `Unprimed`      | Has never been executed, properties can be modified                      |
| `Suspended`     | Can execute, but is not currently doing so                               |
| `Running`       | Instances are executing                                                  |
| `Completed`     | All instances have completed                                             |

The execution mode is a read-only property of the compartment (there is no `Set_Mode`
procedure), but there are [mode-changing procedures](#mcp) that can change the mode.

```ada
function Mode (Compartment: not null access Program_Compartment) 
return 
   Compartment_Mode is abstract;
```

The function `Mode` returns the current execution mode of a compartment. 


### Vacant Mode

A compartment is a system object. Whenever the compartment is
[vacant](../objects/objects.md#state), its executional mode is `Vacant`. 


### Unprimed Mode

When a compartment's execution mode is `Unprimed`, its properties can be changed.

See [Property Initialisation](#propinit) for more about the normal initialisation of
compartment properties. 

After this initialisation, properties can be set to other values until the compartment is put
into a different mode. 


### Suspended Mode

In the `Suspended` execution mode, none of the [tasks](../pxcr/tasks.md) of any of the
[instances](instances.md) of the compartment are [eligible](?????) for execution, but all of
the instances' state is preserved (unchanged) as its 
[saved state](../objects/objects.md#state). 


### Running Mode

In the `Running` execution mode, the instance's tasks are eligible for execution, as normal. 


### Completed Mode

The `Completed` execution mode indicates that all of the compartment's instances have completed
execution, and therefore there can never be any further execution (of any instances) in the
compartment. 

When entering this mode, for every executional instance of the compartment that has not already
been finalised, the instance is automatically finalised and then deallocated. Finalising an
instance can be expected to cause any non-completed tasks to be aborted. 

Whenever all the [instances](instances.md) have [completed](instances.md#end) their execution,
the compartment's mode is automatically changed to `Completed`. 


### Mode-Changing Procedures {#mcp}

Besides the procedures `Create`, `Load`, `Save`, and `Deleted` inherited from `System_Object`,
the package `AdaOS.Compartments` contains the following declarations: 

```ada
procedure Run     (Compartment: in out Program_Compartment) is abstract;
procedure Suspend (Compartment: in out Program_Compartment) is abstract;
procedure Stop    (Compartment: in out Program_Compartment) is abstract;
```

The procedure `Run` is used to transition a compartment into the `Running` mode. The procedure
`Suspend` is used to transition into the `Suspended` mode. The procedure `Stop` is used to
transition into the `Completed` mode. 

The procedures `Create`, `Load`, `Save`, `Deleted`, `Run`, `Suspend`, and `Stop` are the
_mode-changing procedures_ of compartments. 


### Mode Transitions

A compartment's mode can be changed, but there are only certain _mode transitions_ that are
valid: 

| From Mode    | To Mode      | By Calling   | Comments                                      |
| ------------ | ------------ | ------------ | --------------------------------------------- |
| `Vacant`     | `Unprimed`   | `Create`     | Creation of a new component                   |
| `Vacant`     | `Unprimed`   | `Load`       | If mode was `Unprimed` when saved             |
| `Vacant`     | `Suspended`  | `Load`       | If mode was `Suspended` when saved            |
| ------------ | ------------ | ------------ | --------------------------------------------- |
| `Unprimed`   | `Running`    | `Run`        | Sets the component running                    |
| `Unprimed`   | `Suspended`  | `Suspend`    | Setting properties is done                    |
| `Unprimed`   | `Completed`  | `Stop`       | Execution of compartment not needed           |
| `Unprimed`   | `Vacant`     | `Delete`     | Compartment not needed                        |
| `Unprimed`   | `Vacant`     | `Save`       | Persist in unprimed mode                      |
| ------------ | ------------ | ------------ | --------------------------------------------- |
| `Suspended`  | `Running`    | `Run`        | Un-suspend the compartment                    |
| `Suspended`  | `Completed`  | `Stop`       | Further execution not needed                  |
| `Suspended`  | `Vacant`     | `Delete`     | Compartment no longer needed                  |
| `Suspended`  | `Vacant`     | `Save`       | Persist the compartment                       |
| ------------ | ------------ | ------------ | --------------------------------------------- |
| `Running`    | `Suspended`  | `Suspend`    | Suspend the compartment                       |
| `Running`    | `Completed`  | `Stop`       | Terminate running of the compartment          |
| ------------ | ------------ | ------------ | --------------------------------------------- |
| `Completed`  | `Vacant`     | `Delete`     | Exit code no longer needed                    |
| `Completed`  | `Vacant`     | `Save`       | Persist the exit code                         |


### Null Transitions

Certain 'transitions' from a mode to the same mode are allowed (and do nothing). These are
called _null transitions_. 

The following table shows in which modes a certain procedure will cause a null transition: 

| Mode         | By Calling   |
| ------------ | ------------ |
| `Running`    | `Run`        |
| `Suspended`  | `Suspend`    |
| `Completed`  | `Stop`       |


### Mode Error

The package `AdaOS.Compartments` contains the following declaration: 

```ada
Mode_Error: exception;
```

Only a transition listed in either of the above two tables (mode transitions and null
transitions) is valid. An attempt at any other transition causes the exception `Mode_Error` to
be propagated. 


### Persistence

A compartment is a system object, and so it has the [persistence](../objects/objects.md#state)
procedures `Create`, `Load`, `Save`, and `Delete`. However, if `Save` or `Delete` is called
when the execution mode is `Vacant` or `Running`, the exception `Mode_Error` is propagated. 

????? Allow `Save` and `Delete` in running mode? `Save` causes implicit `Suspend` first.
`Delete` just kills the compartment with extreme prejudice (returning it to the vacant state).

On the other hand, an unprimed, suspended, or completed compartment can be persisted (saved
into a stream) and subsequently restored (loaded from a stream). 

This will be the basic mechanism for load balancing in a cluster, where running compartments
will be suspended and then saved, so as to be reloaded into a different computer in the
cluster, and resumed (run again). 







????? At the compartment level or at the partition instance level?

????? Is this good? There could be huge amounts of crap in the stack and heap.





### Events

Whenever a compartment's mode transitions to another mode, an event ......

```ada

type Compartment_Mode_Change
is
   new Event_Object
with
   record
      Compartment: Object_Identifier;
      From, To: Compartment_Mode;

   end record;

```





Other events: 

Compartment created, finalised
Property value set








.....


### Awaiting Completion

For convenience, AdaOS provides the procedure `Await_Completion`, which can be called to wait
for a specified compartment to enter the `Completed` mode (by any means). 

```ada
procedure Await_Completion (Compartment: not null access Program_Compartment'Class;
                            Timed_Out:   out Boolean;
                            Timeout:     in  Ada.Calendar.Time := Ada.Calendar.Clock + 1.0);
```

The procedure `Await_Completion` returns when either of: 

 * the given `Compartment` has entered the mode `Completed`, in which case `Timed_Out` is set
   to `False`, or 
   
 * the current time (as obtained by calling `Ada.Calendar.Clock`) is greater than the given
   `Timeout`, in which case `Timed_Out` is set to `True`. 

Once a compartment has entered the `Completed` mode, it cannot change mode other than to
`Vacant` (by a call of `Save` or `Delete`), so when this procedure returns with `Timed_Out`
false, it can be assumed the compartment is completed. There is no race condition. 

This procedure works by listening on the event channel `?????` for the mode change event to
`Completed` for the specified compartment. 



-----------------------------------------------------------------------------------------------
## Exit Code {#exco}

A compartment represents the execution of a [program](programs.md). 

Every compartment has an _exit code_, represented by the property `Exit_Code`, whose value can
be one of:

| Code      | Meaning                            |
| --------- | ---------------------------------- |
| 0         | Completed successfully (no error)  |
| 1 to 255  | Completed with error               |

When a compartment is created, its exit code is initially `Successful` (0), and can be set to
another value unless the compartment is in `Completed` (or `Vacant`) mode. 

The package `AdaOS.Compartments` includes the following visible declarations: 

```ada

type Program_Exit_Code is range 0 .. 255;

Successful: constant Program_Exit_Code := 0;
Failure:    constant Program_Exit_Code := 1;

subtype Error_Exit_Code     is Program_Exit_Code range 1 .. Program_Exit_Code'Last;

function Exit_Code (Compartment: in Program_Compartment) return Program_Exit_Code is abstract;

procedure Set_Exit_Code (Compartment: in out Program_Compartment;
                         Exit_Code:   in     Program_Exit_Code) is abstract;
```

Please refer to the [Advanced Bash-Scripting Guide][4], [`sysexits.h`][5], and the [OpenBSD
Manual Pages][6] for more information about the nascent conventions regarding the use of the
error values 1 to 255. However, the value 1 can always be used as a general 'failed'
indication. 



-----------------------------------------------------------------------------------------------
## Ambit


The compartment maintains a required [property](#prop) that is a set of authorities
which comprise the [ambit](../security/security.md#amb) 

```ada
function Ambit (Compartment: in Program_Compartment) 
return
   Security.Security_Ambit is abstract;

procedure Set_Ambit (.....) is abstract;
```

.....

, but this is only significant on the AdaOS Native platform




-----------------------------------------------------------------------------------------------
## Default Authority


The compartment maintains a required [property](#prop) that is a an authority identifier which is the
_default authority_ of the compartment. 

```ada
function Default_Authority (Compartment: in Program_Compartment) 
return
   Security.Security_Authority is abstract;

procedure Set_Default_Authority (.....) is abstract;
```



, but this is only significant on the AdaOS Native platform



-----------------------------------------------------------------------------------------------
## Accounts

...... `Accounts` ..........

[property](../intro/intro.md#prop)

a set of [accounts](../security/accounting.md)






-----------------------------------------------------------------------------------------------
## Base Transaction

Every compartment has a [property](#prop) that is its _base transaction_. 

.....





-----------------------------------------------------------------------------------------------
## Domain Identifier

Every compartment has a [property](#prop) that is its _domain identifier_. 

???????????






-----------------------------------------------------------------------------------------------
## Clock Offset

Every compartment has a [property](#prop) that is its _clock offset_.

This is a value of type `Duration` which is added to the result from calling the ......





-----------------------------------------------------------------------------------------------
## Program Arguments {#arg}

A compartment has a required [property](#prop) `Arguments`, the _program arguments_ of the
compartment. Program arguments are a way to customise or parametrise the execution of a
program. 

This property is of an access type that designates a constant array of access values
referencing constant strings, from index 1 upwards. The strings are `Wide_Wide_String`. 

The package `AdaOS.Compartments` contains the following visible declarations: 

```ada
type Program_Argument_Array is
   array (Positive range <>) of access constant Wide_Wide_String;

function Arguments (Compartment: in Program_Compartment) 
return 
   access constant Program_Argument_Array is abstract;

procedure Set_Arguments (Compartment: in out Program_Compartment; 
                         Arguments:   access constant Program_Argument_Array) is abstract;
```

The function `Arguments` returns the program arguments of a compartment. It will never return
null, but the array may be empty (of length zero). 

The procedure `Set_Arguments` .....

It is recommended that the character 'NUL' (whose code point is value 0) is never used in the
value of any program argument. The character 'NUL' is used as a string terminator in some
programming languages; an embedded 'NUL' is likely to cause premature string termination in
such languages. 



-----------------------------------------------------------------------------------------------
## Predefined Text Streams {#strm}

A compartment has a [property](../intro/intro.md#prop), `Text_Streams`, that comprises ten
[predefined text streams](streams.md). 

```ada
function Text_Streams (Compartment: not null access Program_Compartment) 
return 
   access constant Predefined_Text_Stream_Array is abstract;

procedure Set_Text_Streams (.....) is abstract;
```

The function `Text_Stream` returns .....



.....



### Redirections {#redir}

It is an often-used technique in [Allegra](?????)---or any command-line or similar shell 
language---to _redirect_ the standard output, standard input, and/or standard error .....

.....


### .....

[standard streams](../adalib/adalib.md#stdstrm) 
defined by Ada and AdaOS, and a procedure that enables
them to be set. 

........



-----------------------------------------------------------------------------------------------
## Environment Variables {#envvar}

An [environment variable](envvars.md) has a name and value that are both strings. The
[property](#prop) `Environment_Variables` of a compartment is a mapping of environment
variables, from names to values. 



????? It is implemented as a standard Ada map container. 



```ada
package Environment_Variable_Maps is
   new Ada.Containers.Indefinite_Hashed_Maps (Unprimeded_Wide_Wide_String, 
                                              Environment_Variable'Class);

function Environment_Variables (Compartment: not null access Program_Compartment) 
return 
   access Environment_Variable_Maps.Map'Class is abstract;

procedure Set_Environment_Variables
   (Compartment: not null access Program_Compartment;
    Map:         access Environment_Variable_Maps.Map'Class)  is abstract;
```

The function `Environment_Variables` returns ..... 

.......



-----------------------------------------------------------------------------------------------
## Sub-Compartments

..... read-only [property](#prop) named `Compartments` .....





-----------------------------------------------------------------------------------------------
## Current Working Directory {#cwd}

A compartment maintains a [property](#prop) `Current_Directory` which is its _current working
directory_, or _CWD_.

This is an absolute [path](../objects/paths.md#cwd) of a
[directory](../objects/containers.md#dir) to which the compartment has access
[permission](../security/security.md#perm). 

Any use by the (program executing within the) compartment of a 
[relative path](../objects/paths.md#absrel) is a way to specify a path relative to the CWD, .....


```ada
function Current_Directory (.....) 
return
   not null access Object_Directory'Class is abstract; 

procedure Set_Current_Directory (.....) is abstract;
```

The function `Current_Directory` returns (an access value referencing) the CWD. 

The procedure `Set_Current_Directory` .....

Like all system objects, the current directory must be [engaged](../objects/objects.md#eng)
before it can be used, and *must* be disengaged as soon as it is no longer needed. 


### Environment Variable `PWD`

Reading the `PWD` [environment variable](envvars.md#cwd) is an alternative way to obtain the
absolute path of the CWD. 


### Windows

On the Microsoft Windows platform, each drive has its own current directory, .....

The package `AdaOS.Compartments.Microsoft_Windows` includes the following visible declarations: 

```ada

type Windows_Compartment is limited interface and Program_Compartment;

type Storage_Drive_Letter is new Character range 'A' .. 'Z';

function Current_Drive (Compartment: in Windows_Compartment) 
return 
   Storage_Drive_Letter is abstract;

.....

function Current_Directory (Compartment: in Windows_Compartment;
                            Drive:       in Storage_Drive_Letter) 
return 
   access Object_Directory'Class;

overriding
function Current_Directory (Compartment: in Windows_Compartment) 
return 
   access Object_Directory'Class;
```
The function `Current_Directory` has an overloading that takes an in-parameter, `Drive`, indicating the
drive ..... The overloading without the `Drive` parameter assumes the current drive. 



### Unix-Based

.....



-----------------------------------------------------------------------------------------------
## System Event Broker

The [property](#prop) `Event_Channels` is a [stock service](#stksvc) which 
..... 

.....





-----------------------------------------------------------------------------------------------
## Stock Programs {#stkprg}

The [property](#prop) `Programs` is a [directory](../objects/containers.md#dir) of the
[stock programs](programs.md#stock) which are accessible to the compartment (to be run). 

......



-----------------------------------------------------------------------------------------------
## Stock Services {#stksvc}

The [property](#prop) `Services` of a compartment is a
[directory](../objects/containers.md#dir) of the [stock services](../services/services.md#stock) 
available to the compartment. 

The stock services are currently: 

| Property              | Name         | Description                                |
| --------------------- | ------------ | ------------------------------------------ |
| `Event_Channels`      | `chan`       | System Event Broker                        |
| ` ` | ` ` |  |
| ` ` | ` ` |  |
| ` ` | ` ` |  |
| ` ` | ` ` |  |

The Name in the above table is the name the service has within the
[stock service directory](../native/dirhier.md#home) `/adaos/serv`. 



-----------------------------------------------------------------------------------------------
## 



-----------------------------------------------------------------------------------------------
## 



-----------------------------------------------------------------------------------------------
## 



-----------------------------------------------------------------------------------------------
## 



-----------------------------------------------------------------------------------------------
## Configuring a Compartment {#config}

A compartment can be configured .......

............



### Environment Variables

An environment variable can be configured using the following command: 

    pxcr image X variable "N=V"
    
where `X` is the name of the executable image, `N` is the name of the environment variable, and 
`V` is the value it should be set to. 

......

...... `DEFAUTH` .......



### Ambits

........


### Accounts

......





### Programs

..........


### Services

A service program can be added to the service directory of the [base compartment](#basecmpt) 

......

A service program can be added to the service directory with the following command: 

    pxcr image X service S program Z
    
where `X` is the executable image, `S` is the name of the service, and `Z` is the name of the 
associated service program. 

The service program must already have been fully configured as a program. 

............

```
pxcr image myorg.myprog program myorg.myservprog add
pxcr image myorg.myprog service myorg.myservice program myorg.myservprog
```

or alternatively:

```
pxcr <<~~~
image myorg.myprog
program myorg.myservprog add
service myorg.myservice program myorg.myservprog
~~~
```


............

Similarly, the [program directory](programs.md#progdirs) that a program should have 
set up is configured ......:

............





-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
## 



.....


-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## .....

.....



-----------------------------------------------------------------------------------------------
## Exports



?????



And this module class exports the following data:

| Name                        | Purpose
| --------------------------- | ---------------------------------------------------------------
| `objects`                   | the [objects](../objects/objects.md) of a compartment
| `instances`                 | the [executional instances]() of a compartment
| `partitions`                | the [partitions](../rts/partitions.md) of a compartment
| `compartments`              | the subcompartments of a compartment
| `members`                   | the [members](?????) of a compartment
| ``                          | 
| ``                          | 
| ``                          | 
| ``                          | 
| ``                          | 
| ``                          | 
| ``                          | 
| ``                          | 
| ``                          | 








-----------------------------------------------------------------------------------------------
## 



-----------------------------------------------------------------------------------------------
## 



-----------------------------------------------------------------------------------------------
## 



-----------------------------------------------------------------------------------------------
## 



```ada


   
   
   
   
   Compartment_Table_Length: 
      constant Natural := 200
   with 
      Export, External_Name => "system.rts.components.table.length";

   type Compartment_Table_Count 
   is 
      range 0 .. Compartment_Table_Length 
   with 
      Export, External_Name => "system.rts.components.table.count";

   subtype Compartment_Table_Index 
   is 
      Table_Count range 1 .. Table_Count'Last
   with 
      Export, External_Name => "system.rts.components.table.index";

   ?????type Compartment_Allocation_Set is array (Table_Index) of Boolean;



   type Compartment_Access is access AdaOS.Security.Principal_Compartment'Class;

   type Compartment_Table: ;


   protected Environment
   with
      Export,
      External_Name => "system.rts.components"
   is
      .....
      
      
      
      
      function Allocated return Table_Count is (Allocation_Counter);
   
   private
      Allocation_Counter: Table_Count := 0;
      Table: array (Table_Index) of Program_Compartment_Access := (others => null);
   end;


   protected type Base_Compartment
   with
      Export, External_Name => "system.compartment"
   is
      new AdaOS.Security.Principal_Compartment
   with
      .....
   private
      Partitions: 
      Streams:          ?????must be system objects? array (Standard_Stream) of Character_Stream_Access;
      Users: 
      Roles: 
      Variables: 
      Ambit: 
      Accounts: 
      Programs: 
      Services: 
      Home: 
      Compartments: 
      Host: 
   end;
   
   
   
   
   

```





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
## References

[1]: <https://www.pathname.com/fhs/> "Filesystem Hierarchy Standard"

[2]: <https://pubs.opengroup.org/onlinepubs/9699919799/>
     "The Open Group Base Specifications Issue 7, 2018 edition, Section 8"

[3]: <https://www.tenforums.com/tutorials/3234-environment-variables-windows-10-a.html>
     "TenForums: Complete List of Environment Variables in Windows 10"

Disclaimer: Windows 10 Forums is an independent web site and has not been authorized, 
sponsored, or otherwise approved by Microsoft Corporation. "Windows 10" and related materials 
are trademarks of Microsoft Corp.

[4]: <https://tldp.org/LDP/abs/html/exitcodes.html> 
     "Appendix E. Exit Codes With Special Meanings"

[5]: <https://gist.github.com/bojanrajkovic/831993> "sysexits.h"

[6]: <https://man.openbsd.org/sysexits.3> "sysexits.h"