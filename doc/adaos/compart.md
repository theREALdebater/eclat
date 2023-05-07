-----------------------------------------------------------------------------------------------
# Compartments

A _compartment_ is a [security](../security/security.md) and
[accounting](../security/accounting.md) boundary, which conceptually contains the system
resources which are accessible to one or more [executional instances](instances.md). It also
represents many (or all) aspects of the 'outside world' to the instances. 

Normally, when a [program](programs.md) is started, a new compartment is created for the
execution of all that program's instances. Alternative arrangements are possible, but not
currently used by AdaOS.

Compartments form a [hierarchy](../intro/hier.md). 


### Compartment Models

A _compartment model_ is a non-system object that, although it is not a compartment, holds all
the information needed to create a compartment, as a set of properties. 

The interface type `Compartment_Model`, declared in the package `AdaOS.Compartments`,
represents a compartment model.

A compartment model is a value object, meaning that it can be copied (in Ada terms, its is
non-limited). Assignment copies a value (object). All of its properties also, therefore, have
value objects as their values, and so can be copied. 


### Compartment Objects

A compartment is a system object. 

The task interface type `Program_Compartment`, declared in the package `AdaOS.Compartments`,
represents a compartment. This type is derived from `System_Object`. 

The function `Model` returns (a remote access value referencing) the underlying model of the
compartment. The model of a compartment is said to be _bound_. All the procedures for changing
a model cannot be used to change a bound model; attempting to call one of these procedure on a
bound model propagates the exception `AdaOS.Compartments.Status_Error`. 

Every [principal](../security/security.md#princ) has a _default compartment model_, which is
used to create the compartment of any program run [vicariously](programs.md#vic) for the
principal. 

The type `Program_Compartment` has a function method named `Create_Compartment`, that creates a
new child compartment of it. One or more compartment models can be passed into this function;
the model of the new child compartment is built up from merging together the properties of the
given models as well as the parent compartment. The way each property is merged is generally
different for each property, and the description for each property includes how its is merged. 



-----------------------------------------------------------------------------------------------
## Program Execution

A compartment is normally created when a [program](programs.md) is started. 

The procedure `Run` declared in the package `Ada.Programs` can be used to run a program. 

This procedure creates a new compartment by calling the `Create_Compartment` of a
compartment model

????? which one?

. 

.....

For the execution of a program, normally, all the [executional instances](instances.md) of the
[assemblies](assemblies.md) of the program share the same compartment, and no other instances
are of this compartment. 

If there are multiple assemblies, they typically correspond to the
[partitions](../eclat/building.md#part) of a multi-partition Ada program. If there is only one
assembly, it may correspond to a single-partition Ada program or a C program. 

Nevertheless, care should be taken not to make assumptions. It is _possible_ for any set of 
instances to have the same compartment, and it is _possible_ for instances arising from the 
same execution of a program to have different compartments. Neither of these situations is 
normal, but possible. 

.....

For the execution of an [external program](programs.md#extprogs), a compartment is created
whose properties and methods mostly replicate the corresponding aspects of the (external)
execution environment of the external program. 



-----------------------------------------------------------------------------------------------
## Co-Compartments

Since the AdaOS Native platform will, in the future, support some (or all) of the assemblies of
a program being executed on different computer systems (of a network), the concept of
_co-compartments_ will be implemented in the future. 

The concept behind co-compartments is that there is a set of compartments, each existing on a
different computer system, which are all effectively and conceptually the same compartment, and
(normally) have different assemblies of the same program running within them. 

This is aimed at providing all the assemblies with the same executional environment regardless 
of which different computer systems they are executing on. 

In order to maintain the fiction that the co-compartments are all effectively the same 
compartment, the co-compartments will communicate with one another. Any change made to one 
co-compartment will be automatically propagated out to the others, so that they all maintain 
the same state. 

However, for practicality, there will be a set of compartment properties that are propagated,
all other properties will not be propagated. The default will be for no properties to be
propagated, so this is an 'opt in' arrangement. 

????? opt-in for all props?

Each program will be configurable as to which compartment properties are propagated (as well as
which class of computer system each assembly is to be executed on). 

This has an effect on the definitive semantics of many of the methods of all compartments. 

.....



-----------------------------------------------------------------------------------------------
## 



-----------------------------------------------------------------------------------------------
## 



-----------------------------------------------------------------------------------------------
## Compartment Ownership {#owner}

Whenever a compartment is created, the compartment is associated with a
[principal](../security/security.md#princ) who/which is the compartment's _owner_. 

A compartment's owner will be the owner of its super-compartment (the one immediately above it
in the compartment hierarchy) unless the compartment was created as a result of the [vicarious
execution](programs.md#vic) of a program.

The ownership of a compartment cannot be changed. In order to achieve this effect, it is
necessary to create a new compartment by executing a program vicariously. 

The _identity_ of a compartment is the identity of the compartment's owner. 



-----------------------------------------------------------------------------------------------
## Top Compartment {#top}

.....

The [top user's](../security/security.md#topuser) default compartment model is called the _top
compartment model_. 

When the [top program instance](?????) is created during the system's 
[boot procedure](../pxcr/booting.md), its compartment is created (solely) from the top 
compartment model. 

.....



The _top compartment_ has no super-compartment. Its owner will always be the 
[top user](../security/security.md#topuser). 

The top compartment contains (system objects that represent) *all* the resources of the
[effective system](../intro/intro.md#effsys). 

.....



-----------------------------------------------------------------------------------------------
## Compartment Properties {#prop}

A compartment model has the following properties: 

 * the [system event broker](../events/events.md#sb); 

 * a set of [redirections](#redir), `Redirections`; 

 * a [domain identifier](../objects/objects.md#oid), `Domain`;  ????? env var?

 * one [principal](../security/security.md#princ) which is the compartment's [owner](#owner)
   and defines the compartment's identity, `Owner`; 

 * a set of [predefined text streams](#strm) available to this compartment, `Text_Streams`;
 
 * a set of [environment variables](#envvar), `Environment_Variables`; 
 
 * a set of [principals](../security/security.md#princ), which are the sub-users and roles of 
   the compartment's owner; ????? implied by the owner?
 
 * an ambit and a [base authority](../security/security.md#baseauth); ????? ambit implied by the owner?
 
 * a [base authority](../security/security.md#baseauth), `Base_Authority`; 
 
 * a set of [accounts](../security/accounting.md), `Accounts`; 
 
 * a directory of [assemblies](assemblies.md) available to this compartment, `Assemblies`; 
 
 * a directory of [programs](programs.md) available to this compartment, `Programs`; 
 
 * a [service directory](../services/services.md) available to this compartment, `Services`; 

 * the owner principal's [home directory](files.md#home); ????? implied by the owner?

 * a set of sub-compartments of this compartment, `Compartments`; 
 
 * optionally, the [host filesystem](files.md#hostfs); ?????
 
 * a [current working directory](#cwd) and a working directory stack, `Current_Directory`; 
 
 * an optional [base transaction](../database/transactions.md#basetran), `Base_Transaction`; 

 * a [temporary directory](../objects/paths.md#temp), `Temporary_Directory`; 

 * a [directory for package installation data](../objects/paths.md#work); ????? implied by program's package? `.Program.Installation.State_Directory`

 * a [directory for package installation specific data within a 
   system](../objects/paths.md#wrks), `Local_State_Directory`; 

 * a [directory for package data](../objects/paths.md#data); ????? implied by program's package? `.Program.Installation.Base_Package.State_Directory`

 * a [clock offset](?????), `Clock_Offset`; 

 * an optional reference to an external process; ?????

 * an optional [progress updater](../services/kronos.md#prog). ????? event channel?

For a compartment model `M`, a property `P` can be read by calling the function `M.P` and set
by calling the procedure `M.Set_P`. 

For a compartment `C`, a model property `P` can be read using the expression `C.Model.P`, but
the property cannot be set because the model is bound (its value might change by other means). 

In addition to its model, a compartment contains the following properties:

 * an _execution mode_, `Mode`; 

 * a reference to the [program](programs.md) of the compartment, `Program`; 

 * a set of the [executional instances](instances.md) of the compartment, `Instances`; 

 * a sequence of [program arguments](#arg) that provide a way of customising the running of a
   program, `Program_Arguments`; 

 * a [status code](#stat), indicating whether the compartment has started or finished
   execution, and, if it has finished, what the outcome was (success or error), `Exit_Code`. 
 
All these properties of a compartment or compartment model are described in the following
sections. 

Many of these properties will not exist or be applicable or appropriate to a compartment
(model), in which case the property will have a null or empty value, and might propagate an
exception if an attempt is made to set it. 


### Summary

Here is a summary of the properties.

Compartment model:

| Name                     | Description                       | Type                        |
| ------------------------ | --------------------------------- | --------------------------- |
| ` `      | External process proxy            |
| `Accounts`               | Accounts                          | ` ` |
| `Assemblies`             |             |
| `Base_Authority`         | Base authority of the compartment | ` ` |
| `Base_Transaction`       |             |
| `Clock_Offset`           |             |
| `Compartments`           | Sub-compartments
| `Current_Directory`      | Current working directory         | `Object_Directory'Class`    |
| `Domain`                 |             |
| `Environment_Variables`  | Environment variables             | ` ` |
| `Event_Channels`         | System Event Broker               | `Event_Broker'Class`        |
| `Owner`                  | Compartment's owner               | ` ` |
| `Programs`               | Available programs                | `Program_Directory'Class`   |
| `Redirections`           |             |
| `Services`               | Available services                | `Service_Directory'Class`   |
| `Text_Streams`           | Standard streams                  | ` ` |
| ` `      |             |
| ` `      |             |

Compartment:

| Name                     | Description                       | Type                     |
| ------------------------ | --------------------------------- | ------------------------ |
| `Assemblies`             | Available executional assemblies  | `Assembly_Directory`     |
| `Instances`              | Executional instances             | ` ` |
| `Program_Arguments`      | Program arguments                 | `Program_Argument_Array` |
| `Program`                | Program of this compartment       | `Program_Access`         |
| `Status`                 | Program exit code                 | `Program_Exit_Code`      |
| `Mode`                   | Compartment mode                  | `Compartment_Mode`       |
| ` `      |             |
| ` `      |             |
| ` `      |             |
| ` `      |             |
| ` `      |             |


These are all described in the following sections.



-----------------------------------------------------------------------------------------------
## Execution Modes {#mode}

At any one time, a compartment can be in one of four _execution modes_:

The package `AdaOS.Compartments` contains a visible declaration of the `Compartment_Mode`
enumerated type whose four values are as follows:

| Mode            | Description                                                                 |
| --------------- | --------------------------------------------------------------------------- |
| `Vacant`        | Has not been created or loaded, or after it has been deleted                |
| `Suspended`     | Can execute, but is not currently doing so                                  |
| `Running`       | Instances are executing                                                     |
| `Completed`     | All instances have completed                                                |

The execution mode is a read-only property of the compartment.

```ada
function Mode (Compartment: not null access Program_Compartment) 
return 
   Compartment_Mode is abstract;
```

The function `Mode` returns the current execution mode of a compartment. 


### Vacant

Whenever the compartment is [vacant](../objects/objects.md#state), its executional mode is
`Vacant`. 


### Suspended

In this mode, none of the [tasks](../pxcr/tasks.md) of any of the [instances](instances.md) of
the compartment are [eligible](?????) for execution, but all of the instances' state is
preserved (unchanged) as its [saved state](../objects/objects.md#state). 


### Running

In this mode, the instance's tasks are eligible for execution, as normal. 


### Completed

This mode indicates that all of the compartment's instances have completed execution (and
therefore there can never be any further execution of any instances in the compartment). 


### Mode Transitions

A compartment's mode can be changed, but there are only certain _mode transitions_ that are
valid: 

| From Mode    | To Mode      | By Calling
| ------------ | ------------ | ------------- 
| `Suspended`  | `Running`    | `Run`
| `Running`    | `Suspended`  | `Suspend`
| `Suspended`  | `Completed`  | `Stop`
| `Running`    | `Completed`  | `Stop`

Any attempt to make a different transition to one of the above fails, with the exception
`AdaOS.Compartments.Mode_Error` being propagated instead. 

The procedure `Suspend` is used to transition into the `Suspended` mode; `Run` into `Running` mode;
and `Stop` into `Completed` mode. These procedures are all primitive operations of the type
`Executional_Instance`. 

.....

```ada

procedure Run     (Compartment: not null access Program_Compartment) is abstract;
procedure Suspend (Compartment: not null access Program_Compartment) is abstract;
procedure Stop    (Compartment: not null access Program_Compartment) is abstract;

Mode_Error: exception;
```

.....


### Persistence



The compartment is a system object, and so its [persistence](../objects/objects.md#state)
procedures `Load`, `Save`, `Create` and `Delete` may be called. However, if `Save` or `Delete`
is called when the execution mode is `Running`, the exception `Mode_Error` is propagated. 



### Awaiting Completion



Whenever an [instance](instances.md) completes



```ada

type Compartment_Completion_Waiter is interface;

function New_Waiter (Compartment: not null access Program_Compartment) 
return 
   access Compartment_Completion_Waiter'Class is abstract;

procedure Await_Completion (Waiter: not null access Compartment_Completion_Waiter) is abstract;
```


.....



-----------------------------------------------------------------------------------------------
## Exit Code {#exitcode}

A compartment represents the execution of a [program](programs.md). 

Every compartment has a _exit code_, whose value can be one of:

| Code      | Meaning                            |
| --------- | ---------------------------------- |
| 0         | Completed successfully (no error)  |
| 1 to 255  | Completed with error               |

Please refer to the [Advanced Bash-Scripting Guide][4], [`sysexits.h`][5], and the [OpenBSD
Manual Pages][6] for more information about the nascent conventions regarding the use of the
error values 1 to 255. However, the value 1 can always be used as a general 'failed'
indication. 

When a compartment is created, its exit code is initially `Successful` (0). 

The package `AdaOS.Compartments` includes the following visible declarations: 

```ada

type Program_Exit_Code is range 0 .. 255;

Successful: constant Program_Exit_Code := 0;
Failure:    constant Program_Exit_Code := 1;

subtype Completed_Exit_Code is Program_Exit_Code range 0 .. Program_Exit_Code'Last; -- ?????
subtype Error_Exit_Code     is Program_Exit_Code range 1 .. Program_Exit_Code'Last;

function Exit_Code (Compartment: in Program_Compartment) return Program_Exit_Code is abstract;

procedure Set_Exit_Code (Compartment: in out Program_Compartment;
                         Exit_Code:   in     Program_Exit_Code) is abstract;
```





?????? this can all go, I think

The function `Status` returns the current value of the status of a compartment.

The procedure `Start` changes the status of a compartment from -2 (Not yet started) to -1
(currently running), and creates all of the compartment's instances .....

Calling this procedure when the status is anything other than -2 propagates the
`Bad_Compartment_Status` exception (and does nothing else). 

The procedure `Stop` changes the status of a compartment from -1 (Currently running) to a
completion status (between 0 and 255), and terminates and then deletes all of the compartment's
instances .....

Calling this procedure when the status is anything other than -1 propagates the
`Bad_Compartment_Status` exception (and does nothing else). 

It is important that the [main assembly](?????) calls its compartment's procedure `Stop`, with
an appropriate `Final_Status` value, when it is ready to complete its execution. Calling `Stop`
will cause the termination of itself and all the compartment's other instances. However,
non-main instances are actually free to self-terminate, or to be terminated, and even to be
created or re-created, without affecting the termination status of the compartment itself. 

The procedure `Await_Completion` returns (only) when the compartment's status becomes a
completion status (0 to 255), which implies that all the executional instances of the
compartment have terminated. 

The subtype `Completed_Status` can be used to test if the compartment's status is a completion
status (0 to 255). 

Beware of race conditions using this kind of test; it is safe to rely on 

    C.Status in Completed_Status

 being `True`, because it cannot change to `False`. It is not safe to rely on it being `False`,
because it could change to `True` at any time; in these situations, use `Await_Completion`
instead. 












### Active-Immutable Properties {#actimm}

????? I think this can go - properties of a bound model cannot be set

Many properties of a compartment are _active-immutable_. The value (or any sub-property or
other characteristic) of an active-immutable property cannot be changed unless the compartment
is in status 'Not yet started' (-2). 

The exception `Bad_Compartment_Status` is propagated if an attempt is made to modify
active-immutable property of a compartment which is not 'Not yet started'. 


.....




-----------------------------------------------------------------------------------------------
## Compartment Program {#prog}

A compartment has the _program_ property. This is the [program](programs.md) which was run to
create the compartment. 

```ada
function Program (Compartment: in Program_Compartment) return Program_Access;
```

...... function `Program` ......

The program of a compartment is immutable. It is set when the compartment is created and never
changes. 

This property is the property of a compartment, *not* of the compartment's model. If the `Run`
procedure is used to create a compartment, the new compartment's program will be the value of
the `Program` parameter passed into the procedure. 



-----------------------------------------------------------------------------------------------
# Stowage {#stow}



????? review this



A compartment is a [stowable object](../objects/objects.md#stow), and can be stowed in order to
convert all the state of the compartment (and everything in it) into a file (a piece of data),
that can be stored for a relatively long time and then unstowed again in order to restore the
compartment and its contents to its previous state. 

.....



-----------------------------------------------------------------------------------------------
## 














-----------------------------------------------------------------------------------------------
## Program Arguments {#arg}

A compartment has a _program arguments_ property. The arguments are a way to customise or
parametrise the execution of the program. 

.....

This property is a vector (a dynamically resizable array) of strings, from index 1 upwards. The
strings are held as a standard Ada vector container of `Wide_Wide_Unbounded_String` values. 

The package `AdaOS.Containers` contains the following visible declarations:

```ada
package Program_Argument_Vectors is
   new Ada.Containers.Vectors (Positive, Wide_Wide_Unbounded_String);

function Arguments (Compartment: access Program_Compartment) 
return 
   Program_Argument_Vectors.Vector;

procedure Set_Arguments (Compartment: access Program_Compartment; 
                         Vector:      in     Program_Argument_Vectors.Vector); -- ????? needed?
```

The function `Arguments` returns the program arguments of a compartment. 

The procedure `Set_Arguments` sets the program arguments of a compartment. ????? needed?

It is recommended that the character 'NUL' (whose code point is value 0) is never used in the
name or the value of any program argument. The character 'NUL' is used as a string terminator
in some programming languages; an embedded 'NUL' is likely to cause premature string
termination in such languages. 



????? don't need this - they are set when program is run (comp is created) and remain immutable

The object (a vector) containing program argument values of a compartment is an
[active-immutable](#actimm) property. The object this access value references cannot be changed
when the compartment is not in the Preparatory (-2) status. However, the program arguments kept
in the vector can change at any time (they can be added, deleted, and their values changed). 

There is no concurrency control provided for access to the program arguments in the vector.
This will probably need to be provided by a higher layer of software. The AdaOS implementation
of the standard library `Ada.Program_Arguments` does not allow the arguments to be changed, and
so it provides read-only access without (the need for) any concurrency control. 



-----------------------------------------------------------------------------------------------
## Predefined Text Streams {#strm}

A compartment model has a property that comprises ten [predefined text streams](streams.md). 

```ada
function Text_Streams (Compartment: access Program_Compartment) return Predefined_Text_Stream_Array;

procedure Set_Text_Streams (Compartment: access Program_Compartment;
                            Streams:     in     Predefined_Text_Stream_Array);
```

These streams are all [system objects](../objects/objects.md) of a type derived from the 
interface `Input_Text_Stream` or `Output_Text_Stream`, declared in the package 
`AdaOS.Objects.Text`. 

.....




????? don't need this - they are set when program is run (comp is created) and remain immutable

The predefined text streams are an [active-immutable](#actimm) property. Calling
`Set_Text_Streams` when the compartment's status is not Preparatory (-2) will propagate the
exception `Bad_Compartment_Status`. 

Reading from and writing to the streams can be done at any time. 

There is no concurrency control provided for access to the streams. This will probably need to
be provided by a higher layer of software, for example the AdaOS implementation of the standard
libraries for text I/O. 


### Redirections {#redir}

It is an often-used technique in [Allegra](?????)---or any command-line or similar shell 
language---to _redirect_ the standard out, standard input, and/or standard error .....

.....


### .....

[standard streams](../adalib/adalib.md#stdstrm) 
defined by Ada and AdaOS, and a procedure that enables
them to be set. 

........



-----------------------------------------------------------------------------------------------
## Environment Variables {#envvar}

An [environment variable](envvars.md) has a name and value that are both strings. The
_environment variables_ property of a compartment is a mapping of environment variables, from
names to values. It is implemented as a standard Ada map container. 

```ada
package Environment_Variable_Maps is
   new Ada.Containers.Indefinite_Hashed_Maps (Unbounded_Wide_Wide_String, 
                                              Environment_Variable'Class);

function Environment_Variables (Compartment: access Program_Compartment) 
return 
   Environment_Variable_Maps.Map;

procedure Set_Environment_Variables (Compartment: access Program_Compartment;
                                     Map:         in     Environment_Variable_Maps.Map);
```




The function `Environment_Variables` returns ..... 

.......





????? don't need this - the map is set when program is run (comp is created) and remains immutable (although its contents are not)


The object (a map) containing the environment variables of a compartment is an
[active-immutable](#actimm) property. The object this access value references cannot be changed
when the compartment is not in the Preparatory (-2) status. However, the environment variables
kept in the map can change at any time (they can be added, deleted, and their values changed). 

There is no concurrency control provided for access to the environment variables in the map.
This will probably need to be provided by a higher layer of software, for example the AdaOS
implementation of the standard library `Ada.Environment`. 



-----------------------------------------------------------------------------------------------
## Services

The property `Services` is a [directory](../objects/containers.md#dir) of the
[services](../services/services.md) available to the compartment. 

The type of this directory is in `Service_Directory'Class`. The concrete limited private type
`Service_Directory`, derived from `AdaOS.Objects.Object_Directory`, is declared visibly in the
package `AdaOS.Services`. 

This kind of directory has some extra information in its [metadata](../objects/objects.md#meta). 

The package `AdaOS.Services` declares the abstract tagged non-limited type `Service_Metadata`,
which is derived from `AdaOS.Objects.Member_Metadata`. The metadata produced by a service
directory must be derived from `Service_Metadata`, which adds the following components: 

 * `Service_Program`, a link to the [service program](../services/servprog.md) which
   implements the service;

 * `Mode`, the mode of the service program (at the time the metadata was extracted); 

 * `Implementor`, a link to the compartment which implements the service (or null if the
   service is dormant). 

......






-----------------------------------------------------------------------------------------------
## Current Working Directory {#cwd}

A compartment maintains a _current working directory_, or _CWD_, of the compartment, which is
an absolute [path](../objects/paths.md) of a [directory](../objects/containers.md#dir) to which
the compartment has full access. 

Any use by the (program executing within the) compartment of a [relative
path](../objects/paths.md#absrel) is a way to specify a path relative to the CWD, .....




The function `Current_Directory` returns (an access value referencing) the CWD. 

The procedure `Set_Current_Directory` 




Reading the `PWD` [environment variable](envvars.md#cwd) is an alternative way to obtain the absolute path
of the CWD. 

Like all system objects, the current directory must be [engaged](../objects/objects.md#eng)
before it can be used, and *must* be disengaged as soon as it is no longer needed. 


### Windows

On the Microsoft Windows platform, each drive has its own current directory, .....

The package `AdaOS.Compartments.Microsoft_Windows` includes the following visible declarations: 

```ada

type Windows_Compartment is abstract limited new Program_Compartment with private;

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


### Shell Commands

[Allegra](?????) defines commands related to the CWD and the working directory
stack. 

Allegra (not the compartment) maintains a _working directory stack_, which is a vector (a
dynamically resizing array) of the saved paths of working directories. 

Here are the main commands, and a brief description of what they do: 

    pwd
    
Prints the absolute path of the CWD onto the current output. 

    cd P

Changes the CWD to path `P` (absolute or relative). 

    push P
    
Pushes (appends) the current path of the CWD onto (end of) the working directory stack, and
then changes the CWD to path `P`. 

    pop
    
Pops (fetches and then deletes) a path off the top (end) of the working directory stack and
changes the CWD to that path. 
    

### Ada

.....

```ada
with Ada.Directories;
...
   Put_Line (Ada.Directories.Current_Directory);
```

.....

```ada
with AdaOS.Instances;
use  AdaOS.Instances;
...
   Put_Line (Task_Instance.Compartment.Current_Directory.Path);
```



.....

```ada



```

.....

```ada



```



-----------------------------------------------------------------------------------------------
## Home Directory {#homedir}

........ `Home_Directory` ..... [system root](files.md#sysroot) .....





-----------------------------------------------------------------------------------------------
## System Root Directory {#sysroot}

........ `Root_Directory` ..... [system root](files.md#sysroot) .....




### AdaOS Native

..... the system root directory is always the same object as the home directory .....





### Windows

There is a specially declared abstract type representing compartments of the 
[Windows platform](?????) .....

The package `AdaOS.Compartments.Microsoft_Windows` includes the following visible declarations: 

```ada

type Windows_Compartment is abstract limited new Program_Compartment with private;

type Storage_Drive_Letter is new Character range 'A' .. 'Z';

function System_Drive (Compartment: in Windows_Compartment) 
return 
   Storage_Drive_Letter is abstract;

function Current_Drive (Compartment: in Windows_Compartment) 
return 
   Storage_Drive_Letter is abstract;

.....

function Root_Directory (Compartment: in Windows_Compartment;
                         Drive:       in Storage_Drive_Letter) 
return 
   access Object_Directory'Class;

overriding
function Root_Directory (Compartment: in Windows_Compartment) 
return 
   access Object_Directory'Class;
```

The function `Root_Directory` has an overloading that takes an in-parameter, `Drive`, indicating the
drive ..... The overloading without the `Drive` parameter assumes the system drive. 




-----------------------------------------------------------------------------------------------
## Owner

The [principal](../security/security.md#princ) which is the compartment's [owner](#owner) .....

The user or role name of the owner is available as the values of 
[environment variables](envvars.md#user) .....


-----------------------------------------------------------------------------------------------
## Ambit and Base Authority


The compartment maintains a set of authorities which comprise the [ambit](../security/security.md#amb) 

, but this is only significant on the AdaOS Native platform

The ambit is accessible (to get or set) as the values of the
[environment variable](envvars.md#amb) `AMBIT`

The ambit of a compartment is an [active-immutable] property. 





The [base authority](../security/security.md#baseauth) is the first value specified in the
environment variable `AMBIT`.



-----------------------------------------------------------------------------------------------
## User and Role Identities



?????



Under a [hosted platform](?????), .....

On the AdaOS Native platform, the [user and role identities](../security/security.md#princ) .....



-----------------------------------------------------------------------------------------------
## Accounts

...... `Accounts` ..........

a set of [accounts](../security/?????)




-----------------------------------------------------------------------------------------------
## Executional Instances

A compartment maintains the set of [executional instances](instances.md) of the
[assemblies](assemblies.md) of the [program](programs.md) whose execution gave rise to the
compartment. 

.....



-----------------------------------------------------------------------------------------------
## Assemblies

A compartment maintains a directory of the [assemblies](assemblies.md) which are accessible to
the compartment. 

.....



-----------------------------------------------------------------------------------------------
## Programs

The property `Programs` is a map of the [programs](programs.md) which are accessible to the 
compartment, indexed by name. 

......



-----------------------------------------------------------------------------------------------
## Subcompartments

A compartment maintains a set of subcompartments as part of its state. 

......



-----------------------------------------------------------------------------------------------
## External process proxy

.....



| `Process`      |             | `External_Process'Class`              |


-----------------------------------------------------------------------------------------------
## Progress updater

.....

[progress updater](../services/kronos.md#prog)

..... `Progress` ..... `Progress_Updater'Class`              |


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



-----------------------------------------------------------------------------------------------
## Top Compartment {#top}

The [run time system](rts.md), during its initialisation, creates a compartment called the _top 
compartment_, .............

During its initialisation, the RTS populates the top compartment with 
objects and values based on its own built-in defaults and some configuration data passed to 
it in the [run time system configuration module](?????). 

........



### Standard Streams

Some platforms will be aimed at embedded and small 'headless' computers/machines/devices, 
which might have limited memory, limited (or no) secondary storage, and very limited means to 
perform any kind of text I/O. 

Therefore, if the platform offers no streams, the `Streams` array's elements will all be null. 


### Environment Variables

The directory `env` contains the [environment variables](envvars.md). 

Some of these are configurable constant values, with defaults. These are as follows. 

| Name                     | Default Value                                                   |
| ------------------------ | --------------------------------------------------------------- |
| `APPDATA`                |  |
| `LOCALAPPDATA`           |  |
| `PROGRAMDATA`            |  |
| `RANDOM`                 | (Dynamic function)
| `RANDOM_MIN`             | `0`
| `RANDOM_MAX`             | `32767`
| `PWD`                    | `/`
| `CWD`                    | (linked to 'PWD')
| `CD`                     | (linked to 'PWD')
| `TMPDIR`                 | (see table below)
| `TEMP`                   | (linked to `TMPDIR`)
| `TEMPDIR`                | (linked to `TMPDIR`)
| `TMP`                    | (linked to `TMPDIR`)
| `TMPDIR`                 | (linked to `TMPDIR`)
| `HOME`                   | (Dynamic function)
| `HOMEDRIVE`              | (Dynamic function)
| `HOMEPATH`               | (Dynamic function)
| `USER`                   | 
| `SESSIONAME`             | 
| `USERDOMAIN`             | 
| `USERNAME`               | 
| `USERPROFILE`            | 
| `ROLE`                   | 
| `BASEAUTH`               | `0` (the top user)
| `DATETIME`               | (Dynamically set)
| `OS`                     | `AdaOS`
| `SYSTEMDRIVE`            | `/dev/c`
| `SYSTEMROOT`             | `/`
| `NUMBER_OF_PROCESSORS`   | (number of cores/processors)
| `PROCESSOR_ARCHITECTURE` | 
| `PROCESSOR_IDENTIFIER`   | 
| `PROCESSOR_LEVEL`        | 
| `PROCESSOR_REVISION`     | 

Many environment variables have dynamic functions. These are set up as follows. 

The default getter functions are all declared in the visible part of the package `AdaOS.?????`, of type
`Environment_Variable_Getter`. 



    function Environment_Variable_Function is access function return Wide_String;

| Name                     | Default Getter Function          | Default Getter Behaviour     |
| ------------------------ | -------------------------------- | ---------------------------- |
| `APPDATA`                |  |
| `LOCALAPPDATA`           |  |
| `PROGRAMDATA`            |  |
| `RANDOM`                 | `Random_Integer`
| `RANDOM_MIN`             | `Random_Integer_Min`             | Returns `0`                  |
| `RANDOM_MAX`             | `Random_Integer_Max`             | Returns `32767`              |
| `PWD`                    | `Current_Directory_Path`
| `CWD`                    | `Current_Directory_Path`
| `CD`                     | `Current_Directory_Path`
| `TMPDIR`                 | (see table below)
| `TEMP`                   | (linked to `TMPDIR`)
| `TEMPDIR`                | (linked to `TMPDIR`)
| `TMP`                    | (linked to `TMPDIR`)
| `TMPDIR`                 | (linked to `TMPDIR`)
| `HOME`                   | (Dynamic function)
| `HOMEDRIVE`              | (Dynamic function)
| `HOMEPATH`               | (Dynamic function)
| `USER`                   | 
| `SESSIONAME`             | 
| `USERDOMAIN`             | 
| `USERNAME`               | 
| `USERPROFILE`            | 
| `ROLE`                   | 
| `BASEAUTH`               | `0` (the top user)
| `DATETIME`               | (Dynamically set)
| `OS`                     | `AdaOS`
| `SYSTEMDRIVE`            | `/dev/c`
| `SYSTEMROOT`             | `/`



| Name                     | Default Getter Function          | Default Setter Procedure     |
| ------------------------ | -------------------------------- | ---------------------------- |
| `APPDATA`                |  |
| `LOCALAPPDATA`           |  |
| `PROGRAMDATA`            |  |
| `PWD`                    | `Current_Directory_Path`         | `Set_Current_Directory_Path` |
| `CWD`                    | `Current_Directory_Path`         | `Set_Current_Directory_Path` |
| `CD`                     | `Current_Directory_Path`         | `Set_Current_Directory_Path` |
| `TMPDIR`                 | (see table below)
| `TEMP`                   | (linked to `TMPDIR`)
| `TEMPDIR`                | (linked to `TMPDIR`)
| `TMP`                    | (linked to `TMPDIR`)
| `TMPDIR`                 | (linked to `TMPDIR`)
| `HOME`                   | (Dynamic function)
| `HOMEDRIVE`              | (Dynamic function)
| `HOMEPATH`               | (Dynamic function)
| `USER`                   | 
| `SESSIONAME`             | 
| `USERDOMAIN`             | 
| `USERNAME`               | 
| `USERPROFILE`            | 
| `ROLE`                   | 
| `BASEAUTH`               | `0` (the top user)
| `DATETIME`               | (Dynamically set)
| `OS`                     | `AdaOS`
| `SYSTEMDRIVE`            | `/dev/c`
| `SYSTEMROOT`             | `/`

The `TMPDIR` environment variable is set as follows. 

| Platform     | Directory                               |
| ------------ | --------------------------------------- |
| Windows      | `C:\Users\U\AppData\Roaming`            |
| Linux/FHS    | `/H/.config/P`                          |
| AdaOS Native | `/tmp`                                  |

### Ambit

The [ambit](../security/security.md#ambit) is set to simply the top user (authority 0).
However, since the top user is superior to all other authorities, this ambit could not
(meaningfully) include any other authorities anyway. 

This ambit means that the top compartment has totally unrestricted access to everything in the
[effective system](../intro/intro.md#effsys).


### Accounts

?????


### Programs

The available programs are the image's [configured programs](programs.md#config). 


### Services

The available services comprises the base services and the configured [controlled
services](../services/servprog.md). 


### Host Filesystem

........ `host`


### Home Directory

......... `home`


### Subcompartments

There are no sub-components.


### Example Configuration

.....

```xml
<pxcr .....>
   <image .....>

      <module name="" uuid="" ..... />
      .....

      <envvar name="LANG"> en_US.UTF-8 </envvar>

      <assembly name=""></assembly>

      <program name="">
      
      
      
      </program>

      <service name="">
        <service-program name=""/>
      </service>

      .....

   </image>
</pxcr>
```










-----------------------------------------------------------------------------------------------
## Base Compartment {#basecmpt}

The _base compartment_ is, for a hosted platform, the [host compartment](#hostcmpt).

On the AdaOS Native platform, the base compartment depends on the 
[authority](../security/security.md#auth) of the compartment (called the 
_parent_) that is starting the execution of another program (whose compartment will be 
the _child_):

 * for the top authority, the base compartment is the top compartment; 
 
 * any other authority has a base compartment defined for it. 







-----------------------------------------------------------------------------------------------
## Host Compartment {#hostcmpt}

On hosted platforms, the RTS creates a compartment, the _host compartment_, which inherits 
from the [top compartment](#topcmpt) but then adds in further objects which represent aspects 
of the host operating system. 

The AdaOS Native platform has no host compartment. 

.....


### Standard Streams

The standard streams are initialised as follows. 

| Name         | Initialised As        
| ------------ | ------------------------------------------------------------------------------
| `stdout`     | host's standard output
| `stdin`      | host's standard input
| `stderr`     | host's standard error
| `stdprt`     | output stream into file whose name is in `STDPRT`
| `stdaux`     | output stream into file whose name is in `STDAUX`
| `stdlog`     | output stream into file whose name is in `STDLOG`
| `stddbg`     | output stream into file whose name is in `STDDBG`
| `stdsys`     | output stream into file whose name is in `STDSYS`

The expression "whose name is in `V`" means that, if the environment variable `V` exists and 
is not blank, the variable is expected to contain the full absolute path of a file. That file 
is created, if it doesn't already exist, and opened for appending (writing) a text stream. 

The members `stdauxin` and `stdauto` are absent. 


### Environment Variables

The host compartment's variables are copied. 

This means that `BASEAUTH` is not set, which is appropriate for hosted platforms. 


### Ambit

The ambit has only one member, named `top`, which identifies the [top 
authority](../security/security.md#auth), which is the only authority in use on a hosted 
platform. 


### Accounts

??????


### Programs

Inherited from the top compartment. 


### Services

Inherited from the top compartment. 


### Compartments

Inherited from the top compartment, but with a new member, named `host`, which represents this 
(the host) compartment. 


### Host Filesystem

........


### Home Directory

.........



-----------------------------------------------------------------------------------------------
### Top Compartment on the AdaOS Native Platform {#adaos}

.........


### Standard Streams

?????The [MVTC service](../services/mvtc.md) can be configured to add a host compartment 
(named `?????` per convention), which will have `stdout` and `stdin` in its 
`?????/streams` .......

.......

 `stderr` 

.......

 `stdprt`
.......

 `stdaux`
 `stdauxin`
.......

 `stdlog`
.......

 `stdauto`
.......

 `stddbg`
.......

 `stdsys`


### Environment Variables

The environment variable `BASEAUTH` should be [configured appropriately](?????). 

The variable `HOME` is set to `/` as this (the root directory on other platforms) is always 
the home directory for any principal on AdaOS. 

The variable `HOMEDRIVE` is not set. 

The variable `HOMEPATH` is set to be an alias of (i.e. a link to) the variable `HOME`. 


### Ambit

?????


### Accounts

?????


### Programs

Inherited from the base compartment. 


### Services

Inherited from the base compartment. 


### Environments

Inherited from the base compartment, but with a new member, named `host`, which represents the 
host compartment. 

...........


### Host Filesystem

........


### Home Directory

.........



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

...... `BASEAUTH` .......



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
| `assemblies`                | the [assemblies](../rts/assemblies.md) of a compartment
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
      Assemblies: 
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