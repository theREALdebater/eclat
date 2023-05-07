-----------------------------------------------------------------------------------------------
# Run Time System (RTS)

The _run time system_, or _RTS_, is the machine-architecture dependent portion of the software 
that programs need to enable them to execute. 

The RTS 



?????In fact, a single [platform](../pxcr/targets.md#plat) could have more than one different RTS 
available. The selection of a RTS is part of the selection of a realisation target. 

.....

There will be one or several modules that support each different RTS (of each different 
machine architecture and host operating system, if any). 


### Hosted and Standalone RTS

ECLAT can target two kinds of [platform](../pxcr/targets.md#plat): 

 * a _hosted platform_, with an underlying host operating system, such as Windows or Linux; 
 
 * a _bare metal platform_, which is entirely in the RTS, such as the [AdaOS
   Native](../native/native.md) platform. 
 
The RTS on a hosted platform merely provides a (usually quite thin) binding to another 
operating system or executional support subsystem, 

?????which is termed the _host operating system_ 
of the platform. 

On the other hand, for a bare metal platform, the RTS is booted into directly, typically from 
the computer's initial boot mechanism (usually stored in some non-volatile memory) or a 
'bootloader' program. 

The RTS is intended to be very low-level and quite minimal. The considerable amount of other 
functionality that most modern programs will expect to be able to call upon must be supplied by 
other means, such as [plugins](../pxcr/plugins.md) and [services](../services/services.md). 

.....


-----------------------------------------------------------------------------------------------
## Usage of the RTS

?????There are three things that need RTS support: 

 * the Realizor
 
 * Low-Level Modules
 
 * the Compiler
 

### Realizor

?????The choice of RTS, besides the machine architecture and host operating system, if any, can 
have implications for the machine code emitted by the Realizor. 

?????The Realizor must therefore know which RTS is being targeted and may need to change the 
machine code it emits as necessary. Sometimes it will be able to emit code to directly achieve 
an action (e.g. a semaphore lock) and at other times it will need to call a subroutine (or 
expand a macro). 

For any one RTS, there will either be:

 * one RTS module, or 
 
 * one main RTS module, with one or more other anciliary RTS modules in order provide extra 
   choices ?????(that the Realizor does not need to be concerned with). 

?????The Realizor targets the main (or only) RTS module; only the main RTS module knows anything 
about the anciliary RTS modules. 


### Low-Level Modules

There will some low-level modules, such as device drivers for example, which will need to have 
a different version for each different RTS. 

This will normally only affect the AdaOS Native platform, but it might affect hosted platforms. 

.....


### Compiler 

????? Does it really need to do this?

When ECLAT builds a [module](../pxcr/modules.md), there are many language constructs which
cannot be translated directly into pseudo-code. Instead, ECLAT generates calls to subroutines
exported by the RTS. 

.....



-----------------------------------------------------------------------------------------------
## Main RTS Module

The RTS is expected to export all the subroutines and constant data [required by the 
Realizor](../pxcr/realizor.md#exports), low-level modules, and compilers. 

The main RTS module is in the [module class](../pxcr/modules.md#modlcass) named: 

    system.rts

This module class defines the following subroutines and macros: 

| Name                        | Purpose
| --------------------------- | ---------------------------------------------------------------
| `execute`                   | Execution of the RTS (and therefore everything else)
| `register_assembly`         | Register an [assembly](programs.md#registry)
| ``                          | 
| ``                          | 
| ``                          | 
| ``                          | 
| ``                          | 
| ``                          | 
| ``                          | 
| ``                          | 
| ``                          | 





????? In Booting instead?

This module class defines the following constants:

| Name                        | Purpose
| --------------------------- | ---------------------------------------------------------------
| `` |  |
| `` |  |
| `` |  |
| `` |  |
| `` |  |

This module class defines the following data:

| Name                        | Purpose
| --------------------------- | ---------------------------------------------------------------
| `top_instance`              | the [top instance](instances.md#top) 
| `top_compartment`           | the [top compartment model](?????) 
| ``                          | 
| ``                          | 
| ``                          | 
| ``                          | 
| ``                          | 
| ``                          | 
| ``                          | 
| ``                          | 
| ``                          | 
| ``                          | 





















The PXC for the `system.rts` module class is:

```pxc
system.rts.compartments.table.length = 200;


$EXPORT $DATA system.rts.compartments: 
   system.struct(system.rts.compartment.[
      allocated: 
         system.ptr(system.array(int(?????, 0), [int(system.len(system.rts.compartment.table), 0)])),
      table: system.struct(system.rts.compartment.table.[
         length: {0, $.uns.max},
         values: @{1, system.config.limits.compartments} * (
            object: {1, system.config.limits.objects},
            ?????
            objects: (
               allocated: @{1, table.length} * {0, 1}
               table: (
                  length: {0, $.uns.max},
                  values: @{1, system.config.limits.objects} * (
                     ?????
                  ),
               ),
            ),
            instances: (
               table: (
                  length: {0, $.uns.max},
                  values: @{1, system.config.limits.instances} * (
                     object: {1, system.config.limits.objects},
                     ?????
                  ),
               ),
               allocated: @{1, table.length} * {0, 1}
            ),
            assemblies: (
               table: (
                  length: {0, $.uns.max},
                  values: @{1, system.config.limits.assemblies} * (
                     object: {1, system.config.limits.objects},
                     ?????
                  ),
               ),
               allocated: @{1, table.length} * {0, 1}
            )
         ),
      ),
   ),
)
```











-----------------------------------------------------------------------------------------------
## Run Time System Configuration Helper {#helper}



????? In Booting?



The _Run Time System Configuration Helper_ is an [ECLAT helper](../eclat/eclat.md#helpers) 
which, as part of the [build process](../eclat/building.md) within ECLAT, 
generates a _system configuration module_, which is in the [module class](../pxcr/modules.md#modlcass) 
named: 

    system.config

This module class exports the following data: 

| Name                        | Purpose
| --------------------------- | ------------------------------------------------------------------
| `limits`                    | lengths of the [system tables](#systab)
| `fixed_assemblies`          | the [fixed assemblies](../rts/assemblies.md#fixed)
| `initial_assembly`          | the [initial assembly](../rts/assemblies.md#initasm)
| ``                          | 
| ``                          | 
| ``                          | 
| ``                          | 
| ``                          | 













??????If `Na` is the number of fixed assemblies, and `Ni` is the number of arguments of the initial 
assembly, the PXC for these exports is:

Ada

```pxc
system.config: (
   limits: (
      objects: {0, $.uns.max},
      compartments: {0, $.uns.max},
      instances: {0, $.uns.max},
      assemblies: {0, $.uns.max},
      ?????: {0, $.uns.max},
   ),
   fixed_assemblies: (
      table: {1, Na} * (
         ?????
      ),
      ?????
   ),
   initial_assembly: (
      id: {1, limits.assemblies},
      arguments: {1, Ni} * @(
         length: {0, $.uns.max}, 
         chars: @{0, length} * {0, $.uns16.max}
      ),
      compartment: {1, limits.compartments}
   )
)
```












-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## Modules

The RTC comprises many modules, but it always has one module, a 

[base module](?????)

which exports ......

.....




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## System Tables {#systab}

The RTS has several _system tables_, each of which is an array, of a fixed size, that contains 
essential information for the RTS.











????? AdaOS?

The _system compartment table_, or SCT, contains information about [compartments](compart.md). 
Each non-deleted compartment is uniqely identifed by a _compartment number_, which is an 
integer between 1 and the _SCT limit_. The array is indexed by compartment number. 

The _system instance table_, or SIT, contains information about [executional instances]
(programs.md). Each non-deleted executional instance is uniqely identifed by an _instance 
number_, which is an integer between 0 and the _SIT limit_. The array is indexed by instance 
number. 



?????instances within the compartment




The information kept by each element of the SCT is:

 * the [object identifier](objects.md) of the compartment; 
 
 * .......
 
The information kept by each element of the SIT is:

 * the [object identifier](objects.md) of the instance; 
 
 * .......
 
For each system table, there is also an _allocation table_, which is a bit array that has the 
value `True` for every element in the system table which is in use at the time. 

When a compartment or executional instance is created, it is allocated an element in the 
corresponding system table using its associated allocation table. When an element of the 
system table is deallocated, its element in the corresponding allocation table is set to 
`False`, indicating that element is available for re-use. 

.....

If an attempt is made to allocate a compartment, by making a call to `?????`, 
when the SCT is full, the call fails (it propagates the exception ????? instead), 
and also a `Compartment_Table_Full` event is sent to the `system` event channel. 

If an attempt is made to allocate an element in the SIT, by making a call to `Create_Instance`, 
when the SIT is full, the call fails (it propagates the exception ????? instead), 
and also an `Instance_Table_Full` event is sent to the `system` event channel. 




The [`eclat-rts` command-line tool](?????) allows the user to configure many of the values 
that are put into the [run time 
system configuration module](#helper) .....

The SCT limit (the length of the SCT) can be configured as `Nc` by the command: 

    limit compartments Nc
    
or

    lim com Nc

where `Nc` is a decimal integer between 1 and some arbitrary upper limit. 

The default value is 100 currently (Oct 2021).

The SIT limit (the length of the SIT) can be configured as `Ni` by the command: 

    limit instances Ni
    
or

    lim ins Ni

where `Ni` is a decimal integer between 1 and some arbitrary upper limit. 

The default value is 100 currently (May 2021).


















-----------------------------------------------------------------------------------------------
## Fulfilments and Requirements of the RTS

The RTS base module fulfils the [module class](../pxcr/modules.md#class): 

    system.rts
    
?????The RTS has no requirements. 

Since all other modules, in any build, will have a requirement for `system.rts`, the RTS base 
module will inevitably be initialised before any other module. 

.....






-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## System Startup {#startup}

.....

..... [boot process](booting.md) ......

When the RTS runs, it .....



 1. RTS Initialisation
 
 2. Initial Assembly Execution
 
 3. RTS Shutdown
 
For every procedure called, if an exception is propagated out, .....

.....



-----------------------------------------------------------------------------------------------
## RTS Initialisation {#init}

..... The RTS initialises itself.

 1. ......
 
 2. ?????AdaOS: The fixed assemblies are [registered](assemblies.md#registry); 
 
 3. ......



-----------------------------------------------------------------------------------------------
## Initial Assembly Execution







????? AdaOS:

..... creates a new [compartment](compart.md), the[top compartment](compart.md#top), .....




A new executional instance, the [top instance](instances.md#top), is created for the [initial assembly](assemblies.md#initasm)
, and the 
initial assembly is executed. 

The top instance will always have [SIT](#sit) number 1, and will remain ..... for the execution of the entire .....

If the initial assembly name were in a variable `N`, the initial assembly is retrieved from the 
registry, into a variable `A` say, by calling:

    A: AdaOS.Execution.Program_Assembly'Class := AdaOS.Assemblies.Registry (N);
    
Then the top instance, in a variable named `T` say, is obtained by calling: 

    T: AdaOS.Execution.Executional_Instance := A.Create_Instance;
    
Now the RTS sets the program arguments, environments, and possibly other properties, of `T`, 
and then transitions `T` into Running mode. 
 
.....



-----------------------------------------------------------------------------------------------
## RTS Shutdown

.....



-----------------------------------------------------------------------------------------------
## System Shutdown (#shutdown)

_System shutdown_ is initiated when the [top instance](#topinst) terminates (goes into the 
Terminated mode). 

......

 1. .....
 
 2. .....
 
 3. .....
 
 4. The RTS performs its _final cleanup_ and _finishes_. 



..... [program shutdown](../services/tethys.md#shutdown) .....



When the RTS performs final cleanup, it .....

When the RTS finishes, .....

On a hosted platform, the RTS terminates itself, in whichever way a program of the host 
operating system terminates itself normally. 
 
On a bare metal platform, the RTS shuts down the computer it is running on. .....



-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## Signals (#sig)

A hosted RTS can receive various _signals_ from its host platform, telling it that something 
has happened or that there is a change in its circumstances. 

AdaOS Native also generates these signals, as a way of controlling assemblies .....

The [environment task](#envtask) of the target assembly traps these signals .....

.....





The following standard signal types correspond to Posix signals:

| Type                   | Posix Signal
| ---------------------- | --------------------------------------------------------------------
| `Program_Abort_Signal` | `SIGABRT` 
| `Numeric_Error_Signal` | `SIGFPE`  
| `Attention_Signal`     | `SIGINT`  
| `Terminate_Signal`     | `SIGTERM` 

The following Posix signals can never actually be generated: 

| Posix Signal           | Description
| ---------------------- | --------------------------------------------------------------------
| `SIGILL`  | "illegal", invalid instruction
| `SIGSEGV` | "segmentation violation", invalid memory access

The reason these can never be generated is because the Realizor never generates code that could
possibly cause either of these situations. Whilst it is possible for native machine code
inserted by a privileged module could cause either of these conditions, it is simply assumed
that it never will, and it is the obligation of the software developer to ensure that native
code insertions are absolutely well-behaved. 










### `Program_Abort_Signal` (Posix `SIGABRT`)

"abort", abnormal termination


### `Numeric_Error_Signal` (Posix `SIGFPE`)

floating point exception



### `Attention_Signal` (Posix `SIGINT`)

"interrupt", interactive attention request sent to the program


### `Terminate_Signal` (Posix `SIGTERM`)

"terminate", termination request sent to the program

..... shut down (stop executing), immediately or very quickly, for ......


### ``

.....


### ``

.....


### ``

.....


### ``

.....


### ``

.....


### ``

.....


### ``

.....


### ``

.....


### ``

.....







-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## Scrying Cache






????? Objects?

The RTS maintains a [scrying cache](../objects/scrying.md#cache). 

.....





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 









-----------------------------------------------------------------------------------------------
## Creating a Run Time System







.....








If a RTS is to be built from Ada source text, then the following restrictions are 
likely to be required.

Standard restrictions:

    pragma Restrictions (Max_Tasks => 0); // no tasks
    pragma Restrictions ();
    pragma Restrictions ();
    pragma Restrictions ();
    pragma Restrictions ();
    pragma Restrictions ();
    pragma Restrictions ();
    pragma Restrictions ();
    pragma Restrictions ();
    pragma Restrictions ();
    pragma Restrictions ();
    pragma Restrictions ();
    pragma Restrictions ();

ECLAT-specific restrictions:

    pragma Restrictions ();
    pragma Restrictions ();
    pragma Restrictions ();
    pragma Restrictions ();
    pragma Restrictions ();
    pragma Restrictions ();
    pragma Restrictions ();
    pragma Restrictions ();
    pragma Restrictions ();
    pragma Restrictions ();
    pragma Restrictions ();



 
These restrictions can be most easily applied using configuration pragma `Profile` thus: 

    pragma Profile (Run_Time_System);

but they could also be applied by the appropriate set of pragma `Restrictions`. 

Since these pragmas ('Restrictions` and `Profile`) are configuration pragmas, the restrictions apply to the entire library, 
and it is therefore likely that the whole library will comprise the RTS and nothing else. 





-----------------------------------------------------------------------------------------------
## 


-----------------------------------------------------------------------------------------------
## Environment Task




????? AdaOS:


Every [program](programs.md) is, whether this is visible or not, a [service 
program](../services/servprog.md). 

An Ada program's entry point is the [main procedure](?????),
but the [Ada Reference Manual, Section 10.2][1] states that there is a conceptual _environment task_ 
that itself calls the main procedure. 

This environment task is supplied by the RTS.

.....


### Signal Catching


????? Does it (or some AdaOS sub-task)?


One of the things the environment task does is to catch [signals](#sig) sent to the instance 







### Awaiting Termination





????? Review


The exported subroutine `adaos.rts.await_signal` can be called in order to wait for 
a signal to arrive. 

This subroutine has one in-parameter, of type `system.time`, which is an absolute point of 
time at which the call (of the subroutine) is to return no later than. The subroutine has one 
out-parameter, of type `adaos.rts.signal`. 

When called, this subroutine will not return until either:

 * the environment task has received a signal; or 
 
 * the timeout time is reached (whichever occurs earlier). 

If the return is due to shut down, the out-parameter contains `true`, otherwise (for a 
timeout) it contains `false`. 
















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
## References

[1] (?????) "Ada Reference Manual: 10.2 ?????"








