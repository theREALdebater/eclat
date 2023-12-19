-----------------------------------------------------------------------------------------------
# Start-Up and Shutdown

Before a [compartment](compart.md) can execute in a stable, normal state of execution, it must
go through a _compartment start-up_ process. Likewise, when the compartment transitions from a
normal state of execution to a terminal state, it must go through a _compartment shutdown_
process. 

Every compartment is associated with the running of a program (an AdaOS-level program). For all
compartments, compartment start-up and shutdown essentially comprises running the program's
partitions and terminating those partitions' instances. 

In the case of the top compartment, compartment start-up and shutdown is synonymous with
_system start-up_ and _system shutdown_. 

System start-up and shutdown entails start-up and shutdown of the
[effective system](../intro/intro.md#effsys). 

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
## System Start-Up {#start}

.....



When the system gets to the tertiary boot stage, the [tertiary boot
loader](../pxcr/booting.md#btldr3) loads the [initial segment](../pxcr/booting.md#iseg) into
memory, and starts executing it. 

After a little of its own initialisation, the tertiary boot loader starts running a set of
[programs](../adaos/programs.md) called the _system start-up programs_. 

One of these system start-up programs is the _main program_, the others are all _secondary
programs_. 

Which programs constitute the system start-up programs, and which of them is the main program,
is configured .....

.....

First, the secondary system start-up programs are all run, in parallel with each other. 

If any of the secondary system start-up programs fail to start executing successfully, [system
shutdown](#shut) is initiated. 

When, and only when, the secondary system start-up programs have all started running without
error, the [main program](#main) is run. 

When the main program completes (successfully or not), system shutdown is initiated. 

.....





On a hosted platform, the arguments and redirections passed to the main program are a
reflection of the arguments and redirections passed to the host program (via the host operating
system's own mechanisms). 

For all secondary programs, there are no arguments or redirections. 

On the AdaOS Native platform, all the system start-up programs have no arguments or
redirections. 






-----------------------------------------------------------------------------------------------
## System Shutdown {#shut}

.....





The _shutdown deadline_ of a compartment is an absolute moment in time at which all
[executional instances](instances.md) of the compartment---in practice, any remaining instances
which have not already shut themselves down---are immediately deleted (without any warning or
any possibility of delay or evasion). 

When a compartment is shut down with a certain deadline, it automatically causes any and all of
its sub-compartments to also be shut down, with the same deadline. 

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

 1. A [compartment shutdown event](?????) is broadcast W1 seconds in advance of the deadline; 
 
 2. The [shepherd](../services/servprog.md#shep) gets a system shutdown
    service control call W2 seconds in advance of the deadline (and can be expected to pass
    that on to all the non-dormant service programs it controls); 
 
 3. Executional instances receive a `SIGTERM` [signal](../events/signals.md) W3 seconds in
    advance of the deadline. 
 
The system shutdown event is broadcast immediately after system shutdown is initiated. 

This means that, by default, system shutdown takes a maximum of about 0.15 seconds to be 
completed. 

On the AdaOS platform, this will .....


### System Shutdown

For every program that is sent the ????? signal, 
the [service shepherd](../services/shepherds.md) 
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


-----------------------------------------------------------------------------------------------
## Main Program {#main}

A normal [executional image](../pxcr/images.md) must have a set of [partitions](partitions.md)
that comprise the _initial program_ of the image. 

When an [effective system](../intro/intro.md#effsys) starts execution (from system boot-up, on
the AdaOS Native platform, or when starting the outer external executable on a hosted
platform), and all of its [system initialisation](#start) has been completed (successfully),
the system then executes the main program of its image.

When the main program completes execution, the system initiates [system shutdown](#shut). 



?????The [run time system configuration module](#helper) contains .....



When the main program is [run](programs.md#run), the compartment it creates (by
`Create_Compartment`) is initialised by the [top user](../security/security.md#topuser) (by
calling `Initialize_Compartment`), and is termed the [top compartment](#topcmpt). 





..... the main program is [run](programs.md#run), the 
compartment created (by calling `Create_Compartment`) is the 
[top compartment](#topcmpt). 





The [service program](../services/servprog.md) [Tethys](../services/tethys.md) has a service
that is a program controller, and can be used as an main program. It is very typical for Tethys
to be used as the main program on all platforms. Tethys is a
[shepherd](../services/servprog.md#shep), and so it is well suited to starting up and managing
all the many programs that typically need to be run (or ready to run) at system start-up. 












????? 

The `.name.` export is a value of type `Wide_String` and contains the name of the initial 
partition of the image. It is termed the _explicit initial partition name_. This value may be a 
null string, which indicates that there is no explicit initial partition name. 

The `.vars.` export contains a list of wide string key-value pairs. These will be used to add 
to, or change, the environment variables passed into the initial partition when the RTS executes 
it. 

The Ada package `AdaOS.Partitions` contains the following declarations: 

```ada
   type Initial_Partition_Descriptor
   is
      record
         Id:               Partition_Id;
         Compartment_Id:   Compartment_Number;
      end record;

   for Initial_Partition_Descriptor
   use
      record
         Id                at 0 range 0 .. Partition_Id'Size - 1;
         Compartment_Id    at 0 range Partition_Id'Size - 1 .. Partition_Id'Size + Compartment_Number'Size - 1;
      end record;

   Initial_Partition: Initial_Partition_Descriptor
   with
      Import,
      External_Name => "system.config.initial_partition";
```

The C header file `adaos/partitions.h` contains the following declarations: 

```c

typedef struct {
   partition_id_t     id;
   compartment_id_t  compartment_id;
} initial_partition_t;
```









????? any real use?

The package `AdaOS.Execution` contains the following declaration:

```ada
function Main_Program return access System_Program'Class;
```

The function `Main_Program` returns (an access value that references) the main program of the
current image. 









.....



-----------------------------------------------------------------------------------------------
## Top Compartment {#topcmpt}

When the main program is [run](programs.md#run), the compartment it creates (by
`Create_Compartment`) is initialised by the [top user](../security/security.md#topuser) (by
calling `Initialize_Compartment`), and is termed the _top compartment_. 

The top compartment is initially the only existing compartment in the effective system. It's
existence spans one execution cycle of the image: .....

..... 

The top user populates the top compartment with 
objects and values based on its own built-in defaults and some configuration data passed to 
it in the 



[run time system configuration module](?????). 









-----------------------------------------------------------------------------------------------
## Configuring the Top Compartment

.....



On a [hosted platform](../pxcr/targets.md#plat), the ..........




On the AdaOS Native [platform](../pxcr/targets.md#plat), ......





????? This is configured for the top compartment.

top user's Initialize_Compartment 

 * sets the program arguments to the array .....

 * sets up the environment variables





### Program Arguments

In order to set the program arguments that will be made available to the initial partition when 
it is executed by the RTS, the following state update item can be set: 

    rts/images(X)/initial_partition/args
    
where `X` is the name of the executable image. 

.....

```ada
   ......
```

The default is an empty array. 





### Standard Streams

The property `Streams` ..... array .....

Some [images](../pxcr/images.md) will be targeted at embedded and small 'headless'
computers/machines/devices, which might have limited memory, limited (or no) secondary storage,
and very limited means to perform any kind of text I/O. 

Therefore, if the image offers no [predefined text streams](streams.md), the `Streams` array's
elements will all be null. 


### Environment Variables

The property `Environment_Variables` contains the [environment variables](envvars.md). 











In order to add to or change an environment variable that will be made available to the 
top compartment, the following state update XML could be used: 

```xml
<pxcr-saved-state xmlns="ns:adaos:pxcr:?????">
   <images>
      <image name="X">
         <main-program>
            <use-program name="P"/>
            ...
            <env-vars>
               <env-var name="V">the value of the env var here</env-var>
            </env-vars>
            ...
         </main-program>
      </image>
   </images>
</pxcr-saved-state>
```

where `X` is the name of the executable image and `V` is the name of the variable. 

.....

```ada
   ......
```



.....



### Environment Variables: Windows

On the Windows platform, the default set of environment variables is the set of environment 
variables inherited from the underlying operating system. 






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
| `TMPDIR`                 | `C:\Users\U\AppData\Roaming`
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
| `DEFAUTH`               | `0` (the top user)
| `DATETIME`               | (Dynamically set)
| `OS`                     | `Windows`
| `SYSTEMDRIVE`            | `/dev/c`
| `SYSTEMROOT`             | `/`
| `NUMBER_OF_PROCESSORS`   | (number of cores/processors)
| `PROCESSOR_ARCHITECTURE` | 
| `PROCESSOR_IDENTIFIER`   | 
| `PROCESSOR_LEVEL`        | 
| `PROCESSOR_REVISION`     | 

Many environment variables have dynamic functions. These are set up as follows. 




### Environment Variables: Linux

On the Linux platform, the default set of environment variables is the set of environment 
variables inherited from the underlying operating system. 





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
| `TMPDIR`                 | `/H/.config/P`
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
| `DEFAUTH`               | `0` (the top user)
| `DATETIME`               | (Dynamically set)
| `OS`                     | `Linux`
| `SYSTEMDRIVE`            | `/dev/c`
| `SYSTEMROOT`             | `/`




### Environment Variables: AdaOS Native

On the AdaOS Native platform, the default set of environment variables is .....




| Name                     | Default Getter Function          | Default Setter Procedure     |
| ------------------------ | -------------------------------- | ---------------------------- |
| `APPDATA`                |  |
| `LOCALAPPDATA`           |  |
| `PROGRAMDATA`            |  |
| `PWD`                    | `Current_Directory_Path`         | `Set_Current_Directory_Path` |
| `CWD`                    | `Current_Directory_Path`         | `Set_Current_Directory_Path` |
| `CD`                     | `Current_Directory_Path`         | `Set_Current_Directory_Path` |
| `TMPDIR`                 | `/tmp`
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
| `DEFAUTH`               | `0` (the top user)
| `DATETIME`               | (Dynamically set)
| `OS`                     | `AdaOS`
| `SYSTEMDRIVE`            | `/dev/c`
| `SYSTEMROOT`             | `/`




### Ambit

The [ambit](../security/security.md#amb) is set to simply the [top authority](../security/security.md#auth) (authority 0).
However, since the top authority is superior to all other authorities, this ambit could not
(meaningfully) include any other authorities anyway. 

The [ambit](../security/security.md#ambit) is set to simply the [top authority](../security/security.md#auth) (authority 0).
However, since the top authority is superior to all other authorities, this ambit could not
(meaningfully) include any other authorities anyway. 

This ambit means that the top compartment has totally unrestricted access to everything in the
[effective system](../intro/intro.md#effsys).

For hosted platforms, the ambit .....

For the AdaOS Native platform, .....


### Accounts

?????


### Programs

The available programs are the image's [configured programs](programs.md#config). 




```xml
<pxcr-saved-state xmlns="ns:adaos:pxcr:?????">
   <programs>
      <program name="AdaOS.Tethys">
         <partitions>
            <use-partition name="A"/>



</pxcr-saved-state>
```

where `X` is the name of the executable image and `V` is the name of the variable. 






### Services

The available services comprises the base services and the configured [controlled
services](../services/servprog.md). 



```xml
<pxcr-saved-state xmlns="ns:adaos:pxcr:?????">
   <services>
      <service name="S" start-mode="auto"/>


</pxcr-saved-state>
```





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

      <partition name=""></partition>

      <program name="">
      
      
      
      </program>

      <service name="">
        <service-program name=""/>
      </service>

      .....

   </image>
</pxcr>
```











On the AdaOS Native platform, the base compartment depends on the 
[authority](../security/security.md#auth) of the compartment (called the 
_parent_) that is starting the execution of another program (whose compartment will be 
the _child_):

 * for the top authority, the base compartment is the top compartment; 
 
 * any other authority has a base compartment defined for it. 






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

This means that `DEFAUTH` is not set, which is appropriate for hosted platforms. 


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

The environment variable `DEFAUTH` should be [configured appropriately](?????). 

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




