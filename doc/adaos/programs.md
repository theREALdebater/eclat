-----------------------------------------------------------------------------------------------
# Programs {#programs}

A _program_ is essentially a discrete unit of functionality, .....

Typically, a usable computer system has many programs, but generally any one of those programs 
could be removed and the other programs would still be just as usable. Sometimes one program 
has dependencies on certain other programs for some or all of its functionality. 

Older programming languages, such as ForTRAN, COBOL, Pascal, Ada, and C for example, have a 
well-defined notion of a program. On very old computers, the computer could only run one 
program at a time, and that more or less defined what a program was. 

.....

Traditionally, a single program is associated one-to-one with a single executable file. 

It might seem odd, therefore, that an executable file could contain multiple 'programs', but 
in fact, for a run time system on a [hosted platform](../pxcr/targets.md#plat), this is a 
feature that allows one program to initiate the execution of other programs without having to 
refer to the host operating system at all. 

For a [run time system](rts.md) on the AdaOS Native platform (where there is no host operating 
system), this is the (only) way a system can have multiple programs. 



-----------------------------------------------------------------------------------------------
## Program Objects

.....

The limited tagged private type `System_Program`, declared in the package `AdaOS.Programs`,  
represents a program. It is derived from `System_Object`, declared in the package
`AdaOS.Objects`, so a program is a [system object](../objects/objects.md). 

......



?????

The package `AdaOS.Security` declares the following access type:

```ada
type Compartment_Access is access all Program_Compartment'Class;
```






There are some types declared which are derived from `System_Program` and represent 
more specialised kinds of program. 







These special kinds of program are described later on in this document. 









-----------------------------------------------------------------------------------------------
## Program Names {#names}

????A program name should be a [scoped entity name](../intro/names.md#scop) ......

However, a [program directory](#progdirs) does not enforce any restrictions on the 
syntax of program names, other than those imposed by all 
[directories](../objects/containers.md#dir). 

.....



-----------------------------------------------------------------------------------------------
##






-----------------------------------------------------------------------------------------------
## Compartments {#cmpt}

When a program is started, that causes the creation of either one or a set of [executional 
instances](instances.md) of the [assemblies](assemblies.md) that correspond to all of 
the assemblies of the program. In many other operating systems the term 'process' is used to 
mean an executional instance of a program (other operating systems have no notion of one 
program giving rise to a set of instances). 

When a program is started, a new [compartment](compart.md) is created, which will be the
compartment of all the executional instances created, and therefore represents an execution of
a program. 



The compartment is used to 
manage the executional instance or instances that represent the assembly or assemblies 
of the program as it runs. 







It assists in making a multi-assembly program execute in a unified 
manner (even if the different assemblies' instances are running on different computer systems,
in which case the [co-compartment](?????) mechanism is used). 

......





-----------------------------------------------------------------------------------------------
## Program Controllers {#progcont}

A _program controller_ provides convenient facilities for controlling programs. 

A program controller is itself a [service](../services/services.md), but a very special one. It
is also an [object directory](../objects/containers.md#dir), containing a set of
[programs](programs.md). 

If a [compartment](compart.md) does not need to itself initiate the execution of any programs,
it does not need a program controller. 

The function `Programs` of a compartment returns (an access value referencing) the
compartment's program controller. 

A program controller provides the following functionality:

 * Maintains a [directory](#dir) containing programs (as system objects);    
   
 * Enables [standby](#standby) programs to be dynamically run; 

 * Supports one or more programs being configured to [start immediately](#auto); 
 
 * Supports programs being configured to start immediately and kept running until [compartment
   shutdown](startshut.md); 

 * Alternatively, supports [service programs](../services/servprog.md) being configured to
   start automatically [on demand](#ondem); 

 * Provides for programs to be configured to be automatically restarted if they fail. Various 
   options provide sensible control over this, so that, for example, a continuously failing 
   program does not tie up large amounts of system resource. 



-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## Program Directory {#dir}

A program controller is an [object directory](../objects/containers.md#dir), containing a set
of [programs](programs.md). 

.....





-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## Running a Program {#run}



.....

We start this subsection with an example.


### Example

Typically, a procedure should be created to encapsulate the details of running a particular
program for a particular purpose. 

Supposing we wanted to run a program named `ACME.Geospacial.Analyser`, .....

.....

```ada
with AdaOS.Compartments, AdaOS.Programs, AdaOS.Instances;
use  AdaOS.Compartments, AdaOS.Programs, AdaOS.Instances;

procedure ?????.Run_Analyser (File_Name: in Name_String; Failed: out Boolean)
is
   Program_Name: constant AdaOS.Name_String := "ACME.Geospacial.Analyser";
   Program: access System_Program'Class := Task_Instance.Compartment.Programs.Find (Program_Name);
   New_Cmpt: access Program_Compartment'Class := Task_Instance.Compartment;
begin
   if Program = null then Failed := True; return; end if;
   New_Cmpt := Program.Create_Compartment;
   New_Cmpt.Arguments.Append (Wide_Wide_String (File_Name));
   New_Cmpt.Streams (AdaOS.?????.Output) := ?????.AdaOS.?????.Log_Stream;
   New_Cmpt.Environment_Variables.Set_Value ("ACME_GEO_ANAL_GEOMAT", "/?????/geomat-221024.dat");
   
   .....

   New_Cmpt.Start;
   New_Cmpt.Await_Completion;

   if New_Cmpt.Status = Successful
   then
      -- program completed with no errors
   else
      -- there was an error
   end if;
end;
```

The example procedure `Run_Analyser` 



The call to `Create_Compartment` creates a new [compartment](compart.md). An access value
referencing the new compartment is returned in the out-parameter `Compartment`. 

The call to `Start` of a compartment creates a set of new [executional instances](instances.md)
in that compartment. 


.....


### Program Arguments

......






### Redirections

......




### Environment Variables

......





### Completion



 are in [Completed](instances.md#mode) mode



When a compartment is stopped, all the instances, the
compartment's `Await_Completion` procedure will return. 

[Status Code and Compartment Completion](compart.md#stat)

.....








-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## Program Start-up Strategies {#start-up}



.......


is one 
of the following words (or phrases): 

 * `standby`
 
 * `auto` or `main`
 
 * `service` 
 
 * `service-auto` 
    
When all programs of strategy `auto`, `main`, or `standby` have completed (the instance 
controller has entered the Terminated mode), then the [service 
controller](../services/tethys.md) will initiate [system shutdown](#shutdown). 


### Start-up Phase

The _start-up phase_, if specified, must be an integer made up solely of decimal digits `0` to
`9`, whose value is between 1 and 99 (inclusive). It is only relevant to strategies `auto`,
`main`, and `service-auto`. If omitted the default is 99. A phase is considered _earlier_ than
phases which have a numerically higher value. 


### Standby {#standby}

If the start-up strategy of a program is set to `standby`, the program is not run unless 
requested ......

.......

See [Running a Program](#run) for how a program is started .....

........

This is the default start-up strategy of a program if a start-up strategy is not specified.


### Auto (non-service) {#auto}

If the start-up strategy of a program is set to `auto` or `main` (not `service-auto`), the 
program controller will run the program (once), as soon as it has completed its own 
initialisation, and after all programs of an earlier phase have started
 
 

?????and are awaiting a [signal](?????). 



........

On a hosted platform, if the start-up strategy of a program is set to `main`, the program
arguments and redirections passed to the program are a reflection of the program arguments and
redirections passed to the host program (via the host operating system's own mechanisms). 

If the start-up type of a program is set to `auto` and the phase is not ?????, there are no program arguments or 
redirections. 

On the AdaOS Native platform, `main` is the same as `auto`. 

In every case, the top compartment is copied to create the compartment in which the program's 
assemblies execute. 




This strategy is used for the main program of an executable image. However, it is also useful 
for programs that need to be run in conjunction with the main program but cannot be expected 
to be run by the main program. 

For example, a program that subscribes to one or more [event channels](../events/events.md). In this case, it may be 



..........


### Service on Demand {#ondem}

If the start-up strategy of a program is set to `service` (without the `auto` word after it), 
the program is run whenever one of its associated services is required and the program is not 
already running. This strategy is termed _service on demand_. 

The program must be a [service program](../services/servprog.md) and it must have had its 
service name or names set. 

.........

?????The program controller, during its initialisation, registers a 
[placeholder](../objects/objects.md#placeholders) 
with the [service directory](#svcdir) of the [role](../security/roles.md) of the program. 

The placeholder has the same name as the service name set for the program. It  .......

........

Whenever the service is [found](../objects/containers.md#find), ......

.........

There are no program arguments or redirections. 


### Auto Service

If the start-up strategy of a program is set to `service-auto`, the program controller, as soon 
as it has completed its own initialisation 
and after all programs of an earlier phase have started
 
 

?????and are awaiting a [signal](?????). 







, will run the program 
(once). This strategy is termed _auto service_. 

If the program completes, the program controller immediately attempts to start it again, 
unless [compartment shutdown](startshut.md) has been initiated (and even then the program will still be started if 
another program requests one of its services). 

The program must be a [service program](../services/servprog.md) and it must have had its 
service name or names set. 

........

There are no program arguments or redirections. 

........





-----------------------------------------------------------------------------------------------
## Program Controller Configuration Modules {#pccm}




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
## External Programs {#extprogs}

On a [hosted platform](../pxcr/targets.md#plat), an _external program_ is an executable entity 
(usually a file) as defined by, and managed by, the underlying host operating system. The AdaOS 
Native platform has, naturally, no external programs. 



?????On a [hosted platform](../pxcr/targets.md#plat) when Tethys is requested to execute a program 
(by giving the program's name), Tethys first tries to find the program in its registry; if it 
cannot find it, then Tethys makes use of the host operating system's facilities for initiating 
the execution of executable files. 

.....

On the AdaOS Native platform, only registered programs are available; however, because on this 
platform dynamic segment images can be added (to the boot image), programs can be dynamically 
added to the registry and removed from it.







?????

I�ll also define a framework for the execution of external programs and the control of 
executing instances (processes). 

This framework will identify every accessible external program by an abstract URI -- which 
could be a URN, a URL, or a simile of a file path, or any other arbitrary string -- and the 
realizor�s configuration can associate these URIs with actual executable files (or other 
executable program entities). A special configuration provider can account for the paths in the 
`PATH` environment variable, but all the programs in these paths will only be seen (and thus 
executable) statically at realisation time. 

The idea is that the framework will allow (but not require) all the available programs to be 
included by the realizor into one monolithic executable (potentially a gigantic one, 
presumably). This is to permit some optimisations, and to permit some of the security checks to 
be done statically. 

The framework will use the security model (see below).

The framework will be modelled as if it were a service, but it could be implemented by calling 
into the underlying platform rather than as a service as such. 







The interface type `External_Program` represents an external program. 

The most important extra property of `External_Program` is the full path of the executable 
file that the external program object represents.  

This property is retrieved with the function:

```ada
function External_Program_Name (Context: in Service_Context) return Wide_String is abstract;
```

The property is set with the procedure:

```ada
procedure Set_External_Program_Name (Context: in out Service_Context; 
                                     Name:    in Wide_String) is abstract;
```

.....





-----------------------------------------------------------------------------------------------
##



Tethys

.....

    pxcr image X program P host "Y"





-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
## Vicarious Programs {#vic}

A _vicarious program_ is a program that is started with a compartment owned by a different
principal to the owner of the compartment of the program that is starting it. In other words,
it's a way of starting a program under a different owner. 

This doesn't happen very often. On hosted platforms, it won't ever happen (the owner of
everything remain the [top user](?????)). 

On the AdaOS Native platform, when a user logs in and their session (which is a program) needs
to be restored, that session program will be executed as a vicarious program. The session's
compartment's owner will be the user who just logged in. 

.....








-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}











????? or Tethys helper?

[Realizor](..pxcr/realizor.md) generates an array containing ......








-----------------------------------------------------------------------------------------------
## 

?????

```ada
procedure Create_Compartment
(
   Program:       not null access System_Program
   Compartment:   out Compartment_Access;
) 
is abstract;
```



-----------------------------------------------------------------------------------------------
## Configuring Programs

........

..... [Tethys](../services/tethys.md) .....
    
A program can be added to the [base program repertoire](?????) of an executable image with the 
following .....

```xml
<tethys xmlns="urn:adaos:ns:tethys.stup.1.0" xmlns:db="">

   <executable-image name="" uuid="">
      <programs>
         
         <program name="" main-assembly="" start-up="auto" phase="2">
            <assembly name="" />
            <assembly name="" />
            <assembly name="" />
         </program>

         <external-program name="ftp" path="ftp" use-path-variable="true" />


      </programs>

      < name="" />

      <assemblies>
         <assembly name="">
            ...
         </assembly>
      </assemblies>
   </executable-image>
   ...

</tethys>
```

.....

The main assembly is configured with the `main-assembly` attribute. However, this is not often
needed, because it is conventional for the main assembly to be named `main`, and if a program
has one assembly named `main` it will be assumed to be the main assembly unless another
assembly has been explicitly configured as the main assembly. 



.....

This will allow programs within the image to run the `ftp` external program. 



-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





