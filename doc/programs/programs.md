-----------------------------------------------------------------------------------------------
# Programs {#programs}

A _program_ is essentially a discrete unit of functionality, whose functionality is activated
by _running_ the program. 

When a program is run, it performs its function and is then complete. 

.....

A [service program](../services/servprog.md) .....

.....



-----------------------------------------------------------------------------------------------
## What is a Program?

Typically, a usable computer system has many programs, but generally any one of those programs 
could be removed and the other programs would still be just as usable. Sometimes one program 
has dependencies on certain other programs for some or all of its functionality. 

Older programming languages, such as ForTRAN, COBOL, Pascal, Ada, and C for example, have a 
well-defined notion of a program. On very old computers, the computer could only run one 
program at a time, and that more or less defined what a program was. 

Traditionally, a single program is associated one-to-one with a single executable file. 

It might seem odd, therefore, that an executable file could contain multiple 'programs', but 
in fact, for a run time system on a [hosted platform](../pxcr/targets.md#plat), this is a 
feature that allows one program to initiate the execution of other programs without having to 
refer to the host operating system at all. 

For a [run time system](rts.md) on the AdaOS Native platform (where there is no host operating 
system), this is the (only) way a system can have multiple programs. 

From the AdaOS perspective, a program can be characterised by: 

 * A program is authored by one entity (person or organisation). All parts of the program are
   under the control (and, usually, ownership) of the same entity. 

 * Each part of a program trusts every other part of the same program. There is usually no need
   for any security, authentication, or permissions between them. 

 * Conversely, a program distrusts, by default, all other programs. There is a necessity for
   security between them. 

Sometimes there is a necessity for security between two different executions of the same
program, if there is confidential information passed into one that needs to be protected from
illicit access by the other. 

For this reason, every execution of a program gives rise to a construct (represented by a
system object), that protects it from illicit access by any other execution (of any program).
This construct is a [compartment](compart.md). 

In a typical AdaOS [effective system](../intro/intro.md#effsys), most programs will be [service
programs](../services/servprog.md). 



-----------------------------------------------------------------------------------------------
## Program Objects

.....

The limited interface type `System_Program`, declared in the package `AdaOS.Programs`,  
represents a program. It is derived from `System_Object`, declared in the package
`AdaOS.Objects`, so a program is a [system object](../objects/objects.md). 

......



?????

The package `AdaOS.Compartments` declares the following access type:

```ada
type Compartment_Access is access all Program_Compartment'Class;
```






There are some types declared which are derived from `System_Program` and represent 
more specialised kinds of program. 







These special kinds of program are described later on in this document. 









-----------------------------------------------------------------------------------------------
## Program Names {#names}

A program name is a [scoped entity name](../intro/names.md#scop) ......

However, a [program directory](#progdirs) does not enforce any restrictions on the 
syntax of program names, other than those imposed by all 
[directories](../objects/containers.md#dir). 

.....



-----------------------------------------------------------------------------------------------
##






-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## Running a Program {#run}

A program can be _run_ in order to create a new [compartment](compart.md) with, within it, a
set of [executional instances](instances.md) corresponding to the [partitions](partitions.md)
of the program. 

Running a program requires the creation of a set of [executional instances](instances.md), one
for each of the [partitions](partitions.md) the program. (Often a program will only have one
partition, so there would then only be one instance.) 

The compartment assists in enabling a multi-partition program to execute in a unified manner,
even if the different partitions' instances are running on different computers, in which case
the [co-compartment](compart.md#coco) mechanism is used. 

The type `System_Program` has a method function `New_Compartment`: 

```ada

function New_Compartment (
   Program:   not null access System_Program;
   Arguments: not null access constant Program_Argument_Array; 
   Super:     not null access Program_Compartment'Class := Task_Instance.Compartment)
return
   access Program_Compartment'Class is abstract;
```

The function `New_Compartment` returns (an access value referencing a system object that is a)
compartment suitable for the program to be executed in. The returned new compartment: 

 * has been created; 

 * is already engaged; 

 * has its super-compartment set to the given parameter `Super`; 

 * has the `Program` property set to the given `Program` parameter; 

 * has its `Arguments` property set to the given `Arguments` parameter; 

 * has all the executional instances corresponding to the partitions of the program; 

 * has its owner set to that of the super-compartment; 

 * is in the mode `Unbound`; 

 * has all its other properties initialised as described [here](compart.md#propinit). 

The parameter `Argument` should contain an array of the program arguments to be passed into the
compartment, in order. The argument values should be manipulated as little as possible (for
example, do not translate them all into upper or lower case) prior to being passed in. Null
cannot be passed in; if there are no program arguments, an empty array (of length zero) must be
passed. 

The parameter `Super` defaults to the compartment of whichever task is creating the new
compartment. This will be the most appropriate super-compartment in most cases.

There is also an overloading of the procedure `New_Compartment` with an `Owner` parameter, to
support [vicarious execution](#vic). 

Normally, for the execution of a program, all the executional instances share the same
compartment (or set of co-compartments), and this compartment has no other instances. 

The new executional instances are are all created and ready to start executing as soon as the
new compartment's mode changes to `Running`. 





For the execution of an [external program](#extprogs), a compartment is created whose
[properties and methods](../intro/intro.md#prop) mostly replicate the corresponding aspects of
the (external) execution environment of the external program. 







-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## Example of Running a Program {#ex1}

This subsection shows an example of running a program. 

Typically, a procedure should be created to encapsulate the details of running a particular
program for a particular purpose. 

Supposing we wanted to run a program named `ACME.Geospacial.Analyser`, .....

.....

```ada
with AdaOS.Compartments, AdaOS.Programs, AdaOS.Instances;
use  AdaOS.Compartments, AdaOS.Programs, AdaOS.Instances;

procedure ?????.Run_Analyser (File_Name: in Name_String)
is
   Program_Name: constant AdaOS.Name_String := "ACME.Geospacial.Analyser";
   Program: access System_Program'Class := Task_Instance.Compartment.Programs.Find (Program_Name);
   New_Cmpt: access Program_Compartment'Class := Task_Instance.Compartment;
   Waiter: access Compartment_Completion_Waiter'Class;

begin
   if Program = null then Failed := True; return; end if;
   New_Cmpt := Program.New_Compartment; -- pre-created, pre-engaged, implicit supercmpt, no args, default env
   if New_Cmpt = null then .....; end if;
   New_Cmpt.Arguments.Append (Wide_Wide_String(File_Name));
   New_Cmpt.Streams(AdaOS.?????.Output) := ?????.AdaOS.?????.Log_Stream;
   New_Cmpt.Environment_Variables.Set_Value ("ACME_GEO_ANAL_GEOMAT", "/?????/geomat-221024.dat");
   -- perhaps set other properties of the new compartment
   Waiter := New_Cmpt.New_Waiter;
   New_Cmpt.Run;
   New_Cmpt.Disengage;

   select
      Waiter.Await_Completion;

      if New_Cmpt.Exit_Code = Successful
      then
         -- program completed with no errors
      else
         -- there was an error
         raise Analyser_Problem;
      end if;

   or
      delay 5.0; -- timeout in 5 seconds
      -- program took too long to run
      raise Analyser_Problem;

   end select;

   New_Cmpt.Delete; -- all done, so compartment can be deleted

exception
   when others =>
      if New_Cmpt /= null then
         New_Cmpt.Disengage;
         New_Cmpt.Delete;
      end if; -- make sure it is deleted

      raise;
end;
```

The example procedure `Run_Analyser` 

.....









### Program Arguments

......






### Redirections

......




### Environment Variables

......





### Completion




.....








-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## Stock Programs {#stock}

Every [compartment](compart.md) maintains a ......






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
## Vicarious Program Execution {#vic}

_Vicarious program execution_ is a program is run with a compartment owned by a different
principal to the owner of the compartment of the program that is running it. In other words,
it's a way of running a program under a different owner. 

This doesn't happen very often. On hosted platforms, it won't ever happen (the owner of
everything remain the [top user](?????)). 

On the AdaOS Native platform, when a user logs in and their session (which is a program) needs
to be restored, that session program will be executed as a vicarious program. The session's
compartment's owner will be the user who just logged in. 

.....

There is an overloading of the procedure `New_Compartment` (of the type `System_Program`) with
an `Owner` parameter: 

```ada

function New_Compartment (
   Program:   not null access System_Program;
   Arguments: not null access constant Program_Argument_Array; 
   Owner:     not null access Security_Principal'Class;
   Super:     not null access Program_Compartment'Class := Task_Instance.Compartment)
return
   access Program_Compartment'Class is abstract;
```

This overloading supports vicarious execution. It does the same as the other overloading except
that the owner of the new compartment is explicitly specified rather than being assumed to be
that of the super-compartment. 



-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}











????? or Tethys helper?

[Realizor](..pxcr/realizor.md) generates an array containing ......








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
         
         <program name="" main-partition="" start-up="auto" phase="2">
            <partition name="" />
            <partition name="" />
            <partition name="" />
         </program>

         <external-program name="ftp" path="ftp" use-path-variable="true" />


      </programs>

      < name="" />

      <partitions>
         <partition name="">
            ...
         </partition>
      </partitions>
   </executable-image>
   ...

</tethys>
```

.....

The main partition is configured with the `main-partition` property. However, this is not often
needed, because it is conventional for the main partition to be named `main`, and if a program
has one partition named `main` it will be assumed to be the main partition unless another
partition has been explicitly configured as the main partition. 



.....

This will allow programs within the image to run the `ftp` external program. 



-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





