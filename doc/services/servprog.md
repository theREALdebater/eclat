-----------------------------------------------------------------------------------------------
# Controlled Services and Service Programs

A _controlled service_ is a [service](../rts/services.md) that is implemented by a dedicated 
[program](../rts/programs.md) that is called a _service program_. 

A service program is dedicated to implementing one or more controlled services when it is 
running. Service programs do not normally do anything other than registering and implementing 
the services. 

It is unusual, in practice, for a service program to run more than one (controlled) service. 



-----------------------------------------------------------------------------------------------
## ?????

A controlled service is represented by the interface type `Controlled_Service`, which is 
derived from `AdaOS.Services.System_Service` and declared in the package 
`AdaOS.Services.Controlled`. 

The interface type `Service_Program` represents a service program. 

The most important extra property of 





?????`Service_Context` 





is the set of names of the services it implements. 

This property is retrieved with the functions:

```ada
function Number_Of_Services (Context: in Service_Context) return Natural is abstract;

function Service_Name (Context: in Service_Context; 
                       Number:  in Positive := 1) return Wide_String is abstract;
```

The property is set with the procedures:

```ada
procedure Set_Number_Of_Services (Context: in out Service_Context; 
                                     Last:    in     Natural) is abstract;

procedure Set_Service_Name (Context: in out Service_Context; 
                               Name:    in     Wide_String;
                               Number:  in     Positive := 1) is abstract;
```

.....







-----------------------------------------------------------------------------------------------
## Dormancy {#dorm}

A service program normally has no more than one compartment (representing one execution of the
program). This compartment, when it exists, is called the _implementor_ of the service program
(and therefore of its controlled services). 

A service program is _dormant_ when no implementor (a compartment corresponding to an execution
of the program) exists. When it is dormant, the service program's [mode](#mode) is either
Dormant or Retired 

Note that there may be a little bit of time in between the service program being in a
particular mode and its implementor actually being created or destroyed. 











.....




-----------------------------------------------------------------------------------------------
## .....

.....




-----------------------------------------------------------------------------------------------
## .....

.....




-----------------------------------------------------------------------------------------------
## Service Modes {#mode}

Every service program is in one of five _service modes_ at any one time.

The package `AdaOS.Services` contains a declaration of the enumerated type `Service_Mode`  
whose five values are as follows:

| Mode         | Description                                                  |
| ------------ | ------------------------------------------------------------ |
| `Dormant`    | No implementor (compartment) exists                          |
| `Stopped`    | Can run, but is not currently running                        |
| `Starting`   | Getting towards `Running`, but not yet able to accept calls  |
| `Running`    | Running normally, able to accept and process calls           |
| `Retired`    | Failed too many times, cannot run                            |

The mode is said to be _good_ if it is `Dormant`, `Stopped`, `Starting`, or `Running`, and
_bad_ if it is `Retired`. 

These modes are very similar to the [execution modes](../adaos/compart.md#mode) of a
compartment (an execution of a program), but they are different, in that they are describing a
service program, rather than a compartment. They don't replace the execution modes of a
compartment. 


### Dormant

A service program is in the _dormant_ mode when no implementor (a compartment corresponding to
an execution of the program) exists.

When a service program is [dormant](#dorm), .....

.....






When a service program is initially in the dormant mode. 

.....


### Stopped

In the _stopped_ mode, the program's controlled services are (nominally) not doing anything and
should not be called upon to do anything. 

Generally, any attempt to call upon a controlled service to do something when its service program is in this mode should 
wait, for a certain period of time, for the service to get into the running mode. If this 
period of time expires without the service getting into the running mode, the attempt should 
fail (if it was a call to a subprogram, that subprogram should propagate the exception 

?????`Mode_Error` of `AdaOS.Services`). 

The service program may (temporarily) be in the process of stopping when it is in this mode.
The program might, therefore, be doing things while in this mode (for example, finishing the
execution of things one of its services was called upon to do before it went into stopped
mode), but it will not start doing anything while in stopped mode. 


### Starting

In the _starting_ mode, a service program is (temporarily) getting itself into the running
mode. Its services are still not yet ready to be called upon to do things. 

Generally, any attempt to call upon a controlled service to do something when it is in this
mode should wait, for a certain period of time, for the service to get into the running mode.
If this period of time expires without the service getting into the running mode, the attempt
should fail (if it was a call to a subprogram, that subprogram should propagate the exception 

?????`Mode_Error` of `AdaOS.Services`). 


### Running

In _running_ mode, a service program may be doing things, and its controlled services are ready
to be called upon to do things. 


### Retired

A service program is typically put into the _retired_ mode when it has failed too many times.
In this mode, the program is (nominally) not doing anything and its controlled services should
not be called upon to do anything. In this mode, the program should not be allowed to be
started. 

Generally, if a service program is put into retired mode, it stays in that mode. A program
controller may provide a means to get programs out of retired mode, but that depends on the
service controller. 

Any attempt to call upon a controlled service to do something when its service program is in
this mode should immediately fail (if it was a call to a subprogram, that subprogram should
propagate the exception 

?????`Mode_Error` of `AdaOS.Services`).

When a service program is in retired mode, it is normally put into the same state as if it were
dormant  .....



-----------------------------------------------------------------------------------------------
## Service Control

Every service program must implement the additional primitive operations, the _service program 
control operations_, of the interface type `Service_Program`, ..... 

.....

Normally, the service program control operations are only called upon (directly) by the [service 
controller](tethys.md). All software (other than the program controller) should use the service 
control operations of the program controller to start and stop services. 

A service program is restarted by calling `Stop` and then `Start`. This sequence should cause the 
program to go through the complete cycle of disposing of its resources and then initialising 
them again, as if it were being started for the first time. No service program should ever cut this 
cycle short in any way. 


### Start

The `Start` procedure transitions the service program from the dormant or stopped mode to the
starting mode. 

......

If the program is already in the starting or running mode, calling `Start` (harmlessly) does 
nothing. 

If the program is in the retired mode, calling `Start` fails, with the exception `Mode_Error` 
(of `AdaOS.Services`) being propagated. 





When in the dormant mode, the service program should create a new
[compartment](../adaos/programs.md#exec), which becomes the [implementor](#dorm) of the service
program. Once the compartment has been started, it should go into Starting mode, in which it
will perform all of its normal initialisation. When the initialisation is complete, the program
should transition into the Running mode. 






When in stopped mode, the program should go into starting mode. To do this, the program will
perform all of its normal initialisation. When the initialisation is complete, the program
should transition into the running mode. 

Starting a service will not normally happen frequently, and is not speed critical. However, the
start-up process should, generally, aim to get the program into a state where it is able to
respond quickly and perform its duties efficiently. The program will be in running mode when it
is in this state. 

If the program is in the middle of stopping when `Start` is called, the program should complete
its stopping procedures normally and then go into starting mode as normal. It is unlikely to be
advisable to try to optimise this situation, but if there is any such optimisation there must
be no danger of the program being destabilised by it. 


### Stop

The `Stop` procedure immediately transitions the service program from starting or running mode
to the stopped mode.

.....

If the program is already in stopped mode, calling `Stop` does nothing. 

If the program is in retired mode, calling `Stop` does nothing (in particular, it does not
change the mode of the program).

If the program is in starting mode when `Stop` is called, it should immediately go into stopped
mode and never go into running mode in between. 

When going into stopped mode from either starting or running mode, the program should reduce
its resource usage to a minimum. Reducing (re-)initialisation time should not normally be
considered. Stopping a service program will not normally happen frequently, and is not speed
critical. 

......


### Retire

The `Retire` procedure immediately transitions the service program from any other mode into
retired mode. 

.....

If the program is already in retired mode, calling `Retire` does nothing. 

If the program is in starting mode when `Retire` is called, it should immediately go into
retired mode and never go into running mode. 

When going into retired mode from either starting or running mode, the program should reduce
its resource usage to a bare minimum. 

.....


### Sleep

The `Sleep` procedure transitions the service program from any other mode except retired into
dormant mode. 



-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## Service Program Shutdown (#shutdown)

.....

When a service program is in stopped mode, the program should 
respond to the .....

.....






-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## Program Startup mode 






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
## Shims

The services of the host operating system, if there is one, can be made available to ECLAT 
programs by creating _shims_.

A shim is a service that acts as intermediary for one or more services, daemons, or other 
functions of the host operating system. 

.....



-----------------------------------------------------------------------------------------------
## Stock Services {#stock}

ECLAT is supplied with a number of _stock services_ that play an essential role in enabling 
programs to execute. In general, programs will assume that these services are available. 


### Tethys

[Tethys](tethys.md) is a program controller .....

The name of this service is: 

    adaos.services.tethys
    
Tethys does not have any associated service program. It is all included in a single [program 
assembly](../rts/assemblies.md) module, named: 

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

The [Event Broker](events.md) service enables assemblies to be sent events raised by 
other assemblies. 

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
that can coordinate the activities of multiple assemblies under the control of a 
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
## Example: Implementing a Service Program

For this example, we are going to implement a weather prediction service. The client provides 
a time and a place, and the service predicts what the weather will be. 



......



.....

```ada
with Ada.Calendar;
use  Ada.Calendar;

package Weather_Prediction
is
   type Weather_Predictor is limited AdaOS.Services.Controlled_Service with private;
   
   type Predictor_Access is access all Weather_Predictor'Class;
   
   type Weather_Location 
   with
      Export, External_Name => "acme.ssc.weather.location"
   is 
      (Nuuk_GL, London_UK, Austin_TX);
   
   type Weather_Forecast is (Hot, Cold, Cloudy);
   
   procedure Make_Forecast (Predictor: in out Weather_Predictor;
                            Location:  in     Weather_Location;
                            Date:      in     Time;
                            Forecast:  out    Weather_Forecast) is abstract;
   
   -- other public operations of Weather_Predictor

private
   type Weather_Predictor is limited AdaOS.Services.Controlled_Service with null record;

end;
```

..... `Weather_Predictor` is derived from the type `Controlled_Service`  ......


..... `acme.ssc.weather.location`, which stands for: a company named Acme, Software Standards
Council, Weather Prediction Service. The values of this enumeration type will also be exported,
as `acme.ssc.weather.location.nuuk_gl`, `acme.ssc.weather.location.london_uk`, and so forth. 

We must also create a [module class definition](../pxcr/mcd.md) file for the module to be generated, .....


The good news is that, because our module class derives from ......, we don't have to tediously
repeat all of the ......, since they will all be inherited.

.....

```xml







```


.....


```ada
with Ada.Calendar;
use  Ada.Calendar;

package UCSD_Weather
is
   Service_Name: constant String := "acme.fgs.weather";
   
end;
```







.....

```ada
package body UCSD_Weather
is
   task type Predictor_Service
   is
      new Weather_Prediction.Weather_Predictor
   with
      overriding 
      entry Make_Forecast (Location:  in  Weather_Location;
                           Date:      in  Time;
                           Forecast:  out Weather_Forecast);

      -- other operations of Weather_Predictor

      overriding entry Engage_Operation;
      overriding entry Disengage_Operation;
      overriding entry Shut_Down_Service_2;
      overriding entry Shut_Down_Service_3;
   end;

   task body Predictor_Service is separate;

   The_Service: aliased UCSD_Predictor; -- singleton object
   
   procedure Engage_Predictor (Predictor: out Predictor_Access)
   is
   begin
      AdaOS.Services.Engage_Service (Service_Name, Predictor);
   end;

begin
   AdaOS.Services.Register_Service (Service_Name, The_Service'Access);

end UCSD_Weather;
```

.....




Now we will define the [endorsements](../security/endorse.md) intermediary package 
for the service. 

The following [endorsement information document](#eid) .....


```xml
<?xml version="1.0"?>
<security xmlns="?????">
   <import name="ada.calendar"/>
   <import name="acme.ssc.weather"/>
   <service-control name="acme.fgs.weather.control">
      <operation name="make-forecast">
         <endorse>
            <input>
               <attribute name="location" type="acme.ssc.weather.location"/>
               <attribute name="date" type="ada.calendar.time"/>
            </input>
         </endorse>
      </operation>
   </service>
</security>
```


config



This EID will be built into a module ......

.....

Now we write a service control package that imports the core type created by the EID, and its 
control operations. 

```ada
with AdaOS.Services.Helpers;
with Weather_Prediction;

use AdaOS.Services.Helpers;
use Weather_Prediction;

package UCSD_Weather_Service_Control
is
   type Service_Control_Core is new Controller_Service_Core with private;
   
   procedure Begin_Make_Forecast (Core:      in out Service_Control_Core;
                                  Location:  in     Weather_Location;
                                  Date:      in     Time);

private
   type Service_Control_Core 
   with
      Import, External_Name => "acme.fgs.weather.control.core";
   
end;
```

In this case, the only control operation is to endorse the 'make forecast' operation. 


.....







```ada
package body UCSD_Weather_Service_Control
is
   procedure Begin_Make_Forecast (
      Core:     in out Service_Control_Core with External_Name => "core";
      Location: in     Weather_Location     with External_Name => "location";
      Date:     in     Time                 with External_Name => "date")
   with 
      Import, External_Name => "acme.fgs.weather.control.make-forecast";

end;
```

.....

Now we can write the predictor service implementation.

```ada
with UCSD_Weather_Service_Control;

use UCSD_Weather_Service_Control;

separate (UCSD_Weather)

task body Predictor_Service
is
   Core: Service_Control_Core;

begin
   -- initialisation actions

   loop
      select
         accept Make_Forecast (Location:  in  Weather_Location;
                               Date:      in  Time;
                               Forecast:  out Weather_Forecast)
         do
            Begin_Make_Forecast (Core, Location, Date);
            
            if not Core.Operation_Permitted then
               raise Permission_Error;
            end if;

            case Location -- (regardless of Date :-)
            is
               when Nuuk_GL   => Forecast := Cold;
               when London_UK => Forecast := Cloudy;
               when Austin_TX => Forecast := Hot;
            end case;
         end do;

         if Core.Auditing_Required then
            -- audit the operation
         end if;

         Core.End_Operation;

      or
         -- accept other operations

      or
         accept Engage;
         Core.Engage_Service;

      or
         accept Disengage;
         Core.Disengage_Service;

      or
         when not Core.Is_Engaged =>
         accept Shutdown_2;
         Core.Shut_Down_Service;

      or
         accept Shutdown_3;
         Core.Shut_Down_Service;

      or
         when not Core.Is_Engaged =>
         delay Core.Service_Timeout;
         exit;
      end select;
   end loop;

   -- shutdown actions

end
   Predictor_Service;
```

This is a task .......






Finally, we can write the implementation of the service program.


```ada
with UCSD_Weather;

procedure Start
is
begin
   UCSD_Weather.

end;


```



```ada
with AdaOS.Services.Helpers;
with Start, Stop;

procedure Main
   is new AdaOS.Services.Helpers.Service_Program (Start, Stop, .....);










?????

procedure Main
is
begin
   AdaOS.Services.Await_Program_Termination;
end;
```





-----------------------------------------------------------------------------------------------
## Service Programs: Main Subprogram

A service program should implement the service in a library-level non-generic package (it might 
be an instantiation of a generic package). 

The reason it should be done like this is for portability. To port your overall application
cluster, group, or suite to an Ada implementation other than ECLAT, the facility to implement
services as service programs (in particular, to link together multiple modules) may not exist,
or it may not be a practical option. In this case, it will be necessary to bring the required
services into the same library, to be compiled all together to make an executable program with
services that are not implemented by separate programs. Having the service implemented by a
library-level package will help to facilitate this kind of manipulation. 

The main subprogram should normally look like this: 

```ada
with AdaOS.Execution;
procedure Main
is
begin
   -- Put here anything that needs to be done before starting up the service
   AdaOS.Services.Await_Program_Termination;
   -- Put here anything that needs to be done after the service has shut down
end;
```

The procedure `Await_Program_Termination`, declared in the package `AdaOS.Services`, 



......



-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
## Configuring Service Programs

........

A service can be added to those associated with a service program in the [base program 
repertoire](#brp) of an executable image with the following 


????? XML STUF?


Realizor AdaShell command: 

```sh
chimg X
chprog Z 
progserv S
```
   
where `X` is the executable image, `Z` is the name of the program, and `S` is the name of the 
service. 

.....







.....



-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
## Example

.....

Supposing we have a service `Forestry.Tree_Counter`, implemented by a program named 
`forestry.tree-counter`, and we wish to register it as a service program with Tethys under the 
same name. 

The supposed purpose of this service is to count up trees. (Perhaps a drone flies over a forest 
and counts trees as it sees them.)

It will have three essential operations: reset the counter (to zero); add a certain number (of 
trees) to the current count; retrieve the current count. 

In addition, we will need to implement the `Startup` and `Shutdown` operations. 

.....


### Service Definition Library

First, we create an abstract type to represent any and all tree-counting services, and a 
library, the _service definition library_ of the tree counting service, to hold just this type. 

The package specification might look like this:

```ada
package Acme.Forestry
with
   Preelaborable_Initialization
is
   type Tree_Counter is interface and AdaOS.Services.Controlled.Controlled_Service;

   procedure Reset (Counter: access Tree_Counter) is abstract;
   
   procedure Add (Counter: access Tree_Counter;
                  Number:  in Natural := 1) is abstract;
   
   procedure Get_Count (Counter: access Tree_Counter;
                        Count: out return Natural) is abstract;
   
   procedure Start_Up (Counter: access Tree_Counter) is abstract;
   procedure Shutdown (Counter: access Tree_Counter) is abstract;
end;
```


### Compiling the SDL

We will say the common library is named:

    acme.forestry

.......

We will assume that the current directory is:

    C:\eclat\src\acme.forestry
    
which is where the Ada source text files are. 

We will assume our environment variables have the following values:

```
PATH=?????
ECLAT_SVC=?????
PXCR_SVC=?????
ECLAT_CWL=acme.footle.bartle

```

The following [Allegra](?????) 
or `bash` shell commands could be used to configure and
compile the service definition library: 

```
export ECLAT_CWL=acme.forestry # change to the correct CWL
eclat compile # creates library acme.forestry and compiles it
eclat version 1.0.0.0 # sets the library's version
eclat wrap # now creates a wrapped version of the library
```

The equivalent in the `CMD` shell would be:

```
set ECLAT_CWL=acme.forestry
eclat compile
eclat version 1.0.0.0
eclat wrap
```

### Service package specification

Now we must create a library to contain an implementation of the tree counter service. 

We're going to call it: 

    acme.pioneer
    
The following Allegra or `bash` shell commands could be used to create the library and set the
service definition library as one of its dependencies: 

```
export ECLAT_CWL=acme.pioneer # change to the correct CWL
eclat dep acme.forestry
```

The package specification might look like this:

```ada
with Acme.Forestry;

package Acme.Pioneer
is
   type Tree_Counter is new Acme.Forestry.Tree_Counter with private;

   procedure Reset (Counter: access Tree_Counter);
   
   procedure Add (Counter: access Tree_Counter;
                  Number:  in Natural := 1);
   
   procedure Get_Count (Counter: access Tree_Counter;
                        Count: out return Natural);
   
   procedure Startup (Counter: access Tree_Counter);
   procedure Shutdown (Counter: access Tree_Counter);

private
   task type Tree_Counter
   is
      new AdaOS.Services.Controlled.Controlled_Service 
   with
      overriding entry Reset;
      overriding entry Add (Number:  in Natural := 1);
      overriding entry Get_Count (Count: out return Natural);
      overriding entry Start_Up;
      overriding entry Shutdown;
   end; 

end;
```

..........


### Service Package Body

.....

```ada
package body Acme.Pioneer
is
   task body Tree_Counter
   is
      Is_Ready: Boolean := False;
      The_Count: Natural := 0;
   begin      
      loop
         select
            accept Reset;
            if not Ready then raise Program_Error; end if;
            The_Count := 0;
         or
            accept Add (Number: in Natural := 1)
            do
               if not Ready then raise Program_Error; end if;
               The_Count := @ + Number;
            end;
         or
            accept Get_Count (Count: out return Natural)
            do
               if not Ready then raise Program_Error; end if;
               Count := The_Count;
            end;
         or
            accept Start_Up;
            if Ready then raise Program_Error; end if;
            Ready := True;
         or
            accept Shutdown;
            if not Ready then raise Program_Error; end if;
            Ready := False;
         or
            terminate;
         end select;
      end loop;
   end; 
   
   The_Counter: aliased constant Tree_Counter;

begin
   AdaOS.Services.Default_Services.Insert ("acme.forestry.tree-counter", The_Counter'Access);

end Acme.Pioneer;
```

..............

Note that we named the service:

    acme.forestry.tree-counter
    
There is no reference to 'Pioneer'. This is because (the authors of) programs wishing to use 
the service are (assumed to be) not interested in the implementation of the service, but 
instead only on what the service interface is, and what its operations promise to do. 

..............


### Main Subprogram

We would have the usual main subprogram for a service program: 

```ada
with AdaOS.Execution;
with Acme.Pioneer;

procedure Main
is
begin
   AdaOS.Services.Await_Program_Termination;
end;
```

.....

It is vital that we create a need (in the technical sense) of the package `Acme.Pioneer`, 
even though we do not apparently use it at all. This is to force the package to be included 
in the build, and to ensure that it is initialised. 


### Building the Service Program

The following `bash` shell commands could be used to configure, compile, and build this service 
program: 

```
eclat build
```

This command creates a build, a module, an assembly, and a program, all named `acme.pioneer`. 

.........


### Test Program

...... a program that will engage the tree counter service and use it to count up some trees. 

......

```ada
with Ada.Command_Line;
with Ada.Text_IO, Ada.Integer_Text_IO;
with AdaOS.Services.Controlled;
with Acme.Forestry;

use Ada.Text_IO, Ada.Integer_Text_IO;

procedure Main
is
   Counter: 
      aliased constant Acme.Forestry.Tree_Counter := 
         AdaOS.Services.Controlled.Services ("acme.forestry.tree-counter");
   Trees: Natural;
   Total: Natural;
begin
   Put_Line ("Starting tree counter test");
   Counter.Reset;
   for Arg in 1 .. Ada.Command_Line.Argument_Count
   loop
      Trees := Natural'Value (Arg);
      Put ("Adding: ");
      Put (Trees);
      Put (" trees");
      New_Line;
      Counter.Add (Trees);   
   end loop;
   Counter.Get_Count (Trees);
   Put ("Total: ");
   Put (Total);
   Put (" trees");
   New_Line;
   Counter.Reset;
end;
```

............

```
export ECLAT_CWL=acme.forestry-test
eclat dep acme.forestry
eclat build
```

...........


## Realising the Test

The following `bash` shell or `CMD` shell commands can be used to realise an executable which 
will contain the service program and the program to test it: 

```
pxcr image tc-test program acme.forestry-test startup main
pxcr image tc-test service acme.forestry.tree-counter program acme.pioneer
pxcr image tc-test make-exe
```

The resulting executable image (file) `tc-test` will be put into the ECLAT `bin` directory 
(on the Windows platform, the file will be named `tc-test.exe`). 


### Running the Test

The following command should run the tree counter test program we have built and realised: 

```
tc-test 23 45 67
```

It should output the following: 

```
Starting tree counter test
Adding: 23 trees
Adding: 45 trees
Adding: 67 trees
Total: 135 trees
```

..............



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







