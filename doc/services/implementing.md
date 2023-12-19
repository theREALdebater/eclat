-----------------------------------------------------------------------------------------------
# Implementing a Service Program



.....



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
               <property name="location" type="acme.ssc.weather.location"/>
               <property name="date" type="ada.calendar.time"/>
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

This command creates a build, a module, a partition, and a program, all named `acme.pioneer`. 

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







