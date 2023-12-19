-----------------------------------------------------------------------------------------------
--/ AdaOS.Execution package specification:

with Ada.Calendar;
with AdaOS.Objects, AdaOS.Remote_Streams, AdaOS.Security;

use AdaOS.Objects;

package AdaOS.Execution 
is
   pragma Remote_Types;

--/ Startpoints:

   type Program_Partition is abstract limited new System_Object with private;

   type Partition_Access is access all Program_Partition'Class;

--| A _program partition_ is roughly equivalent to a tradtional executable (file or program). 

   function Name (Program: not null access Program_Partition) return Wide_String is abstract;

--| The function `Name` returns the full name of a program startpoint, which should (as far as is
--| practicable) uniquely identify the startpoint (to a human). It should include the startpoint's
--| version (but not its revision). this name is not intended to be the file name of the 
--| ?????.

   ????? function Form (Program: not null access Program_Partition) return Wide_String is abstract;

   ????? function UMID (Program: not null access Program_Partition) return Wide_String is abstract;

   function Description (Program: not null access Program_Partition) return Wide_String is abstract;
   
--| ?????

--\
-----------------------------------------------------------------------------------------------
--/ Executable instances

   type Executable_Instance is abstract limited new System_Object with private;

   type Instance_Access is access all Executable_Instance'Class;

--| An _executable instance_ is one execution of a program startpoint. A program startpoint can 
--| generally be executed many times, in sequence or in parallel, and each time its execution 
--| will be represented as a different executable instance. 
--| 
--| These instances will all exhibit the behaviour of the startpoint, but may have different 
--| input (inlcuding possibly having different program arguments), which will cause each 
--| different instance's behaviour to vary accordingly.

   function Startpoint (Instance: access Executable_Instance) 
      return 
         Partition_Access is abstract;

--| The function `Startpoint` returns a remote access value referencing the program startpoint that 
--| the instance is an execution of. 

--| Every executional instance is associated with a _compartment_. 

   function Compartment (Instance: not null access Executional_Instance) 
   return 
      Compartment_Access is abstract;

--| ?????


--\
-----------------------------------------------------------------------------------------------
--/ Instance modes

   type Instance_Mode is (Setup, Stopped, Running, Terminated);

--| An executional instance can be in one of four _instance modes_: 
--| 
--|  * setup; 
--| 
--|  * stopped; 
--| 
--|  * running; 
--| 
--|  * terminated.

--| When an instance is created, it is initially in the setup mode. From this mode it can
--| move into any of the other three, but it can never return to the setup mode. An instance 
--| can move between the stopped and running states any number of times. It can move from 
--| either of these two modes into the terminated mode. Once in the terminated mode, an 
--| instance must remain in this mode. 

--| An instance only ever exhibits any behaviour (does anything) when it is in the running 
--| mode.

   function Mode (Instance: not null access Executional_Instance) return Process_Mode is abstract;

--| The function `Mode` returns the current state of a given executable instance.

   procedure Run  (Instance: not null access Executional_Instance) is abstract;
   procedure Stop (Instance: not null access Executional_Instance) is abstract;
   procedure Kill (Instance: not null access Executional_Instance) is abstract;

--| The procedures `Run`, `Stop`, and `Kill` move a given executional instance into a specific 
--| mode unless the instance is already in that mode, in which case the procedure does nothing. 
--| `Run` moves it into the running mode, `Stop` into the stopped mode, and `Kill` into the
--| terminated mode. `Run` and `Stop` propagate the exception `Mode_Error` if the instance is 
--| in the terminated state. 

   procedure Await_Termination (Instance: not null access Executional_Instance) is abstract;

--| The procedure `Await_Termination` blocks the calling task until a given executional 
--| instance enters the terminated mode. If it is already terminated, the procedure returns 
--| immediately. 

--\
-----------------------------------------------------------------------------------------------
--/ Sub-instances:

--| An executional instance can run a program startpoint, which causes a new instance to be 
--| created. This is called _spawning_ a new instance. 
--| 
--| An executional instance can have a _parent_ instance, that has control over it (in the 
--| security sense). An instance that has no parent is called a _top_ instance. 
--| 
--| Only the [RTS](../rts/RTS.md) can create top instances. 
--| 
--| This parent-child relationship between instances creates a hierarchy (tree). 

   function Spawn (Startpoint: in out Program_Partition;
                   Parent:   in     Instance_Access := null) 
      return 
         Instance_Access is abstract;

--| .... 
--| 
--| The parameter `Parent` can be set to null to indicate that the calling instance be the 
--| parent of the new instance. The new instance is initially in the setup mode. Most 
--| other settings are inherited from the parent. Whilst the new instance remains in the setup 
--| mode, its settings can be modified as needed before it starts running. 

   procedure Iterate_Child_Instances
      (Parent:  access Executional_Instance;
       Process: not null access procedure (Subinstance: in Instance_Access)) is abstract;

--\
--------------------------------------------------------------------------------------------
--/ Shutdown:

type Shutdown_Level is (Normal, Caution, Warning, Imminent);

--\
--------------------------------------------------------------------------------------------
--/ Debugging:

--| ... suspend/reactivate, serialise/deserialise, kernel/machine-level access

--| Idea: declare derived type Debuggable_Instance in child package?

--\
--------------------------------------------------------------------------------------------
--/ Exceptions:

   Mode_Error: exception;
   
--| .....

   _Error: exception;
   
--| .....

   _Error: exception;
   
--| .....

   _Error: exception;
   
--| .....

   _Error: exception;
   
--| .....






   --\

end;

