-----------------------------------------------------------------------------------------------
with .....;

use .....;

package AdaOS.Instances
with
    Remote_Types
is
-----------------------------------------------------------------------------------------------
--/ Executional instances

   type Executional_Instance is abstract limited new System_Object with private;

   type Instance_Access is access all Executable_Instance'Class;

--| An _executional instance_ is one execution of a program startpoint. A program startpoint can 
--| generally be executed many times, in sequence or in parallel, and each time its execution 
--| will be represented as a different executional instance. 
--| 
--| These instances will all exhibit the behaviour of the startpoint, but may have different 
--| input (inlcuding possibly having different program arguments), which will cause each 
--| different instance's behaviour to vary accordingly.

   package Task_Instance
   is
      function Value (Id: in Task_Id := Current_Task) return Instance_Access;
   end;

--| The function `Task_Instance` returns (a remote access value referencing) the 
--| executional instance of the calling task or an explicitly identified task. 

   function Startpoint (Instance: not null access Executable_Instance := Task_Instance) 
   return 
      Partition_Access is abstract;

--| The function `Startpoint` returns (a remote access value referencing) the program 
--| startpoint that the calling task's instance or a given instance is an execution of. 

--| Every executional instance is associated with a _compartment_. 

   function Compartment (Instance: not null access Executional_Instance := Task_Instance) 
   return 
      Compartment_Access is abstract;

--| The function `Compartment` returns (a remote access value referencing) the program 
--| compartment of the calling task's instance or a given instance. 

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
    --/ 

--| Every task has a _controller_, which is an executional instance. The controller of a task is 
--| set to be the executional instance of the task's master. 

    function Self return Executional_Instance'Class;

--| The function `Self` returns the controller of the calling task.

    --\
    --------------------------------------------------------------------------------------------
    --/ 

--| 



    --\
    --------------------------------------------------------------------------------------------
    --/ 

--| 



    --\
    --------------------------------------------------------------------------------------------
    --/ 

--| 



    --\
    --------------------------------------------------------------------------------------------
    --/ Finalisation

--| 

    type Controlled_Object is private;

    type Controlled_Access is access all Controlled_Object;


    --\
    --------------------------------------------------------------------------------------------
    --/ 
    .....

    procedure Run (Instance: not null access Executional_Instance);

--| 



    --\
    --------------------------------------------------------------------------------------------
    --/ Shutdown

    function Shutdown return Shutdown_Level;

--| 


    --\
    --------------------------------------------------------------------------------------------
    --/ Operations aimed at classes that inherit from `Executional_Instance`

--|

    procedure Check_Change_From_Setup_To_Stopped (Instance: in Executional_Instance) is null;





    procedure Change_From_Setup_To_Running (Instance: in Executional_Instance);



    procedure Change_From_Stopped_To_Running (Instance: in Executional_Instance);




--| These procedure should be overridden if appropriate. Almnost certain an overriding 
--| procedure should call the same procedure of the parent class at some point. Only the event 
--| receiver task should ever call any of these procedures (to ensure synchronisation).

    --\
-----------------------------------------------------------------------------------------------
private

    --/ Environment task

    task type Environment_Task (Self: access Executional_Instance'Class) is end;

    type Environment_Task_Access is access Environment_Task;

--| 

    --\
    --/ Event receiver task

    task type Event_Receiver (Self: access Executional_Instance'Class)
    is


    end;

--| 

    --\
    --/ 

    protected type Shutdown_State
    is
        function Level return Shutdown_Level;
        procedure Set_Level (Level: in Shutdown_Level);
        entry Await_Level (Level: in Shutdown_Level);
    end;

--| 

    --\
    --/ 

--| 



    --\
    --/ Finalisation

    type Controlled_Object
    is
        record
            Controller: Instance_Access := Self;
        end record;

--| The _controller_ of a controlled object is the excutional instance of the task that 
--| allocated the object. 

    type Unfinalized_Count is new Natural range 0 .. ?????;

    subtype Unfinalized_Index is Unfinalized_Count range 1 .. Unfinalized_Count'Last;

    type Unfinalized_Registry is
        array (Unfinalized_Index) of Controlled_Access := (others => null); 

--| The `Unfinalized_Registry` of an instance holds pointers to all the controlled objects 
--| allocated by tasks of the instance. When an object is finalised, it is removed from the 
--| array (its pointer is set to null). 



    --\
    --/ Executional instance

    type Executional_Instance
    is
        abstract limited new System_Object
    with
        record
            Mode: Execution_Mode := Setup; -- current execution mode of this instance
            Receiver: Event_Receiver_Access := new Event_Receiver (Executional_Instance);
            Main_Task: Environment_Task_Access := null; -- non-null when Running
            Main_Zombie: Serialized_Task_Access := null; -- non-null when Stopped
            Shutdown: Shutdown_State;
            Unfinalized: Unfinalized_Registry;

        end record;

--| 

--| 

--| 

--| 

--| 

--| 


    --\
-----------------------------------------------------------------------------------------------
end;

