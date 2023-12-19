-----------------------------------------------------------------------------------------------
--/ AdaOS.Execution package specification:

with Ada.Calendar;
with AdaOS.Objects, AdaOS.Objects.Persistence, AdaOS.Remote_Streams, AdaOS.Security;

use AdaOS.Objects, AdaOS.Objects.Persistence;

package AdaOS.Execution is

   pragma Remote_Types;

   --/ Programs:

   type Executable_Program is synchronized interface and System_Object;

   type Program_Access is access all Executable_Program'Class;

   --| An 'executable program' is an entity that exhibits a certain behaviour (typically some
   --| useful job) when it is 'executed'. This behaviour can (and usually does) vary depending
   --| on the value of input to the program, which optionally includes a series of strings
   --| called 'program arguments'.

   function Name (Program: in Executable_Program) return Wide_String is abstract;

   --| The Name function returns the full name of Program, which should (as far as is
   --| practicable) uniquely identify the program (to a human). It should include the program's
   --| version (but not its revision). Not intended to be the file name of the program.

   function Form (Program: in Executable_Program) return Wide_String is abstract;

   function UMID (Program: in Executable_Program) return Wide_String is abstract;

   function Description (Program: in Executable_Program) return Wide_String is abstract;

   --\
   --------------------------------------------------------------------------------------------
   --/ Processes:

   type Program_Process is synchronized interface and System_Object;

   type Process_Access is access all Program_Process'Class;

   --| When an executable program is executed, the result is a 'program process'. An executable
   --| program can generally be executed many times, to produce multiple program processes.
   --| These program processes will all exhibit the behaviour of the executable program, but
   --| may have different input (inlcuding possibly having different program arguments), which
   --| will cause each different program process' behaviour to vary accordingly.

   function Program (Process: in Program_Process) return Program_Access is abstract;

   --| The Program function returns a pointer to the executable program that was executed to
   --| produce the given program process.

   --\
   --------------------------------------------------------------------------------------------
   --/ Process Status:

   type Process_Status is (Setup, Stopped, Running, Terminated);

   --| A program process can be in one of four states: setup; stopped; running; terminated.

   --| When a process is created, it is initially in the setup state. From this state it can
   --| move into any of the other three, but it can never return to this state from another
   --| one. A process can move between the stopped and running states any number of times. It
   --| can move from either of these two states into the terminated state. Once in the
   --| terminated state, a process must remain in this state.

   --| A program process only ever exhibits any behaviour (does anything) when it is in the
   --| running state.

   function Status (Process: in Program_Process) return Process_Status is abstract;

   --| The Status function returns the current state of a given program process.

   procedure Run  (Process: in out Program_Process) is abstract;
   procedure Stop (Process: in out Program_Process) is abstract;
   procedure Kill (Process: in out Program_Process) is abstract;

   --| The Run, Stop, and Kill procedures move a given program process into a particular state
   --| (unless it is already in that state, in which case the procedure does nothing). Run
   --| moves it into the executing state, Stop into the stopped state, and Kill into the
   --| terminated state. Run and Stop propagate Status_Error if the process is in the
   --| terminated state.











I don't think we need this version:

   procedure Await_Termination (Process:   in  Program_Process;
                                Timeout:   in  Ada.Calendar.Time;
                                Timed_Out: out Boolean) is abstract;

   --| The Await_Termination procedure blocks the calling task until either a given program
   --| process enters the terminated state or a certain time is reached. If the process is in
   --| the terminated state when the procedure is called, the procedure does nothing (and
   --| returns immediately). If the time is reached (or if the procedure is called at a time
   --| that is already past the given time) before the process becomes terminated, Timed_Out is
   --| set to True, otherwise (when the process is terminated) it is set to False.


Replace it with:

   procedure Await_Termination (Process: in Program_Process) is abstract;

   --| The Await_Termination procedure blocks the calling task until a given program process
   --| enters the terminated state.

   --| Because Program_Process is a synchronised type, it is possible for a call to this
   --| procedure to be made an ATC .........

   -- select
   --    P.Await_Termination;
   --    -- actions after termination occurs
   -- or
   --    delay D;
   --    -- actions after wait timed out
   -- end select;










   --\
   --------------------------------------------------------------------------------------------
   --/ Program Arguments:

   function Argument_Count (Process: in Program_Process) return Natural is abstract;

   --| The Argument_Count function returns the number of program arguments (strings) passed
   --| into a given program process when it was executed.

   function Argument (Process: in Program_Process;
                      Index:   in Positive) return String is abstract;

   --| The program arguments, a sequence of strings, passed into a program process P are
   --| numbered from 1 to Argument_Count(P) according to the sequence. The function
   --| Argument(P,N) returns the Nth program argument. Constraint_Error is propagated if N >
   --| Argument_Count(P).

   procedure Append_Argument (Process: in Program_Process;
                              Value:   in String);

   --| The Append_Argument procedure passes one extra program argument (string) into a program
   --| procedure. If, before the procedure Append_Argument(P,V) is executed, Argument_Count(P)
   --| is N, then after the procedure is executed Argument_Count(P) is N+1 and Argument(P,N+1)
   --| is V. Status_Error is propagated if P is not in the setup state.

   --\
   --------------------------------------------------------------------------------------------
   --/ Exit Code:

   type Process_Exit_Code is new Integer;

   --| When a program process enters the terminated state, it can output an integer value
   --| called its 'exit code', which is usually used to indicate whether the execution of the
   --| process was (as far as it could tell) successful or not, and possibly to give a (crude)
   --| indication of what went wrong. For portability, it is suggested that negative values and
   --| values greater than 255 are never used.

   Successful_Exit: constant Process_Exit_Code := 0;

   --| An exit code of 0 is usually used to indicate successful execution.

   function Exit_Code (Process: in Program_Process) return Process_Exit_Code is abstract;

   --| The Exit_Code function returns the exit code of a given program process. Status_Error is
   --| propagated if the process is not in the terminated state.

   procedure Set_Exit_Code (Process:   in Program_Process;
                            Exit_Code: in Process_Exit_Code) is abstract;

   --| The Set_Exit_Code procedure sets the exit code of a given program process. Status_Error
   --| is propagated if the process is in the terminated state.

   --\
   --------------------------------------------------------------------------------------------
   --/ Environment:

   --| Every program process is associated with a set of 'environment variables'. Each
   --| environment variable has a value and a name, both the value and name are Unicode
   --| strings. The name uniquely identifies the variable within the set. The process can read
   --| the value of any environment variable, change the value of a variable, add a new
   --| variable to the set, or delete a variable from the set.

   --| Every process (except the Top Process) initially inherits the same set of environment
   --| variables from the process that creates it (by calling ???). They can then be modified
   --| (by calling Set_Environment_String and Set_Environment_Object whilst the process is in
   --| the setup state.

   function Environment_Variable (Process: in Program_Process;
                                  Name:    in Wide_String) return Wide_String is abstract;

   --| The Environment_Variable function returns the value of an environment string of a
   --| process. If the variable does not exist, the function returns "".

   procedure Set_Environment_Variable (Process: in out Program_Process;
                                       Name:    in     Wide_String;
                                       Value:   in     Wide_String) is abstract;

   --| The Set_Environment_Variable procedure changes the value of an environment string of a
   --| process. If the variable does not exist, it is created (unless Value is ""). If Value is
   --| "", the variable is deleted (unless it already doesn't exist). Propagates the
   --| Status_Error exception if the process is not in the setup state.

   --/ Iteration:

   procedure Iterate_Environment_Variabless
      (Process: in out Program_Process;
       Action:  not null access procedure (Value: in Wide_String)) is abstract;

   --| ....

   --\

   --\
   --------------------------------------------------------------------------------------------
   --/ Files:

   --| Every process has access to three separate file trees, corresponding to: files that are
   --| exclusive to the workstation ('unit') the process is (currrently) executing on; files
   --| that are exclusive to the cluster that that workstation belongs to; files that are
   --| common throughout the entire system.

   --| In fact, there will normally be members somewhere in the file tree of each cluster
   --| corresponding to the roots of the files trees of all the workstations in that cluster,
   --| and, similarly, there will normally be members somewhere in the system file tree
   --| corresponding to the roots of the file trees of all the clusters in the system.

   function System_Files (Process: in Program_Process)
                                                    return Object_Container_Access is abstract;

   function Cluster_Files (Process: in Program_Process)
                                                    return Object_Container_Access is abstract;

   function Unit_Files (Process: in Program_Process)
                                                    return Object_Container_Access is abstract;


   --| The three functions System_Files, Cluster_Files, and Unit_Files return access values
   --| referencing a directory in each of the three file trees. In each tree, the directory the
   --| function returns can be anywhere in the tree, and is termed the 'current directory' of
   --| the tree.

   --| For a process executing in distributed mode, the result of calling the Unit_Files could
   --| be different from moment to moment. (It is suggested, therefore, that distributed
   --| programs refrain from using this file tree.)

   --| If the corresponding file tree is not available (for reasons other than a serious error
   --| or malfunction), the corresponding function will return a null access value. No current
   --| version of AdaOS does this, but for future compatibility all programs should be written
   --| to accommodate (as gracefully as possible) the possibility that any of these functions
   --| returns a null access value.

   procedure Set_System_Files (Process:   in out Program_Process;
                               Directory: in     Object_Container_Access) is abstract;

   procedure Set_Cluster_Files (Process:   in out Program_Process;
                                Directory: in     Object_Container_Access) is abstract;

   procedure Set_Unit_Files (Process:   in out Program_Process;
                             Directory: in     Object_Container_Access) is abstract;

   --| The procedures Set_System_Files, Set_Cluster_Files, and Set_Unit_Files can be used to
   --| set the values returned by the three functions, for a process. If the process is not in
   --| the setup state, Status_Error is propagated.

   --\
   --------------------------------------------------------------------------------------------
   --/ Security:

   function Ambit (Process: in Program_Process) return Security.Authority_Set is abstract;

   --| ...

   procedure Set_Ambit (Process: in out Program_Process;
                        Ambit:   in     Security.Authority_Set;
                        Default: in     Security.Authority_ID) is abstract;

   procedure Set_Ambit (Process: in out Program_Process;
                        Ambit:   in     Security.Authority_Set) is abstract;

   --| ...

   function Default_Authority (Process: in Program_Process)
                                                      return Security.Authority_ID is abstract;

   --| ...

   procedure Set_Default_Authority (Process:   in out Program_Process;
                                    Authority: in     Security.Authority_ID) is abstract;

   --| ...

   --\
   --------------------------------------------------------------------------------------------
   --/ Limits and Accounting:

   ...

   --\
   --------------------------------------------------------------------------------------------
   --/ Standard Files:

   type Standard_File_Designator is range 0..255;

   type Standard_File_Status is (Unavailable, Input_Stream, Output_Stream);

   Standard_Output_File:      constant Standard_File_Designator := 0; -- name "stdout"
   Standard_Input_File:       constant Standard_File_Designator := 1; -- name "stdin"
   Standard_Error_File:       constant Standard_File_Designator := 2; -- name "stderr"
   Standard_Printer_File:     constant Standard_File_Designator := 3; -- name "stdprt"
   Standard_Aux_Out_File:     constant Standard_File_Designator := 4; -- name "stdaux
   Standard_Aux_In_File:      constant Standard_File_Designator := 5; -- name "stdauxin"
   Standard_Log_File:         constant Standard_File_Designator := 6; -- name "stdlog"
   Standard_Automation_File:  constant Standard_File_Designator := 7; -- name "stdauto"
   Standard_Debug_File:       constant Standard_File_Designator := 8; -- name "stddbg"
   Standard_System_File:      constant Standard_File_Designator := 9; -- name "stdsys"

   function Standard_File (Process:    in Program_Process;
                           Designator: in Standard_File_Designator)
                                        return Remote_Streams.Remote_Stream_Access is abstract;

   procedure Set_Standard_File (Process:    in out Program_Process;
                                Designator: in     Standard_File_Designator;
                                File:       in     Remote_Streams.Remote_Stream_Access)
                                                                                   is abstract;

   --| ???

   function Status (......









   --\
   --------------------------------------------------------------------------------------------
   --/ Subprocesses:

   function New_Process (Program: in out Executable_Program;
                         Super:   in     Process_Access := null)
                                                             return Process_Access is abstract;

   --| .... The parameter Super can be set to null to indicate that the calling process be the
   --| superprocess of the new process. The new process is initially in the seutp state. All
   --| other settings are inherited from the process specified as the superprocess of the new
   --| process. Whilst the new process remains in the setup state these settings can be
   --| modified as needed before starting the new process running.

   procedure Iterate_Subprocesses
      (Process: in out Program_Process;
       Action:  not null access procedure (Subprocess: in Process_Access)) is abstract;


   --\
   --------------------------------------------------------------------------------------------
   --/ Debugging:

   --| ... suspend/reactivate, serialise/deserialise, kernel/machine-level access

   --| Idea: declare derived type Debuggable_Process in child package?

   --\
   --------------------------------------------------------------------------------------------
   --/ Exceptions:

   Status_Error: exception;






   --\

end;

