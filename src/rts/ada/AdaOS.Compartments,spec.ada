with Ada.Strings.Wide_Unbounded;
with AdaOS.Objects;
with AdaOS.Containers;

use Ada.Strings.Wide_Unbounded;
use AdaOS.Objects;
use AdaOS.Containers;

package AdaOS.Compartments
with
   Remote_Types
is
-----------------------------------------------------------------------------------------------
--/ Compartments

--| A _compartment_ 

--| When a compartment (an object of a type in `Principal_compartment'Class`) is created, its 
--| environment variables are initially a copy of those of the instance that created it. The 
--| variables of the new instance can then be modified (by calling the procedure 
--| `Set_Environment_Variable` whilst the instance is in the setup mode.
--| 


   type Program_Compartment is abstract limited new Object_Directory with private;

   type Compartment_Access is access all Principal_Compartment'Class;


--\
-----------------------------------------------------------------------------------------------
--/ Engagement

--| 

   procedure Engage (Compartment: not null access Program_Compartment) is abstract;

   procedure Disengage (Compartment: not null access Program_Compartment) is abstract;

   function Is_Engaged (Compartment: in Program_Compartment) return Boolean is abstract;




--\   
-----------------------------------------------------------------------------------------------
--/ Name Resolution

--| 

   function Name (Directory: access Object_Directory;
                  OID:       in     Object_Id) return Wide_String is abstract;

--| .....





   function Resolve (Directory: in Object_Directory;
                     Path:      in Path_Array) return Object_Id is abstract;

--| .....

--\
-----------------------------------------------------------------------------------------------
--/ Finding

   function Find (Compartment: in Principal_Compartment;
                  OID:         in Object_Id) return Object_Access is abstract;

   function Find (Compartment: in Program_Compartment;
                  OID:         in Object_Id) return Object_Access is abstract;

--| 

   function Find (Directory: access Object_Directory;
                  Name:      in     Wide_String) return Object_Access is abstract;

--| The function `Find` retrieves the member identified by the given `Name`, and returns  
--| a remote access value referencing a system object. 

--| The given `Name` can be a path, in which case the directory attempts to resolve the path. 
--| The path must be relative, meaning that the first element of the path must be the name of 
--| one of the members of the directory. 

--| If the name or path cannot be resolved (an element of the path does not exist), `null` is 
--| returned. 

   function Contains (Directory: access Object_Directory;
                      Name:      in     Wide_String) return Boolean is abstract;

--| The function `Contains` returns `True` only if a given name is that of a system object 
--| which is a member of a given namesystem container. 

--| If the name or path cannot be resolved (an element of the path does not exist), `null` is 
--| returned. 


--\
-----------------------------------------------------------------------------------------------
--/ Iteration

   procedure Iterate_Names (Directory: access Object_Directory;
                            Process:  not null access procedure (Name: in Wide_String; 
                                                                 OID:  in Object_Id)) 
      is abstract;

--| ....




--\
--\













--\


-----------------------------------------------------------------------------------------------
--/ Program Arguments:


...... function `Program_Name` ......



...... procedure `Set_Program_Name` ......


   function To_Array (Instance: in Executional_Instance) return Program_Argument_Array is abstract;

   procedure Set_Arguments (Instance:  not null access Executional_Instance;
                            Arguments: in Program_Argument_Array);


   function Argument_Count (Instance: access Executional_Instance) return Natural is abstract;

--| The function `Argument_Count` returns the number of program arguments (strings) passed
--| into a given program process when it was executed.

   function Argument (Instance: access Executional_Instance;
                      Index:   in Positive) return String is abstract;

--| The function `Argument` returns the value of one of the program arguments of an executional 
--| instance. The program arguments, a sequence of strings, passed into an instance P are
--| numbered from 1 to `Argument_Count(P)` according to the sequence. The function
--| `Argument(P,N)` returns the `N`th program argument. 
--| 
--| The exception `Constraint_Error` is propagated if `N > Argument_Count(P)`.

   procedure Append_Argument (Instance: access Executional_Instance;
                              Value:    in String) is abstract;

--| The procedure `Append_Argument` adds one extra program argument (string) to those of an 
--| execuctional instance. 
--| 
--| If, before the procedure `Append_Argument(P,V)` is executed, `Argument_Count(P)` is `N`, 
--| then after the procedure is executed `Argument_Count(P)` is `N+1` and `Argument(P,N+1)` is 
--| `V`. 
--| 
--| The exception `Mode_Error` is propagated if `P` is not in the setup mode.

   procedure Clear_Arguments (Instance: access Executional_Instance) is abstract;

--| The procedure `Clear_Arguments` deletes all the arguments already appended, so that 
--| `Argument_Count` returns 0 again. Arguments can be appended again, as normal. 

--\
-----------------------------------------------------------------------------------------------
--/ Exit Code:

   type Exit_Status is new Integer;

--| When an executional instance enters the terminated mode, it can output an integer value, 
--| called its _exit code_, which is usually used to indicate whether the execution of the 
--| instance was (as far as it could tell) successful or not, and possibly to give a (crude)
--| indication of what went wrong. 
--| 
--| The type `Exit_Status` represents an exit code value. 
--| 
--| For portability, it is suggested that negative values and values greater than 255 are never 
--| used. 

   Success_Exit: constant Exit_Status := 0;
   Failure_Exit: constant Exit_Status := 1;

--| An exit code of 0 is usually used to indicate successful execution. The most commonly used 
--| failure code is 1. 

   function Exit_Status_Value (Instance: access Executional_Instance) 
      return 
         Exit_Status is abstract;

--| The function `Exit_Status_Value` returns the exit code of a given executional instance. 
--| 
--| The exception `Mode_Error` is propagated if the instance is not in the terminated mode.

   procedure Set_Exit_Status (Instance: access Executional_Instance;
                              Code:     in Exit_Status) is abstract;

--| The procedure `Set_Exit_Status` sets the exit code of a given executional instance. 
--| 
--| The exception `Mode_Error` is propagated if the instance is in the terminated state.

--\
-----------------------------------------------------------------------------------------------
--/ Environment:








--| Every compartment is associated with a set of _environment variables_. These are 
--| stored as the members of the compartment's directory `env`. 
--| 
--| Traditional environment variables can only have a string as their value. The standard Ada 
--| package `Ada.Environment` reflects this. It is a convention that these traditional 
--| environment variables have values of a type in `AdaOS.Objects.Text.Text_Object'Class`, and 
--| that implementations of the standard package `Ada.Environment` only return those 
--| environment variables that have a value in this class. 

   function Environment (Instance: access Executional_Instance'Class) return Directory_Access;

--| The function `Environment` returns a reference to the directory that contains the 
--| environment variables of an executional instance. It is possible for the returned value to 
--| be `null` which (legitimately) indicates there are no environment variables for the 
--| instance. 

--| This is a convenience function, which simply looks up the `env` member of the instance's 
--| compartment. 







??????? convenience functions?

   function Environment_Variable_Exists (Container: access Executional_Instance;
                                         Name:     in Wide_String) 
      return 
         Boolean is abstract;

--| The function `Environment_Variable_Exists` returns `True` if an environment variable of an 
--| executional instance exists (has been set), or `False` otherwise. 

   function Environment_Variable_Value (Instance: access Executional_Instance;
                                        Name:     in Wide_String) 
      return 
         Variable_Access is abstract;

--| The function `Environment_Variable_Value` returns the value of an environment variable of 
--| an executional instance. If the variable does not exist, the function returns `null`. 

   procedure Set_Environment_Variable (Instance: access Executional_Instance;
                                       Name:     in     Wide_String;
                                       Value:    in     Variable_Access) is abstract;

--| The Set_Environment_Variable procedure changes the value of an environment string of a
--| process. If the variable does not exist, it is created (unless Value is ""). If Value is
--| "", the variable is deleted (unless it already doesn't exist). Propagates the
--| Status_Error exception if the process is not in the setup state.

   procedure Clear_Environment_Variable (Instance: access Executional_Instance;
                                         Name:     in     Wide_String) is abstract;

--| ....

   procedure Clear_Environment_Variables (Instance: access Executional_Instance) is abstract;

--| ....

--/ Iteration:

   procedure Iterate_Environment_Variables (Instance: access Executional_Instance;
                                            Process:  not null access 
                                               procedure (Name:  in Wide_String; 
                                                          Value: in Variable_Access)) 
      is abstract;

--| ....





--\
-----------------------------------------------------------------------------------------------
--/ Security:

--| .....

   function Ambit (Instance: access Executional_Instance) 
   return 
      Security.Authority_Set is abstract;

--| ...

   procedure Set_Ambit (Instance: access Executional_Instance;
                        Ambit:    in     Security.Authority_Set;
                        Default:  in     Security.Authority_ID) is abstract;

   procedure Set_Ambit (Instance: access Executional_Instance;
                        Ambit:    in     Security.Authority_Set) is abstract;

--| ...

   function Default_Authority (Instance: access Executional_Instance)
   return 
      Security.Authority_ID is abstract;

--| ...

   procedure Set_Default_Authority (Instance:  access Executional_Instance;
                                    Authority: in     Security.Authority_ID) is abstract;

--| ...

--\
--------------------------------------------------------------------------------------------
--/ Limits and Accounting:

   ...

--\


