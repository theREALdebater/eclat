   --/ Locking:

   type Locking_Status is (Unlocked,
                           Read_Only,
                           Full_Access);

   --| A system object can be in one of three locking states: unlocked; read-only;
   --| full access. It is initially unlocked.

   subtype Locking_Mode is Locking_Status range Read_Only..Full_Access;

   --| A system object can be locked in one of two locking modes: read-only;
   --| full access.

   function Status (Object: in System_Object) return Locking_Status is abstract;

   --| The Status function returns the current locking state of a system object.

   procedure Obtain_Lock (Object:   in out System_Object;
                          Obtained: out    Boolean;
                          Timeout:  in     Ada.Calendar.Time;
                          Mode:     in     Locking_Mode := Full_Access) is abstract;

   --| The Obtain_Lock procedure attempts to obtain a level of access for the authority
cited by the calling
   --| task to a system object.

   --| The level of access can be either: shared, not permitting the object
   --| to be modified (read-only mode); exclusive, without any restrictions on
   --| modification of the object (full access mode).

   --| The required mode is in Mode.

   --| If the object is unlocked at the time of call, the lock is obtained. If it is read-only
and Mode
   --| is Read_Only, the lock is obtained. Otherwise, the calling task is blocked until
   --| either the conditions change to enable the lock to be obtained (according to the
   --| conditions just stated) or the time in Timeout is reached. True is returned in
   --| Obtained (only) if the lock is obtained. If and when the lock is obtained, the
   --| object's locking state is/becomes Mode.

   procedure Unlock (Object: in out System_Object) is abstract;

   --| The Unlock procedure changes the locking state of a system object to Unlocked.
   --| If the ...

   Locking_Error: exception;







   procedure Read (Stream: in out Remote_Stream;
                   Item:   out    Stream_Element_Array;
                   Last:   out    Stream_Element_Offset) is abstract;

   procedure Write (Stream: in out Remote_Stream;
                    Item:   in     Stream_Element_Array) is abstract;








-----------------------------------------------------------------------------------------------
--/ Environment:

--| Every executional instance is associated with a set of _environment variables_. Each
--| environment variable has a name and a value. The name is a Unicode string, but the value is 
--| a reference to any system object. 
--| 
--| The name uniquely identifies the variable within the set. The instance can read the value 
--| of any environment variable, change the value of a variable, add a new variable to the set, 
--| or delete a variable from the set.
--| 
--| When an instance (an object of a type in `Executional_Instance'Class`) is created, its 
--| environment variables are initially a copy of those of the instance that created it. The 
--| variables of the new instance can then be modified (by calling the procedure 
--| `Set_Environment_Variable` whilst the instance is in the setup mode.

   type Environment_Variable is 
      synchronized interface and Copyable_Object and Deletable_Object;
   
--| The interface type `Environment_Variable` represents a system object that is suitable to be 
--| an environment variable value. Such an object must be copyable and deletable. 
--| 
--| Traditional environment variables can only have a string as their value. The standard Ada 
--| package `Ada.Environment` reflects this. It is a convention that these traditional 
--| environment variables have values of a type in `AdaOS.Objects.Text.Text_Object'Class`, and 
--| that implementations of `Ada.Environment` only return those environment variables that have 
--| a value in this class. 

   type Variable_Access is access all Environment_Variable'Class;

--| .....

   function Environment_Variable_Exists (Instance: access Executional_Instance;
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
--\
