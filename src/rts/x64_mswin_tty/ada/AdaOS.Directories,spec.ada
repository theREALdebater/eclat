with Ada.Strings.Wide_Unbounded;
with AdaOS.Objects;
with AdaOS.Containers;

use Ada.Strings.Wide_Unbounded;
use AdaOS.Objects;
use AdaOS.Containers;

package AdaOS.Directories
with
   Remote_Types
is
-----------------------------------------------------------------------------------------------
--/ Paths

   type Path_Resolution_Scheme is interface;

--| .....

   Default_Resolution: constant Path_Resolution_Scheme'Class; -- private
   
--| .....
   
   
   
   
   type Path_Array is array (Positive range <>) of Wide_Unbounded_String;
   
--| .....
   
   function To_Array (Scheme: in Path_Resolution_Scheme;
                      Path:   in Wide_String) return Path_Array is abstract;
   
--| .....
   
   function To_Array (Path: in Wide_String) return Path_Array
   is
      (Default_Resolution.To_Array (Path));
   
--| .....
   
--\   
-----------------------------------------------------------------------------------------------
--/ Directories

--| An _object directory_ is an object container that also keeps a name for each of its 
--| members. A name is a wide string value. Each member is uniquely identified, within the 
--| container, by its name. Thus the names must always by unique within the container. 

   type Object_Directory is abstract limited new Object_Container with private;
   
   type Directory_Access is access all Object_Directory'Class;

--| The interface type `Object_Directory` represents an object directory. 

--| .....


--\   
-----------------------------------------------------------------------------------------------
--/ Engagement

   procedure Engage (Directory: not null access Object_Directory) is abstract;
   procedure Disengage (Directory: not null access Object_Directory) is abstract;

   function Is_Engaged (Directory: in Object_Directory) return Boolean is abstract;

--\   
-----------------------------------------------------------------------------------------------
--/ Finding

   function Find (Directory: access Object_Directory;
                  OID:       in     Object_Id) return Object_Access is abstract;

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




   function Name (Directory: access Object_Directory;
                  OID:       in     Object_Id) return Wide_String is abstract;

--| .....





   function Resolve (Directory: in Object_Directory;
                     Path:      in Path_Array) return Object_Id is abstract;

--| .....

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




