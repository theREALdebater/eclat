-----------------------------------------------------------------------------------------------
--/ Declaration

--| Copyright © 2022 The AdaOS Project
--| 
--| This file is part of ECLAT.
--| 
--| ECLAT is free software: you can redistribute it and/or modify it under the terms of the GNU 
--| General Public License as published by the Free Software Foundation, either version 3 of 
--| the License, or (at your option) any later version. 
--| 
--| ECLAT is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without 
--| even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
--| GNU General Public License for more details. 
--| 
--| You should have received a copy of the GNU General Public License along with ECLAT.  If 
--| not, see <http://www.gnu.org/licenses/>. 
--| 
--| As a special exception, if other files instantiate generics from this unit, or you link 
--| this unit with other files to produce an executable, this unit does not by itself cause 
--| the resulting executable to be covered by the GNU General Public License. This exception 
--| does not however invalidate any other reasons why the executable file might be covered by 
--| the GNU General Public License. 

--\ 
-----------------------------------------------------------------------------------------------

pragma License(Modified_GPL);

package AdaOS
with
   Pure
is
--------------------------------------------------------------------------------------------
--/ Names and Paths

--| A _path string_ is a string containing a path, which may be a file path of a typical 
--| host operating system, or a Universal Resource Indicator, or something similar. 

--| A _status string_ .....

--| A _description string_ .....

   type Path_String is new Standard.Wide_String;

--| The type `Path_String` represents a path string. 

   type Path_Access is access constant Path_String;

   type Path_Array is array (Positive range <>) of Path_Access;

--| The type `Path_Array` represents a list of multiple paths.

   subtype Name_String is Path_String;

   type Name_Access is access constant Name_String;

   type Name_Array is array (Positive range <>) of Name_Access;

--| A `Name_Array` is a path: a sequence of names.

   type Status_String is new Standard.Wide_String;

--| The type `Status_String` represents a status string.

   type Description_String is new Standard.Wide_String;

--| The type `Description_String` represents a description string.

--\
--------------------------------------------------------------------------------------------
--/ Program Arguments

--| The _arguments_ of a program are a sequence of strings used as initials inputs to a 
--| program when it is run. 

   type Program_Argument_String is new Standard.Wide_String;

   type Program_Argument_Access is access constant Program_Argument_String;

   type Program_Argument_Array is array (Positive range <>) of Program_Argument_Access;

--| .....

--\
--------------------------------------------------------------------------------------------
--/ Environment Variables

--| 

   type Environment_Variable_Name_String is new Standard.Wide_String;

   type Environment_Variable_Name_Access
   is
      access constant Environment_Variable_Name_String; 

--| .....

   type Environment_Variable_Value_String is new Standard.Wide_String;

   type Environment_Variable_Value_Access
   is
      access constant Environment_Variable_Value_String; 

--| .....

   type Environment_Variable_Association
   is
      record
         Name:  Environment_Variable_Name_Access;
         Value: Environment_Variable_Value_Access;
      end record;

   type Environment_Variable_Association_Access 
   is 
      access constant Environment_Variable_Association;

--| .....

   type Environment_Variable_Array
   is
      array (Positive range <>) of Environment_Variable_Association_Access;

--| .....

   type Environment_Variable_Set is private;

--| .....

   function To_Set (Vars: in Environment_Variable_Array) return Environment_Variable_Array;

   function To_Array (Set: in Environment_Variable_Set) return Environment_Variable_Set;

--| .....

   Empty_Environment: constant Environment_Variable_Association_Access := null;

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
--/

--| 

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

   Security_Violation: exception;
   
--| .....

--\
--------------------------------------------------------------------------------------------



end;

