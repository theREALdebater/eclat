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

