--/ AdaOS.Execution package specification:

with Ada.Calendar;
with AdaOS.Objects, AdaOS.Remote_Streams, AdaOS.Security;

use AdaOS.Objects;

package AdaOS.Startpoints
is
   pragma Remote_Types;

-----------------------------------------------------------------------------------------------
--/ Startpoints:

   type Program_Assembly is abstract limited new System_Object with private;

   type Assembly_Access is access all Program_Assembly'Class;

--| A _program assembly_ is roughly equivalent to a tradtional executable (file or program). 

--\
-----------------------------------------------------------------------------------------------
--/ 

--| 

   function Name (Startpoint: in Program_Assembly) return Wide_String is abstract;

--| The function `Name` returns the full name of a program startpoint, which should (as far as is
--| practicable) uniquely identify the startpoint (to a human). It should include the startpoint's
--| version (but not its revision). this name is not intended to be the file name of the 
--| ?????.

   ????? function Form (Startpoint: in Program_Assembly) return Wide_String is abstract;

   ????? function UMID (Startpoint: in Program_Assembly) return Wide_String is abstract;

   function Description (Startpoint: in Program_Assembly) return Wide_String is abstract;
   
--| ?????




--| 



--\
-----------------------------------------------------------------------------------------------
--/ Body Procedure

--| The _body procedure_ of a startpoint is a procedure that comprises:
--| 
--|  * the initialisation logic for the startpoint (if any); followed by 
--| 
--|  * the main procedure of the startpoint (if any).
--| 
--| The initialisation logic for a startpoint comprises all the initialisation logic for each of 
--| the program units that the startpoint contains, in an order that is consistent with the 
--| depndencies between them. 

   type Assembly_Body_Procedure is access procedure;

--| The type `Assembly_Body_Procedure` represents (a remote access value referencing) a body 
--| procedure. 

   function Body_Procedure (Startpoint: in Program_Assembly) 
   return 
      Assembly_Body_Procedure is abstract;

--| The function `Body_Procedure` returns (a remote access value referencing) the body 
--| procedure of a given startpoint. 

--\
-----------------------------------------------------------------------------------------------
--/ Startpoint Program

--| 

   function Program (Startpoint: in Program_Assembly) return Program_Access is asbtract;

--| 



--\
-----------------------------------------------------------------------------------------------
--/ 

--| 


--\
-----------------------------------------------------------------------------------------------
--/ 

--| 



--\
-----------------------------------------------------------------------------------------------
--/ 

--| 



--\
-----------------------------------------------------------------------------------------------
--/ 

--| 



--\

end;

