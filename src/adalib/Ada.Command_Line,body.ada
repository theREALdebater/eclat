with System.Execution.Self;
with Ada.Characters.Conversions;

use Ada.Characters.Conversions;

package body Ada.Command_Line is

   function Argument_Count return Natural is
   begin
      return System.Execution.Self.Argument_Count;
   end;

   function Argument (Number: in Positive) return String is
   begin
      return To_Character( System.Execution.Self.Argument(Number) );
   end;

   function Command_Name return String is
   begin
      return To_Character( System.Execution.Self.Program.Name );
   end;

   procedure Set_Exit_Status (Code: in Exit_Status) is
   begin
      return System.Execution.Self.Set_Exit_Code(Code);
   end;

end Ada.Command_Line;

