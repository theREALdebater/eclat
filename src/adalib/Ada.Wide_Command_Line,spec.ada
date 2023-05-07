with AdaOS.Execution;

package Ada.Wide_Command_Line is
   pragma Preelaborate(Command_Line);
   
   function Argument_Count return Natural;
   function Argument (Number: in Positive) return Wide_String;
   function Command_Name return Wide_String;
   
   subtype Exit_Status is AdaOS.Execution.Process_Exit_Code;
   
   Success: constant Exit_Status;
   Failure: constant Exit_Status;
   
   procedure Set_Exit_Status (Code: in Exit_Status);

end Ada.Command_Line;
