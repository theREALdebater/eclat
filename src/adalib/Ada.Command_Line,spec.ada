with AdaOS.Execution;

package Ada.Command_Line
is
   pragma Preelaborate(Command_Line);
   
   function Argument_Count return Natural;
   function Argument (Number: in Positive) return String;
   function Command_Name return String;
   
   subtype Exit_Status is AdaOS.Execution.Process_Exit_Code;
   
   Success: constant Exit_Status;
   Failure: constant Exit_Status;
   
   procedure Set_Exit_Status (Code: in Exit_Status);

private

   Success: constant Exit_Status := 0;
   Failure: constant Exit_Status := 1;

end Ada.Command_Line;
