-- ...

with Ada.Wide_Text_IO, Ada.Wide_Text_IO_Exceptions;
with Ada.Wide_Strings.Wide_Unbounded_Strings;
private with Ada.Finalization;

use Ada.Wide_Strings.Wide_Unbounded_Strings;


package Tenet.Config is

   type Configuration_File is limited private;

   function Is_Empty (File: in Configuration_File) return Boolean;

   function Name (File: in Configuration_File) return Wide_String;
   function Form (File: in Configuration_File) return Wide_String;

   procedure Load (File: in out Configuration_File;
                   Name: in     Wide_String := "";
                   Form: in     Wide_String := "");

   --| ..... If "" is specified for the Name, the configuration file is loaded from a default
   pathname.

   procedure Set_Save_Location (File: in out Configuration_File;
                                Name: in     Wide_String;
                                Form: in     Wide_String := "");

   procedure Mandatory_Save  (File: in out Configuration_File);
   procedure Save_If_Changed (File: in out Configuration_File);

   procedure Reset (File: in out Configuration_File);



   type Boolean_Configuration_Parameter (File: access Configuration_File;
                                         Name: access String) is limited private;

   type Integer_Configuration_Parameter (File: access Configuration_File;
                                         Name: access String) is limited private;

   type Float_Configuration_Parameter   (File: access Configuration_File;
                                         Name: access String) is limited private;

   type String_Configuration_Parameter  (File: access Configuration_File;
                                         Name: access String) is limited private;

   type Date_Configuration_Parameter    (File: access Configuration_File;
                                         Name: access String) is limited private;

   -- etc.

   function Value (Parameter: in Boolean_Configuration_Parameter) return Boolean;
   function Value (Parameter: in Integer_Configuration_Parameter) return Integer;
   function Value (Parameter: in Float_Configuration_Parameter)   return Float;
   function Value (Parameter: in String_Configuration_Parameter)  return Wide_String;
   function Value (Parameter: in Date_Configuration_Parameter)    return ???;
   -- etc.

   function Value (Parameter: in Boolean_Configuration_Parameter;
                   Default:   in Boolean) return Boolean;

   function Value (Parameter: in Integer_Configuration_Parameter;
                   Default:   in Integer) return Integer;

   function Value (Parameter: in Float_Configuration_Parameter;
                   Default:   in Float) return Float;

   function Value (Parameter: in String_Configuration_Parameter;
                   Default:   in Wide_String) return Wide_String;

   function Value (Parameter: in Date_Configuration_Parameter;
                   Default:   in ???) return ???;

   -- etc.

   procedure Set_Value (Parameter: in Boolean_Configuration_Parameter;
                        Value:     in Boolean);

   procedure Set_Value (Parameter: in Integer_Configuration_Parameter;
                        Value:     in Integer);

   procedure Set_Value (Parameter: in Float_Configuration_Parameter;
                        Value:     in Float);

   procedure Set_Value (Parameter: in String_Configuration_Parameter;
                        Value:     in Wide_String);

   procedure Set_Value (Parameter: in Date_Configuration_Parameter;
                        Value:     in ???);

   -- etc.


   Name_Error: exception renames Ada.Wide_Text_IO_Exceptions.Name_Error;





private

   type Configuration_File is new Ada.Finalization.Limited_Controlled with
      record
         ...
      end record;





   overriding
   procedure Finalize (File: in out Configuration_File) is
   begin
      Save_If_Changed(File);
   end;


















--    generic
--       type Generic_Type is private; -- serializable
--
--       function Input ()
--
--    package Configuration_IO is
--
--       function Load (Name: in Wide_String;
--                      Mode: in Wide_String := "") return Generic_Type;
--
--       ...
--
--       procedure Save (Name:  in Wide_String;
--                       Value: in Generic_Type;
--                       Mode:  in Wide_String := "");
--

-- Name syntax is
--    u::i where u is EURL of a configuration file and i is pathnme of a configuration item OR
--    i in which case the EURL defaults to "$CONFIG/default.config"

-- i syntax is series of names separated by '.' e.g. "Sender.Name"





--    type Configuration_Control_File is limited private;

