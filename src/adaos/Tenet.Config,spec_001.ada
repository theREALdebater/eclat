-- ...

with Ada.Wide_Text_IO, Ada.Wide_Text_IO_Exceptions;

package Tenet.Config is

   type Configuration_File is limited private;

   function Is_Empty (File: in Configuration_File) return Boolean;

   function Name (File: in Configuration_File) return Wide_String;
   function Form (File: in Configuration_File) return Wide_String;

   procedure Load (File: in out Configuration_File;
                   Name: in     Wide_String;
                   Form: in     Wide_String := "");

   procedure Save (File: in out Configuration_File;
                   Name: in     Wide_String;
                   Form: in     Wide_String := "");

   procedure Set_Save_Location (File: in out Configuration_File;
                                Name: in     Wide_String;
                                Form: in     Wide_String := "");

   procedure Save (File: in out Configuration_File);

   type Numeric_Type is private;

   function Numeric (Value: in

   type Config_ is new ;

   type Config_ is new ;

   type Config_ is new ;

   type Config_ is new ;

   type Config_ is new ;

   function Read (File:    in Configuration_File;
                  Name:    in Wide_String;
                  Default: in Config_Integer) return Config_Integer;





   Name_Error: exception renames Ada.Wide_Text_IO_Exceptions.Name_Error;





   generic
      type Generic_Type is private; -- serializable

      function Input ()

   package Configuration_IO is

      function Load (Name: in Wide_String;
                     Mode: in Wide_String := "") return Generic_Type;

      ...

      procedure Save (Name:  in Wide_String;
                      Value: in Generic_Type;
                      Mode:  in Wide_String := "");


-- Name syntax is
--    u::i where u is EURL of a configuration file and i is pathnme of a configuration item OR
--    i in which case the EURL defaults to "$CONFIG/default.config"

-- i syntax is series of names separated by '.' e.g. "Sender.Name"





   type Configuration_Control_File is limited private;

