with Tenet.Config;
with Ada.Wide_Strings.Wide_Unbounded_Strings;

use Ada.Wide_Strings.Wide_Unbounded_Strings;

procedure AdaOS.Programs.Soter is

   Config: Tenet.Config.Configuration_File;
   Backup_Object_Name: Unbounded_String;
   Manifest_Name: Unbounded_String;











begin

   --/ Load configuration:
   Config.Load;
   Backup_Object_Name := Config.Input("backup_directory","~/backup");
   Manifest_Name := Backup_Object_Name & "/manifest";
   ...
   --\

   --/





























   --/ Save configuration:
   Config.Save;
   --\

end AdaOS.Programs.Soter;

