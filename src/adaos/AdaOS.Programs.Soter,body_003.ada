with Tenet.Config;
--with Ada.Wide_Strings.Wide_Unbounded_Strings;
with Ada.Directories;

--use Ada.Wide_Strings.Wide_Unbounded_Strings;

procedure AdaOS.Programs.Soter is

   Config: Tenet.Config.Configuration_File;

   Backup_Object_Name_Config_Name:  constant Wide_String := "backup_directory";

   Backup_Object_Name_Config_Param: String_Configuration_Parameter(Config'Access, Backup_Object_Name_Config_Name'Access);












begin

   Config.Load;

   declare
      Backup_Object_Name: constant Wide_String := Backup_Object_Name_Config_Param.Value("~/backup");
      Manifest_Name: constant Wide_String := Ada.Directories.Compose(Backup_Object_Name, "manifest");
      ...





























      Set_Value(Backup_Object_Name_Config_Param, Backup_Object_Name);

   end;

   Config.Save_If_Changed;

end AdaOS.Programs.Soter;

