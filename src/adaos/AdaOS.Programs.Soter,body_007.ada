with Tenet.Config;
--with Ada.Wide_Strings.Wide_Unbounded_Strings;
with Ada.Directories;
with AdaOS.Objects.Persistence;

--use Ada.Wide_Strings.Wide_Unbounded_Strings;

procedure AdaOS.Programs.Soter is

   Config: Tenet.Config.Configuration_File;

   Backup_Object_Name_Config_Name:  constant Wide_String := "backup_directory";

   Backup_Object_Name_Config_Param: String_Configuration_Parameter(Config'Access, Backup_Object_Name_Config_Name'Access, "~/backup");



   protected Backup_Directory is new AdaOS.Objects.Persistence.Object_Container with









begin

   Config.Load;

   declare
      Backup_Object_Name: constant Wide_String := Backup_Object_Name_Config_Param.Value("~/backup");
      Manifest_Name: constant Wide_String := Ada.Directories.Compose(Backup_Object_Name, "manifest");
      ...



   begin
      ...




























      Set_Value(Backup_Object_Name_Config_Param, Backup_Object_Name);

   end;

   Config.Save_If_Changed; -- not really necessary

end AdaOS.Programs.Soter;








--    declare
--       Backup_Object_Name: constant Wide_String := Backup_Object_Name_Config_Param.Value("~/backup");
--       Manifest_Name: constant Wide_String := Ada.Directories.Compose(Backup_Object_Name, "manifest");
--       ...
--
--
--
--    begin
--       ...
