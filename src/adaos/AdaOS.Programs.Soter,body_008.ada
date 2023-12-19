with Tenet.Config;
--with Ada.Wide_Strings.Wide_Unbounded_Strings;
with Ada.Directories, Ada.Tags;
with AdaOS.Objects.Persistence;

--use Ada.Wide_Strings.Wide_Unbounded_Strings;

procedure AdaOS.Programs.Soter is

   Config: Tenet.Config.Configuration_File;

   Backup_Object_Name_Config_Param: String_Configuration_Parameter :=
      New_String_Configuration_Parameter(Config'Access, "backup_directory", "~/backup");



   protected Backup_Directory is new AdaOS.Objects.Persistence.Backup_Directory with

      function Object_Description (OID: in Object_Identifier) return Wide_String;

      function Object_Status (OID: in Object_Identifier) return Wide_String;

      function Object_Type (OID: in Object_Identifier) return Ada.Tags.Tag;

      function Is_Engaged (OID: in Object_Identifier) return Boolean;

      function Is_Engaged (Token: in Engagement_Token) return Boolean;

      function OID (Token: in Engagement_Token) return Object_Identifier;

      procedure Engage (OID:   in  Object_Identifier;
                        Token: out Engagement_Token);

      procedure Disengage (Token: in  Engagement_Token);

      function Object (Token: in  Engagement_Token) return System_Object_Access;

      procedure Iterate_Members (Process: in not null access
                                            procedure (OID: in Object_Identifier));

      function Contains (OID: in Object_Identifier) return Boolean;

      procedure Delete_Member (OID: in Object_Identifier);

      procedure Delete_Member (Token: in Engagement_Token);

      procedure Delete_All_Members (Container: in Backup_Directory);

      function Is_Link (OID: in Object_Identifier) return Boolean;

      function Link_Description (OID: in Object_Identifier) return Wide_String;

      function Link_Status (OID: in Object_Identifier) return Wide_String;

      function Link_Type (OID: in Object_Identifier) return Ada.Tags.Tag;

      function Link_Is_Engaged (OID: in Object_Identifier) return Boolean;

      function Link_Is_Engaged (Token: in Engagement_Token) return Boolean;

      procedure Engage_Link (Container: in  Backup_Directory;
                             OID:       in  Object_Identifier;
                             Token:     out Engagement_Token);

   private

      .......

   end Backup_Directory;









   protected Backup_Directory body is

      function Object_Description (OID: in Object_Identifier) return Wide_String is
      begin
         .....
      end;

      function Object_Status (OID: in Object_Identifier) return Wide_String is
      begin
         .....
      end;

      function Object_Type (OID: in Object_Identifier) return Ada.Tags.Tag is
      begin
         .....
      end;

      function Is_Engaged (OID: in Object_Identifier) return Boolean is
      begin
         .....
      end;

      function Is_Engaged (Token: in Engagement_Token) return Boolean is
      begin
         .....
      end;

      function OID (Token: in Engagement_Token) return Object_Identifier is
      begin
         .....
      end;

      procedure Engage (OID:   in  Object_Identifier;
                        Token: out Engagement_Token) is
      begin
         .....
      end;

      procedure Disengage (Token: in  Engagement_Token) is
      begin
         .....
      end;

      function Object (Token: in  Engagement_Token) return System_Object_Access is
      begin
         .....
      end;

      procedure Iterate_Members (Process: in not null access
                                            procedure (OID: in Object_Identifier)) is
      begin
         .....
      end;

      function Contains (OID: in Object_Identifier) return Boolean is
      begin
         .....
      end;

      procedure Delete_Member (OID: in Object_Identifier) is
      begin
         raise Invalid_Operation;
      end;

      procedure Delete_Member (Token: in Engagement_Token) is
      begin
         raise Invalid_Operation;
      end;

      procedure Delete_All_Members (Container: in Backup_Directory) is
      begin
         raise Invalid_Operation;
      end;

      function Is_Link (OID: in Object_Identifier) return Boolean is
      begin
         raise Invalid_Operation;
      end;

      function Link_Description (OID: in Object_Identifier) return Wide_String is
      begin
         raise Invalid_Operation;
      end;

      function Link_Status (OID: in Object_Identifier) return Wide_String is
      begin
         raise Invalid_Operation;
      end;

      function Link_Type (OID: in Object_Identifier) return Ada.Tags.Tag is
      begin
         raise Invalid_Operation;
      end;

      function Link_Is_Engaged (OID: in Object_Identifier) return Boolean is
      begin
         raise Invalid_Operation;
      end;

      function Link_Is_Engaged (Token: in Engagement_Token) return Boolean is
      begin
         raise Invalid_Operation;
      end;

      procedure Engage_Link (Container: in  Backup_Directory;
                             OID:       in  Object_Identifier;
                             Token:     out Engagement_Token) is
      begin
         raise Invalid_Operation;
      end;

   end Backup_Directory;












begin

   Config.Load;

   declare
      Backup_Object_Name: constant Wide_String := Backup_Object_Name_Config_Param.Value;
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
