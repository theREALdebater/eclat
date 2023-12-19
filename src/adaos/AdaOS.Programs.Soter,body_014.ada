with Tenet.Config;
--with Ada.Wide_Strings.Wide_Unbounded_Strings;
with Ada.Directories, Ada.Tags, Ada.Streams.Stream_IO;
with AdaOS.Objects.Persistence;

--use Ada.Wide_Strings.Wide_Unbounded_Strings;

procedure AdaOS.Programs.Soter is

   Config: Tenet.Config.Configuration_File;

   Manifest_Name_Config_Param: constant String_Configuration_Parameter :=
      New_String_Configuration_Parameter( Config'Access, "Manifest_Name", "~/backup/manifest" );

   Manifest_Form_Config_Param: constant String_Configuration_Parameter :=
      New_String_Configuration_Parameter( Config'Access, "Manifest_Form", "" );







   type Manifest_Member_Descriptor is
      record
         Name: ???

         .....
      end record;




   protected Backup_Manifest is
      new Ada.Finalization.Limited_Controlled
      and AdaOS.Objects.Persistence.Object_Container with

      function Object_Description (OID: in Object_Identifier) return Wide_String;

      function Object_Status (OID: in Object_Identifier) return Wide_String;

      function Object_Type (OID: in Object_Identifier) return Ada.Tags.Tag;

      function Is_Engaged (OID: in Object_Identifier) return Boolean;

      function Is_Engaged (Token: in Engagement_Token) return Boolean;

      function OID (Token: in Engagement_Token) return Object_Identifier;

      procedure Engage (OID:   in  Object_Identifier;
                        Token: out Engagement_Token);

      procedure Disengage (Token: in Engagement_Token);

      function Object (Token: in Engagement_Token) return System_Object_Access;

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

      function Hash (OID: in Object_Identifier) return Ada.Containers.Hash_Type;

      type Manifest_Member is new Ada.Objects.System_Object with
         record
            Actual: Ada.Objects.Persistence.Link_Descriptor;
            .....
         end record;

      package Manifest_Storage is new Ada.Containers.Hashed_Maps(Object_Identifier, Manifest_Member, Hash, "=");

      Store: Manifest_Storage.Map;

      Guardian: AdaOS.Security.Object_Guardian_Access;

      Use_Action: AdaOS.Security.Action_Lock_ID;

      .......

      procedure Initialize (Manifest: in out Backup_Manifest);
      procedure Finalize   (Manifest: in out Backup_Manifest);

   end Backup_Manifest;









   protected Backup_Manifest body is

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

      procedure Disengage (Token: in Engagement_Token) is
      begin
         .....
      end;

      function Object (Token: in Engagement_Token) return System_Object_Access is
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
         .....
      end;

      procedure Delete_Member (Token: in Engagement_Token) is
      begin
         .....
      end;

      procedure Delete_All_Members (Container: in Backup_Directory) is
      begin
         .....
      end;

      function Is_Link (OID: in Object_Identifier) return Boolean is
      begin
         return True; -- every member of this directory is a link
      end;

      function Link_Description (OID: in Object_Identifier) return Wide_String is
      begin
         .....
      end;

      function Link_Status (OID: in Object_Identifier) return Wide_String is
      begin
         .....
      end;

      function Link_Type (OID: in Object_Identifier) return Ada.Tags.Tag is
      begin
         .....
      end;

      function Link_Is_Engaged (OID: in Object_Identifier) return Boolean is
      begin
         .....
      end;

      function Link_Is_Engaged (Token: in Engagement_Token) return Boolean is
      begin
         .....
      end;

      procedure Engage_Link (Container: in  Backup_Directory;
                             OID:       in  Object_Identifier;
                             Token:     out Engagement_Token) is
      begin
         .....
      end;

      procedure Initialize (Manifest: in out Backup_Manifest) is
      begin
         --/ Load store from file, if it exists:

         if Ada.Directories.Exists(Manifest_Name_Config_Param.Value) then
            declare
               Store_File: Ada.Streams.Stream_IO.File_Type;
               Member:     Manifest_Member_Descriptor;
            begin
               Open( Store_File, In_File, Manifest_Name_Config_Param.Value, Manifest_Form_Config_Param.Value );
               ..... -- TODO: read header
               while not End_of_File(Store_File) loop
                  Member := Manifest_Member_Descriptor'Input( Stream(Store_File) );
                  Store.Insert( Member );
               end loop;
               Close( Store_File );
            end;

         --\
         --/

         .....

         --\
      end;

      procedure Finalize   (Manifest: in out Backup_Manifest) is
      begin
         .....
      end;

   end Backup_Manifest;












begin

   Config.Load;

   declare
      Backup_Object_Full_Name: constant Wide_String := Backup_Object_Full_Name_Config_Param.Value;
      Manifest_Full_Name: constant Wide_String := Ada.Directories.Compose(Backup_Object_Name, Manifest_Name_Config_Param.Value);
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
