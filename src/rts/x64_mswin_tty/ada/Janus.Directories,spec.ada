-----
--/ AdaOS.Programs.Janus.Directories package specification:

with AdaOS.Objects.Persistence;

package AdaOS.Programs.Janus.Directories is

   type File_Directory is limited tagged
      new AdaOS.Objects.Persistence.Object_Container and .....;






































   type Member_Information is private;

   function Name            (Info: in Member_Information) return Wide_String;
   function Object_Type     (Info: in Member_Information) return Wide_String;
   function Processor_Chain (Info: in Member_Information) return Wide_String;
   function Storage_Mode    (Info: in Member_Information) return Wide_String;
   function Access_Control  (Info: in Member_Information) return Wide_String;

   type Attribute_Lock_ID is private;



   function Attribute_Value (Info:     in Member_Information;
                             Atribute: in Attribute_Lock_ID) return Wide_String;

   function Info (Directory:   in File_Directory;
                  Member_Name: in Wide_String)
                                            return access Member_Information'Class is abstract;

   type Name_List       is array (Positive range <>) of Unbounded_Wide_String;
   type Info_List       is array (Positive range <>) of access Member_Information'Class;
   type Controller_List is array (Positive range <>) of access Object_Controller'Class;

   function All_Names       (Directory: in File_Directory) return Name_List is abstract;
   function All_Info        (Directory: in File_Directory) return Info_List is abstract;
   function All_Controllers (Directory: in File_Directory) return File_List is abstract;

   procedure Iterate_Names (Directory: in File_Directory;
                            Process:   in not null access
                                                 procedure (Name: in Wide_String)) is abstract;

   procedure Iterate_Info (Directory: in File_Directory;
                           Process:   in not null access
                                procedure (Info: access Member_Information'Class)) is abstract;

   procedure Iterate_Controllers (Directory: in File_Directory;
                                  Process:   in not null access
                                      procedure (Info: access File_Controller'Class)) is
abstract;

   function Contains (Directory: in File_Directory;
                      Name:      in Wide_String) return Boolean is abstract;

   function Member (Directory: in File_Directory;
                    Name:      in Wide_String) return access File_Controller'Class is abstract;

   procedure Create_Member (Directory:  in File_Directory;
                            Name:       in Wide_String;
                            Controller: out access File_Controller'Class) is abstract;

   procedure Delete_Member (Directory:  in File_Directory;
                            Name:       in Wide_String) is abstract;

   procedure Delete_Member (Directory:  in     File_Directory;
                            Controller: in out File_Controller'Class) is abstract;





