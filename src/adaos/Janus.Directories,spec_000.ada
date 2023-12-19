-----
--/

package AdaOS.

   type Directory_Controller is interface and Object_Controller;

   type Member_Information is interface;

   function Name            (Info: in Member_Information) return Wide_String is abstract;
   function Object_Type     (Info: in Member_Information) return Wide_String is abstract;
   function Processor_Chain (Info: in Member_Information) return Wide_String is abstract;
   function Storage_Mode    (Info: in Member_Information) return Wide_String is abstract;
   function Access_Control  (Info: in Member_Information) return Wide_String is abstract;

   function Info (Directory:   in Directory_Controller;
                  Member_Name: in Wide_String)
                                            return access Member_Information'Class is abstract;

   type Name_List       is array (Positive range <>) of Unbounded_Wide_String;
   type Info_List       is array (Positive range <>) of access Member_Information'Class;
   type Controller_List is array (Positive range <>) of access Object_Controller'Class;

   function All_Names       (Directory: in Directory_Controller) return Name_List is abstract;
   function All_Info        (Directory: in Directory_Controller) return Info_List is abstract;
   function All_Controllers (Directory: in Directory_Controller) return File_List is abstract;

   procedure Iterate_Names (Directory: in Directory_Controller;
                            Process:   in not null access
                                                 procedure (Name: in Wide_String)) is abstract;

   procedure Iterate_Info (Directory: in Directory_Controller;
                           Process:   in not null access
                                procedure (Info: access Member_Information'Class)) is abstract;

   procedure Iterate_Controllers (Directory: in Directory_Controller;
                                  Process:   in not null access
                                      procedure (Info: access File_Controller'Class)) is
abstract;

   function Contains (Directory: in Directory_Controller;
                      Name:      in Wide_String) return Boolean is abstract;

   function Member (Directory: in Directory_Controller;
                    Name:      in Wide_String) return access File_Controller'Class is abstract;

   procedure Create_Member (Directory:  in Directory_Controller;
                            Name:       in Wide_String;
                            Controller: out access File_Controller'Class) is abstract;

   procedure Delete_Member (Directory:  in Directory_Controller;
                            Name:       in Wide_String) is abstract;

   procedure Delete_Member (Directory:  in     Directory_Controller;
                            Controller: in out File_Controller'Class) is abstract;





