-----
--/ AdaOS.Files package specification:

with Ada.Strings.Wide_Unbounded_Strings;
with AdaOS.Objects, AdaOS.Objects.Persistence, AdaOS.Objects.Persistence.Structure;
with AdaOS.Scripting, AdaOS.Integrity, AdaOS.Remote_Streams, AdaOS.Security, AdaOS.Kernel;

use Ada.Strings.Wide_Unbounded_Strings;
use AdaOS.Objects, AdaOS.Objects.Persistence, AdaOS.Objects.Persistence.Structure;
use AdaOS.Remote_Streams;

package AdaOS.Files is

   pragma Remote_Types;

   ------
   --/ File directory type:

   type File_Directory is synchronized interface and Persistent_Object and Object_Container;

   type File_Directory_Access is access all File_Directory'Class;

   --| A file directory .....

   --\
   -----
   --/ Persistent object activator type:

   type Object_Activator is synchronized interface and Persistent_Object;

   type Object_Activator_Access is access all Object_Activator'Class;

   --| ....

   procedure Activate (Activator: in out Object_Activator;
                       File:      in       Object_Link_Descriptor;
                       Stream:    not null Remote_Stream_Access;
                       Object:    out    Persistent_Object_Access; -- never null (?)
                       Authority: out    Security.Authority_ID) is abstract;

   --| The Activate procedure .....

   --\
   -----
   --/ File-type registry type:

   type File_Type_Registry is synchronized interface and Persistent_Object;

   --| A file-type registry is a persistent object which contains a set of named file types, called
   the registry's 'gamut'. Each file type in the registry's gamut is identified by a unique
name (a string), and can be associated with .....

   type File_Type_Gamut is array (Positive range <>) of access constant Wide_String;

   function Gamut (Registry: in File_Type_Registry) return File_Type_Gamut is abstract;

   type File_Type_Lock_ID is range 0..2**16-1;

   procedure Lock_Type (Registry: in out File_Type_Registry;
                        Name:       in     Wide_String;
                        Lock:     out    File_Type_Lock_ID) is abstract;

   procedure Unlock_Type (Registry: in File_Type_Registry;
                          Lock:     in File_Type_Lock_ID) is abstract;

   procedure Delete_Type (Registry: in File_Type_Registry;
                          Lock:     in File_Type_Lock_ID) is abstract;

   function Activator (Registry: in File_Type_Registry;
                       Lock:     in File_Type_Lock_ID) 
                                                    return Object_Activator_Access is abstract;

   procedure Set_Activator (Registry:  in File_Type_Registry;
                            Lock:      in File_Type_Lock_ID;
                            Activator: in Object_Activator_Access) is abstract;

   --\
   -----
   --/ :

   type Directory_File_List is is array (Positive range <>) of access constant Wide_String;

   function File_List (Directory: in File_Directory) return Directory_File_List is abstract;

   function File_List (Directory: in File_Directory;
                       File_Type: in File_Type_Lock_ID) return Directory_File_List is abstract;

   type Filename_Pattern_Element is
      record
         Char_First, Char_Last: Wide_Character;
         Occurrance_Min, Occurrance_Max: Positive := 1;
      end record;

   type Filename_Pattern_Alternative is array (Positive range <>) of Filename_Pattern_Element;

   type Filename_Pattern is array (Positive range <>) of
                                                  access constant Filename_Pattern_Alternative;

   function File_List (Directory: in File_Directory;
                       Pattern:   in Filename_Pattern) return Directory_File_List is abstract;

   function File_List (Directory: in File_Directory;
                       Pattern:   in Filename_Pattern;
                       File_Type: in File_Type_Lock_ID) return Directory_File_List is abstract;

   --\
   -----
   --/ :

   type File_Lock_ID is mod 2**32;

   --/ Locking:

   type File_Locking_Status is (Unlocked,
                                Read_Only,
                                Full_Access);

   --| A file can be in one of three locking states: unlocked; read-only;
   --| full access. It is initially unlocked.

   subtype File_Locking_Mode is File_Locking_Status range Read_Only..Full_Access;

   --| A file can be locked in one of two locking modes: read-only;
   --| full access.

   ??? function Status (File: in ???) return Locking_Status is abstract;

   ??? --| The Status function returns the current locking state of a system file.

   procedure Lock_File_Fully (Directory:   in out File_Directory;
                        Name:        in     Wide_String;
                        Obtained:    out    Boolean; ??? or exception?
                        File:        out    File_Lock_ID;
                        Transaction: in     Transaction_Controller_Access; -- null if none
                        Timeout:     in     Ada.Calendar.Time;
                        Mode:        in     File_Locking_Mode := Full_Access) is abstract;

   --| The Lock_File procedure attempts to obtain a level of access for the authority
cited by the calling
   --| task to a file.

   --| The level of access can be either: shared, not permitting the file
   --| to be modified (read-only mode); exclusive, without any restrictions on
   --| modification of the object (full access mode).

   --| The required mode is in Mode.

   --| If the file is unlocked at the time of call, the lock is obtained. If it is read-only
and Mode
   --| is Read_Only, the lock is obtained. Otherwise, the calling task is blocked until
   --| either the conditions change to enable the lock to be obtained (according to the
   --| conditions just stated) or the time in Timeout is reached. True is returned in
   --| Obtained (only) if the lock is obtained. If and when the lock is obtained, the
   --| file's locking state is/becomes Mode.

   procedure Lock_File_RO (Directory:   in out File_Directory;
                        Name:        in     Wide_String;
                        Obtained:    out    Boolean; ??? or exception?
                        File:        out    File_Lock_ID;
                        Transaction: in     Transaction_Controller_Access; -- null if none
                        Timeout:     in     Ada.Calendar.Time) is abstract;





   procedure Create_File (Directory: in out File_Directory;
                          Name:      in     Wide_String;
                          File_Type: in     File_Type_Lock_ID;
                          Transaction: in     Transaction_Controller_Access; -- null if none
                          Obtained:  out    Boolean; ??? or exception?
                          File:      out    File_Lock_ID; -- full access
                          OID:       out    Object_Identifier;
                          Timeout:   in     Ada.Calendar.Time) is abstract;

   procedure Create_File (Directory: in out File_Directory;
                          File_Type: in     File_Type_Lock_ID;
                          Transaction: in     Transaction_Controller_Access; -- null if none
                          Obtained:  out    Boolean; ??? or exception?
                          File:      out    File_Lock_ID; -- full access
                          OID:       out    Object_Identifier;
                          Timeout:   in     Ada.Calendar.Time) is abstract;

   --| ...

   --| The Creat_File procedure has two overloadings, one of which has a Name parameter and the other does not. If the Name is specified .....

   procedure Unlock_File (Directory: in out File_Directory;
                          File:      in     File_Lock_ID) is abstract;

   --| The Unlock_File ...

   procedure Delete_File (Directory: in out File_Directory;
                          File:      in     File_Lock_ID) is abstract;

   --| The Delete_File procedure ...


   --\
   --/ :

   procedure Rename_File (Directory: in out File_Directory;
                          File:      in     File_Lock_ID;
                          New_Name:  in     Wide_String) is abstract;

   --| The Rename_File procedure ...

   procedure Rename_File (Directory: in out File_Directory;
                          File:      in     File_Lock_ID) is abstract;

   --| The Rename_File procedure without the New_Name parameter changes the name of the file
   to a randomly chosen name which is guaranteed to be unique within the given directory (and
   therefore cannot fail due to the new name being already in use). ......


   --\
   --/ :

   procedure Copy_File (Directory: in out File_Directory;
                        File:      in     File_Lock_ID;
                        New_Name:  in     Wide_String) is abstract;

   procedure Copy_File (Directory: in out File_Directory;
                        File:      in     File_Lock_ID;
                        Target:    in     File_Directory_Access) is abstract;

   procedure Copy_File (Directory: in out File_Directory;
                        File:      in     File_Lock_ID;
                        Target:    in     File_Directory_Access) is abstract;
                        New_Name:  in     Wide_String) is abstract;

   --| The Copy_File procedures ...

   --\

   procedure Move_File (Directory: in out File_Directory;
                        File:      in     File_Lock_ID;
                        Target:    in     File_Directory_Access) is abstract;

   procedure Move_File (Directory: in out File_Directory;
                        File:      in     File_Lock_ID;
                        Target:    in     File_Directory_Access) is abstract;
                        New_Name:  in     Wide_String) is abstract;

   --| The Move_File procedures ...

   --\
   --/ :

   --\
   -----
   --/ :



   --\
   -----
   --/ :



   --\
   --/ Member file name and property values:

   function File_Name (Directory: in File_Directory;
                       File:      in File_Lock_ID) return Wide_String;

   function Property_Value  (Directory: in File_Directory;
                              File:      in File_Lock_ID;
                              Property: in Property_Lock_ID) return Wide_String;

   --\
   --/ :









   --\
   --/ :

   procedure Iterate_Files (Directory: in File_Directory;
                            Process:   in not null access
                                                procedure (File: in File_Lock_ID)) is abstract;

   --\
   --/ :

   function Contains_File (Directory: in File_Directory;
                           Name:      in Wide_String) return Boolean is abstract;

   --\
   --/ :

   function Create_File (Directory: in File_Directory;
                         Name:      in Wide_String;
                         File_Type: in File_Type_Lock_ID) return File_Lock_ID is abstract;

   procedure Delete_Member (Directory: in File_Directory;
                            Name:      in Wide_String) is abstract;

   procedure Delete_Member (Directory: in     File_Directory;
                            Member:    in out System_Object'Class) is abstract;

   procedure _File (Directory: in out File_Directory;
                          File:      in     File_Lock_ID;
                          ) is abstract;

   --| The _File procedure ...


   --\
   --/ :

   procedure _File (Directory: in out File_Directory;
                          File:      in     File_Lock_ID;
                          ) is abstract;

   --| The _File procedure ...


   --\
   --/ :

   procedure _File (Directory: in out File_Directory;
                          File:      in     File_Lock_ID;
                          ) is abstract;

   --| The _File procedure ...


   --\
   --/ :

   procedure _File (Directory: in out File_Directory;
                          File:      in     File_Lock_ID;
                          ) is abstract;

   --| The _File procedure ...


   --\
   --/ :

   --\
   --/ :

   --\
   --/ :

   --\
   --/ :

   --\
   -----
   --/ :

   type File_Segment_ID is range 0..2**63-1;

   --| .......

   --| The default segment number is 0. This corresponds to the $DATA stream of NTFS and the
data fork of AFS (segment 1 corresponds to the resource fork).

   --\
   --/ :

   function Is_Open (Directory: in out File_Directory;
                     File:      in     File_Lock_ID;
                     Segment:   in     Segment_ID := 0) return Boolean is abstract;

   type Stream_Access_Mode is (In_Stream, Out_Stream, Append_Stream);

   procedure Open_As_Stream (Directory: in out File_Directory;
                             File:      in     File_Lock_ID;
                             Mode:      in     Stream_Access_Mode;
                             Stream:    out    Remote_Stream_Access;
                             Segment:   in     File_Segment_ID := 0) is abstract;

   function Close (Directory: in out File_Directory;
                   File:      in     File_Lock_ID;
                   Segment:   in     File_Segment_ID := 0) is abstract;



   --\
   -----
   --/ :

   -- type Region_Access_Mode is (Execute_Only, Read_Only, Execute_And_Read, Read_And_Write);

   procedure Open_As_Region (Directory: in out File_Directory;
                             File:      in     File_Lock_ID;
                             Region:    out    Kernel.Resource_ID;
                             Mode:      in     Kernel.Region_Access_Mode :=
Kernel.Read_And_Write;
                             Segment:   in     File_Segment_ID := Default_Segment_ID) is
abstract;


   --\
   --/ :

   function Top_Segment (Directory: in File_Directory;
                         File:      in File_Lock_ID) return File_Segment_ID is abstract;
   
   procedure Set_Top_Segment (Directory: in out File_Directory;
                                 File:      in     File_Lock_ID;
                                 Segment:   in     File_Segment_ID) is abstract;

   --| The 'top segment' is the highest numbered segment .....
   
   --| The default top segment of every file is 0. This means that one segment, segment 0 (the default segment) is available.
                                    
   --\
   --/ :

   type Segment_Length_Count is range 0..2**63-1; -- in bits
   
   function Segment_Length (Directory: in File_Directory;
                            File:      in File_Lock_ID;
                            Segment:   in File_Segment_ID) 
                                                       return Segment_Length_Count is abstract;
   
   procedure Set_Segment_Length (Directory: in out File_Directory;
                                 File:      in     File_Lock_ID;
                                 Segment:   in     File_Segment_ID;
                                 Length:    in     Segment_Length_Count) is abstract;
   
   --| Every segment has a specific length, in bits. It is assumed that the significant data stored in the segment comprises that number of bits. Bits are numbered starting from bit 0 of the first stream element of the segment, through to bit 0 of the next stream element, and so on. Non-significant data (after the end of the length) may be preserved or it may not; no reliance should ever be placed on either behaviour.
   
   --| The default length of every segment is 0.
   
   
   


   --\
private

   package Item_Indexing is new Ada.Containers.Hashed_Index(Wide_String,Wide_String);
-- Correct?

   type Member_Information is
      record
         Lookup: Item_Indexing.Index;
      end record;

   function Object_Name (Info: in Member_Information) return Wide_String is
   begin
      return Info.Index.Find("Name");
   end;

   function Count_Items (Info: in Member_Information) return Natural is
   begin
      return Info.Index.Count;
   end;

   function Item_Value  (Info: in Member_Information;
                         Name: in Wide_String) return Wide_String is abstract;

   procedure Iterate_Items (Info:    in Member_Information;
                            Process: not null access procedure (Name:  in Wide_String;
                                                                Value: in Wide_String))
                                                                                   is abstract;






   Locking_Error: exception;

