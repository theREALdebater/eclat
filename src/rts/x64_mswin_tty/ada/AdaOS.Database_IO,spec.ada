--/ Database_IO package specification:

package Database_IO is

   pragma Remote_Types;

   type Count is ...; -- integer type
   subtype Positive_Count is Count range 1..Count'Last;

   type Database_Table is interface; -- really a cursor

   type Remote_Table is access all Database_Table'Class; -- remote access type

   procedure Open (Table: in out Remote_Table;
                   Name:  in     Wide_String;
                   Form:  in     Wide_String := "") is abstract; -- opens a cursor

   procedure Close (Table: in out Remote_Table);

   function Columns (Table: in Remote_Table) return Count is abstract;

   function Column_Name (Table: in Remote_Table;
                         Index: in Positive_Count) return Wide_String is abstract;

   function Rows (Table: in Remote_Table) return Count is abstract;




   type Repositionable_Table is interface and Database_Table;

   type Remote_Repositionable is access all Repositionable_Table'Class; -- remote access type

   procedure Open_Repostionable (Table: in out Remote_Table;
                                 Name:  in     Wide_String;
                                 Form:  in     Wide_String := "") is abstract;

   function Position (Table: in Remote_Repositionable) return Count is abstract;
   -- returns 0 iff table empty

   procedure Reposition (Table:    in Remote_Repositionable;
                         Position: in Positive_Count) is abstract;






package Database_IO.Modification is

   type Modifiable_Table is interface and Repositionable_Table;

   procedure Open_Modifiable (Table: in out Remote_Table;
                              Name:  in     Wide_String;
                              Form:  in     Wide_String := "") is abstract; -- opens cursor R/W

   procedure Insert_Row (Table: in Remote_Table;
                         ???) is abstract

   procedure Set_Row (Table: in Remote_Table;
                         ???) is abstract

   procedure Delete_Row (Table: in Remote_Table;
                         ???) is abstract




package Database_IO.Restructuring is

   type Restructurable_Table is interface and Modifiable_Table;

   procedure Open_Restructuring (Table: in out Remote_Table;
                                 Name:  in     Wide_String;
                                 Form:  in     Wide_String := "") is abstract;

   procedure Delete_Column (Table: in out Restructurable_Table;
                            Index: in     Positive_Count) is abstract;

   procedure Insert_Column (Table: in out Restructurable_Table;
                            Name:  in     Wide_String) is abstract;

   procedure Rename_Column (Table: in out Restructurable_Table;
                            Index: in     Positive_Count;
                            Name:  in     Wide_String) is abstract;



