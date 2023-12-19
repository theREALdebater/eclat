

package AdaOS.Scheduling is



   type Argument_Array is array (Positive range <>) of not null access constant Wide_String;
   
   Null_Argument_Array: constant Argument_Array := (1..0 => null);

   type Scheduling_Descriptor is
      record
         Name:             Wide_String;
         Timepoint:        Ada.Calendar.Time;
         Command_Name:     Wide_String;
         Arguments:        Argument_Array := Null_Argument_Array;
         Standard_Files:   Standard_Files_Descriptor := All_Logged;
         Enviroment:       ... := Inherited_Environment;
         ...
      end record;
   
   
   --| A scheduling descriptor provides all the information necessary to schedule the execution of a program (once).









