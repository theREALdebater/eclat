package Ada.Wide_Environment_Variables is
   pragma Preelaborate(Wide_Environment_Variables);

   function Value (Name: in Wide_String) return String;
   function Exists (Name: in Wide_String) return Boolean;
   
   procedure Set (Name: in Wide_String; Value: in Wide_String);
   procedure Clear (Name: in Wide_String);
   procedure Clear;
   procedure Iterate (Process: not null access procedure (Name, Value: in Wide_String));

end Ada.Wide_Environment_Variables;
