package Ada.Environment_Variables is
   pragma Preelaborate(Environment_Variables);

   function Value (Name: in String) return String;
   function Exists (Name: in String) return Boolean;
   
   procedure Set (Name: in String; Value: in String);
   procedure Clear (Name: in String);
   procedure Clear;
   procedure Iterate (Process: not null access procedure (Name, Value : in String));

end Ada.Environment_Variables;
