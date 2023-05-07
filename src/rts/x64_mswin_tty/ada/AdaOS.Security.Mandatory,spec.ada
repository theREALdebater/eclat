






package AdaOS.Security.Mandatory is


   ------------------------------------------------------------
   -- Sensitivity and Integrity:

   -- Each 'sensitivity level' defines a degree to which information needs
   -- to be kept secret. Each 'integrity level' defines a degree to which
   -- (the source of) information can be relied upon (to be correct). For
   -- both, there is always a 'lowest' level.


   type Security_Level is private;


   function "<"  (Left, Right: in Security_Level) return Boolean;
   function "<=" (Left, Right: in Security_Level) return Boolean;
   function ">"  (Left, Right: in Security_Level) return Boolean;
   function ">=" (Left, Right: in Security_Level) return Boolean;


   function Name (Level: in Security_Level) return Wide_String;

   -- Each sensitivity and Integrity level has a name; these functions
   -- return the appropriate name.


   type Sensitivity_Level is new Security_Level;
   type Integrity_Level is new Security_Level;


   function Default_Sensitivity return Sensitivity_Level;
   function Default_Integrity return Integrity_Level;


   ----------------------------------------
   -- :


   procedure Set_Sensitivity (
         Object:      access Secure_Object'Class;
         Sensitivity: in     AdaOS.Security.Sensitivity_Level );

   procedure Set_Integrity (
         Object:      access Secure_Object'Class;
         Integrity: in     AdaOS.Security.Integrity_Level );


   function Sensitivity (Object: access Secure_Object'Class)
                                   return AdaOS.Security.Sensitivity_Level;

   function Integrity (Object: access Secure_Object'Class)
                                   return AdaOS.Security.Integrity_Level;


   ----------------------------------------
   -- :

   procedure Set_Sensitivity (Authority: in Authority_ID; Sensitivity: in Sensitivity_Level);
   procedure Set_Integrity   (Authority: in Authority_ID; Integrity:   in Integrity_Level);

   function Sensitivity (Authority: in Authority_ID) return Sensitivity_Level;
   function Integrity   (Authority: in Authority_ID) return Integrity_Level;



