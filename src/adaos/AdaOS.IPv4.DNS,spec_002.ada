

package AdaOS.IPv4.DNS is

   pragma Remote_Types;



   --/ Name server:

   type Name_Server is interface and Objects.System_Object;



   function Name (Resolver: in Name_Server) return Wide_String is abstract;



   function Resolve (Resolver: access Name_Server;
                     Name:     in     Wide_String) return Node_Address is abstract;

   function Resolve (Resolver: access Name_Server;
                     Name:     in     Wide_String) return Wide_String is abstract;




