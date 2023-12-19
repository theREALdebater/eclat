

package AdaOS.IPv4.DNS is

   pragma Remote_Types;





   type Name_Server is interface and Objects.System_Object;



   function Name (Resolver: in Name_Server) return Wide_String;



   function Get_Address (Resolver: in Name_Server;
                         Name: in Wide_String




