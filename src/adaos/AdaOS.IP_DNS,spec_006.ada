with AdaOS.IPv4, AdaOS.IPv6;

package AdaOS.IP_DNS is

   pragma Remote_Types;

   --/ Name server system object:

   type Name_Server is interface and Objects.System_Object;

   type Name_Server_Access is access Name_Server;

   function Name (Resolver: in Name_Server) return Wide_String is abstract;

   function Resolve (Resolver: in Name_Server_Access;
                     Name:     in Wide_String) return IPv4.Node_Address is abstract;

   function Resolve (Resolver: in Name_Server_Access;
                     Name:     in Wide_String) return IPv6.Node_Address is abstract;

   function Resolve (Resolver: in Name_Server_Access;
                     Name:     in Wide_String) return Wide_String is abstract;

   --| All overloadings of the Resolve function perform a recursive query for the
   given domain name.

   --| The first expects to find an A record (potentially following
   a chain of CNAME records first), returning an IPv4 node address.

   --| The second expects to find a AAAA record (.....)
   returning an IPv6 node address.

   --| The third
   expects to find a PTR or TXT record, returning its payload (as a string).

   --| In all cases,
   if the expectation is not met, or if the given domain name finds no record
   (or if it finds only records that have been repudiated), then Resolution_Error
   is propagated.

   --\
   --/ Exceptions:

   Resolution_Error: exception;



   --\

end;

