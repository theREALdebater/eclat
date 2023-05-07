-----
--/ Package specification AdaOS.Interop:

with AdaOS.RPC;

use AdaOS.RPC;

package AdaOS.Interop is

   pragma Remote_Types;

   --/ :

   type Dynamic_Execution_Controller is synchronized interface;

   --| A dynamic execution controller, or 'dynec', maintains a register of dynamic interfaces
and
   for each interface a register of its methods.

   type Interface_Lock_ID is mod 2**32;

   --| ...

   type Interface_Locking_Mode is (Interface_Usage, Interface_Modification);

   function Name (Dynec: in Dynamic_Execution_Controller;
                  iface: in Interface_Lock_ID) return Wide_String is abstract;

   function Mode (Dynec: in Dynamic_Execution_Controller;
                  iface: in Interface_Lock_ID) return Interface_Locking_Mode is abstract;

   --| ...

   procedure Lock_Interface (Dynec: in out Dynamic_Execution_Controller;
                             Name:  in     Wide_String;
                             Mode:  in     Interface_Locking_Mode := Interface_Usage;
                             iface: out    Interface_Lock_ID) is abstract;

   --| ...

   procedure Relock_Interface (Dynec: in out Dynamic_Execution_Controller;
                               iface: in     Interface_Lock_ID;
                               Mode:  in     Interface_Locking_Mode := Interface_Usage)
                                                                                   is abstract;

   --| ...

   procedure Unlock_Interface (Dynec: in out Dynamic_Execution_Controller;
                               iface: in     Interface_Lock_ID) is abstract;

   --| ...

   procedure Delete_Interface (Dynec: in out Dynamic_Execution_Controller;
                               iface: in     Interface_Lock_ID) is abstract;

   --| ...



   type Method_ID is mod 2**16;

   type Parameter_ID is mod 2**16;

   --| ...

   function Name (Dynec:  in Dynamic_Execution_Controller;
                  iface:  in Interface_Lock_ID;
                  Method: in Method_ID) return Wide_String is abstract;

   --| ...

   procedure Add_Method (Dynec:  in out Dynamic_Execution_Controller;
                         iface:  in     Interface_Lock_ID;
                         Name:   in     Wide_String;
                         Method: out    Method_ID) is abstract;

   --| ...

   procedure Delete_Method (Dynec:  in out Dynamic_Execution_Controller;
                            iface:  in     Interface_Lock_ID;
                            Method: in     Method_ID) is abstract;

   procedure





   type Dynamic_Object is synchronized interface;

   --| ...

   function Implements (Object: in Dynamic_Object;
                        iface:  in Interface_Lock_ID) return Boolean is abstract;

   --| ...

   procedure Invoke_Method (Object: in     Dynamic_Object;
                            iface:  in     Interface_Lock_ID;
                            Method: in     Method_ID;
                            Params: access Params_Stream_Type;
                            Result: access Params_Stream_Type) is abstract;

   --| ...








