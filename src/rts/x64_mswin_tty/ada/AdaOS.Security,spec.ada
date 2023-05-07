-----
--/ AdaOS.Security package specification:

with Ada.Calendar;

package AdaOS.Security is

   --------------------------------------------------------------------------------------------
   --/ Authority_ID type:

   type Authority_ID is range 0 .. 2**32-1;

   Top_Authority: constant Authority_ID := 0;

   --\

   function Subauthority_Limit (Authority: in Authority_ID) return Natural;

   function New_Subauthority (Authority: in Authority_ID) return Authority_ID;

   procedure Delete_Subauthority (Subauthority: in Authority_ID);

   type Authority_Set is private;

   ... set operations

   function Subauthorities (Authority: in Authority_ID) return Authority_Set;

   function Covers (Left, Right: in Authority_ID) return Boolean;

   function Covers (Left: in Authority_ID; Right: in Authority_Set) return Boolean;

   function Covers (Left: in Authority_Set; Right: in Authority_Id) return Boolean;

   function Covers (Left, Right: in Authority_Set) return Boolean;



   ------
   --/ Object guardians:

   type Object_Guardian is synchronised interface and Persistent_Object;

   type Object_Guardian_Access is access all Object_Guardian'Class;

   --| .....

   type Action_Lock_ID is private;

   type Usage_Lock_ID is private;

   --| .....

   procedure Lock_Action (Guardian: in out Object_Guardian;
                          Name:     in     Wide_String;
                          Action:   out    Action_Lock_ID) is abstract;

   procedure Unlock_Action (Guardian: in out Object_Guardian;
                            Action:   in     Permission_Lock_ID) is abstract;

   function Name (Guardian: in out Object_Guardian;
                  Action:   in     Action_Lock_ID) return Wide_String is abstract;

   procedure Lock_Usage (Guardian: in out Object_Guardian;
                         Name:     in     Wide_String;
                         Usage:    out    Usage_Lock_ID) is abstract;

   procedure Unlock_Usage (Guardian: in out Object_Guardian;
                           Usage:    in     Usage_Lock_ID) is abstract;

   function Name (Guardian: in out Object_Guardian;
                  Usage:    in     Usage_Lock_ID) return Wide_String is abstract;

   --| .....

   --| .....

   type Usage_Quantity is range 0..2**63-1;

???   type Usage_Float   is new Long_Float range 0.0 .. Long_Float'Last;

   --| .....

   function Most_Recent_Change (Guardian: in Object_Guardian) return Ada.Calendar.Time;

   function Permitted (Guardian:  in Object_Guardian;
                       Principal: in Authority_ID;
                       Object:    in Object_Link;
                       Action:    in Action_Lock_ID) return Boolean is abstract;

   procedure Allocate_Usage (Guardian:  in out Object_Guardian;
                             Principal: in     Authority_ID;
                             Object:    in     Object_Link;
                             Usage:     in     Usage_Lock_ID;
                             Request:   in     Usage_Quantity;
                             Grant:     out    Usage_Quantity) is abstract;

???   procedure Allocate_Usage (Guardian:  in out Object_Guardian;
???                             Principal: in     Authority_ID;
???                             Object:    in     Object_Link;
???                             Usage:     in     Usage_Lock_ID;
???                             Request:   in     Usage_Float;
???                             Grant:     out    Usage_Float) is abstract;

   --| .....

   procedure Grant_Permission (Guardian:  in out Object_Guardian;
                               Principal: in     Authority_ID;
                               Object:    in     Object_Link;
                               Action:    in     Action_Lock_ID) is abstract;

   procedure Revoke_Permission (Guardian:  in out Object_Guardian;
                               Principal: in     Authority_ID;
                               Object:    in     Object_Link;
                               Action:    in     Action_Lock_ID) is abstract;

   --\

   --| Iteration:

   procedure Iterate_Actions .....

   procedure Iterate_Usages .....

   procedure Iterate_Permissions .....


   --\









