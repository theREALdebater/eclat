-----
--/ AdaOS.Security package specification:


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

   type Permission_Lock_ID is private;
   
   type Usage_Lock_ID is private;

   --| .....

   procedure Lock_Permission (Guardian:   in out Object_Guardian;
                              Name:       in     Wide_String;
                              Permission: out    Permission_Lock_ID) is abstract;
   
   procedure Unlock_Permission (Guardian:   in out Object_Guardian;
                                Permission: in     Permission_Lock_ID) is abstract;

   function Name (Guardian:   in out Object_Guardian;
                  Permission: in     Permission_Lock_ID) return Wide_String is abstract;
                                
   procedure Lock_Usage (Guardian: in out Object_Guardian;
                         Name:     in     Wide_String;
                         Usage:    out    Usage_Lock_ID) is abstract;
   
   procedure Unlock_Usage (Guardian: in out Object_Guardian;
                           Usage:    in     Usage_Lock_ID) is abstract;

   function Name (Guardian: in out Object_Guardian;
                  Usage:    in     Usage_Lock_ID) return Wide_String is abstract;
                                
   --| .....

   --| .....
   
   type Usage_Integer is range 0..2**63-1;
   
   type Usage_Float   is new Long_Float range 0.0 .. Long_Float'Last;
   
   --| .....

   function Permitted (Guardian:   in out Object_Guardian;
                       Principal:  in     Authority_ID;
                       Object:     in     Object_Link;
                       Permission: in     Permission_Lock_ID) return Boolean is abstract;

   procedure Allocate_Usage (Guardian:  in out Object_Guardian;
                             Principal: in     Authority_ID;
                             Object:    in     Object_Link;
                             Usage:     in     Usage_Lock_ID;
                             Request:   in     Usage_Integer;
                             Grant:     out    Usage_Integer) is abstract;

   procedure Allocate_Usage (Guardian:  in out Object_Guardian;
                             Principal: in     Authority_ID;
                             Object:    in     Object_Link;
                             Usage:     in     Usage_Lock_ID;
                             Request:   in     Usage_Float;
                             Grant:     out    Usage_Float) is abstract;

   --| .....

   procedure Add_Permission (Guardian:   in out Object_Guardian;
                       Principal:  in     Authority_ID;
                       Object:     in     Object_Link;
                       Permission: in     Permission_Lock_ID) is abstract;

   procedure Add_Permission (Guardian:   in out Object_Guardian;
                       Principal:  in     Authority_ID;
                       Object:     in     Object_Link;
                       Permission: in     Permission_Lock_ID;
                             Group:      in     ?????Security.Authorisation_Set) is abstract;

   --\

   
   
   
   
   
   
   

