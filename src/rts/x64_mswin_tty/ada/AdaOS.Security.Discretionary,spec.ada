




package AdaOS.Security.Discretionary is




   ------------------------------------------------------------
   -- Ringfence-based access control:


   type Ringfence_ID is private;

   -- A 'ringfence' is a special, exclusive, security group. An entity can
   -- belong to a ringfence (be within it), in which case, special
   -- retrictions associated with the ringfence will be applied to the
   -- entity.

   -- Ringfences provide facilities very like those of the Access Control Lists (ACLs) of other operating systems.


   function Name (Ringfence: in Ringfence_ID) return Wide_String;

   -- Each ringfence also has a name, which this function returns.


   procedure Create_Ringfence (Name: in Wide_String) return Ringfence_ID;

   -- Creates (allocates) a new ringfence (ID) to the caller. The owner of
   -- the new ringfence will be the owner of the authority of the calling
   -- program (partition). It's name will be as given.


   procedure Rename (Ringfence: ...);


   procedure Delete (Ringfence: ...);


   type Ringfence_Guardian is
      access procedure (Authority: in  Authority_ID;
                        Ringfence: in  Ringfence_ID;
                        Accepted:  out Boolean);

   -- When a guardian procedure is set for a ringfence, whenever any
   -- program (partition) makes an attempt to access an entity within that
   -- ringfence, the guardian procedure is called first, with the appro-
   -- priate details supplied. Only if Accepted is True is the original
   -- access permitted. For a ringfence which does not have a guardian
   -- procedure set, the access is immediately permitted.


   procedure Set_Guardian (Ringfence: in Ringfence_ID;
                           Guardian:  in Ringfence_Guardian);

   -- Sets the guardian procedure for a ringfence. If Guardian is null, the
   -- ringfence will have no guardian procedure set for it. This is the
   -- initial state for all guardians.

   -- Only the owner of a ringfence is permitted to set (or remove) its
   -- guardian. If this procedure is called by a program (partition) whose
   -- authority's owner is not the owner of the ringfence, the procedure
   -- does nothing but propagate the Security_Error exception.


   function Guardian (Ringfence: in Ringfence_ID)
                                              return Ringfence_Guardian;

   -- Returns the current guardian procedure set for a ringfence, or null
   -- if none is set.


   ----------------------------------------
   -- :

   type Secure_Object is abstract new Public_Object with private;


   ----------------------------------------
   -- Object-based access control:


   type Object_Guardian is
      access procedure (Authority: in     Authority_ID;
                        Object:    access Secure_Object'Class;
                        Accepted:  out    Boolean);

   -- When a guardian procedure is set for a secure object, whenever any
   -- program (partition) makes a call to one of that object's primitive
   -- operations, the guardian procedure is called first, with the appro-
   -- priate details supplied. Only if Accepted is True is the original
   -- call made. For an object which does not have a guardian procedure set,
   -- calls proceed as normal.


   procedure Set_Guardian (Object:   access Secure_Object'Class;
                           Guardian: in     Object_Guardian);

   -- Sets the guardian procedure for a secure object. If Vet is null, the
   -- object will have no guardian procedure set for it. This is the initial
   -- state for any secure object.


   function Guardian (Object: access Secure_Object) return Object_Guardian;

   -- Returns the current guardian procedure set for a secure object, or
   -- null if none is set.



   ----------------------------------------
   -- :

   procedure Set_Ringfence (Object:    access Secure_Object'Class;
                            Ringfence: in     Ringfence_ID);

   function Ringfence (Object: access Secure_Object) return Ringfence_ID;








