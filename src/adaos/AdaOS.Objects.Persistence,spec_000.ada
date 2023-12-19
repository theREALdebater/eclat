-----
--/ AdaOS.Objects.Persistence package specification:

with Ada.Streams;

package AdaOS.Objects.Persistence is

   pragma Remote_Types;

   --/ Persistent_Object type:

   type Persistent_Object is synchronized interface and System_Object;

   --| A persistent object is an object whose 'state' is capable of being 'saved' by
being written into a standard stream, so that the object can be restored to exactly the
same behaviour as it had just before being saved purely from the data that was written into the
stream. The stream can be stored on a long-term medium (such as a magnetic disk), enabling the
object to 'persist' (e.g. surviving the system being shut down or rebooted).

   type Persistent_Object_Access is access all Persistent_Object'Class;
   
   --\
   --/ Saving state, activation, and deactivation:
   
   --| A persistent
object is either 'active' or 'inactive'. It cannot be used for its intended purpose when it is
inactive. A persistent object may or may not be initially inactive when it is created.

   function Is_Active (Object: in out Persistent_Object) return Boolean is abstract;

   --| The Is_Active function returns True if the given persistent Object is active, or False otherwise. 

   procedure Save (Object: in out Persistent_Object;
                   To:     in out Ada.Streams.Root_Stream_Type'Class) is abstract;

   --| The Save procedure writes the current state of Object into the stream To. If Object is
not active, Status_Error is propagated.

   procedure Save_If_Changed (Object:  in out Persistent_Object;
                              To:      in out Ada.Streams.Root_Stream_Type'Class;
                              Changed: out    Boolean) is abstract;

   --| The Save_If_Changed procedure saves the state of Object into the stream To only if
Object's state has changed since the most recent previous execution of
Activate, Save, Save_If_Changed or Deallocate. An object O's state at time t1 has not changed,
compared to time t2, if
Save(O,S) at t2 writes the same data sequence into stream S as Save(O,S) at time t1. Propagates Status_Error if Object is not active.

   procedure Activate (Object: in out Persistent_Object;
                       From:   in out Ada.Streams.Root_Stream_Type'Class) is abstract;

   --| The Activate procedure reads data from the stream From, and uses the data to restore
the state of Object. If the Object is active, Status_Error is propagated, otherwise Object
becomes active. If the data cannot be interpreted as a valid state of Object, Data_Error is
propagated (and Object remains inactive).

   --| The legality or meaning of the same state (data) applying to objects of different
types is undefined.

   procedure Deactivate (Object: in out Persistent_Object;
                         To:     in out Ada.Streams.Root_Stream_Type'Class) is abstract;

   --| The Deactivate procedure save the state of Object into the stream To as if
Save(Object,To) were called, and then changes Object to inactive.

--| Deactivate(O,S) followed, with no intervening operations on object O or
stream S, by Activate(O,S) must never have any effect on O (providing no exception is raised
by either call). The behaviour of a persistent object must depend only on its state: any
operation on object O1 must have the same effect as the same operation (with the same input
parameter values) on an object O2 if O1 and O2 are of the same type and have the same state.

   --\
   --/ Exceptions:

   Status_Error: exception;
   Data_Error: exception;

   --| ...

   --\

end;

--\