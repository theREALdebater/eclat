-----------------------------------------------------------------------------------------------
--/ AdaOS.Objects.Persistence package specification:

with Ada.Streams;

package AdaOS.Objects.Persistence is

   pragma Remote_Types;

   --/ Persistent objects:

--   type Persistent_Object is synchronized interface and System_Object;

   --| A persistent object is an object whose 'state' is capable of being 'saved' by being
   --| written into a standard stream, so that the object can be restored to exactly the same
   --| behaviour as it had just before being saved purely from the data that was written into
   --| the stream. The stream can be stored on a long-term medium (such as a magnetic disk),
   --| enabling the object to 'persist' (e.g. surviving the system being shut down or
   --| rebooted).

--   type Persistent_Object_Access is access all Persistent_Object'Class;

   --| A persistent object is not formally differentiated from a system object in the Ada code.

   --\
   --/ Object_Container type:

   type Object_Container is synchronized interface and System_Object;

   type Object_Container_Access is access all Object_Container'Class;

   --| A persistent object can, in implementational terms, be deleted (whereby its existence
   --| ends) after its state is saved, and then created again and have that state restored, in
   --| such a way that the object's existence remains apparently unbroken and continuous. When
   --| a persistent object is saved and deleted, it is said to be 'shelved'. When it is created
   --| and its state restored again, it is said to be 'unshelved'.

   --| An object container is a system object which conceptually contains a certain set of
   --| persistent objects. That set is termed the 'membership' of the container, and the
   --| objects it contains are its 'members'.

   --| The container is capable of shelving and unshelving any of its members. A persistence
   --| controller will normally use the Activate procedure when unshelving a persistent object,
   --| and the Deactivate procedure when shelving it.

   type Object_Identifier is private;

   --| An object identifer, or OID, is a small (e.g. 64-bit) value which uniquely and
   --| permanently identifies every persistent object within the membership of an object
   --| container. No two OIDs ever identify the same object (within the membership). It is by
   --| its OID that a persistent object can continue to be identified between being shelved and
   --| unshelved.

   function Object_Description (Container: in Object_Container;
                                OID:       in Object_Identifier)
                                                             return Wide_String is abstract;

   function Object_Status (Container: in Object_Container;
                           OID:       in Object_Identifier) return Wide_String is abstract;

   --| The Object_Description function returns a textual description of the persistent object
   --| identified by the given OID. The result should assume no other knowledge of the object,
   --| and should identify the object as much as reasonably possible. The result of this
   --| function will not normally change over time.

   --| The Object_Status function returns a textual description of the current state of the
   --| persistent object identified by the given OID. The result should be broad and general,
   --| and not be too detailed or verbose. The result of this function will change as the
   --| object's state changes.

   --| Both these functions might return the same text that the functions of the same name
   --| return for the objects themselves (as system objects), but are not required to. In
   --| particular, they may reflect the fact that the object is specifically persistent, and
   --| add information as to its being shelved and active.

   --\
   --/ Classes of a member:

   subtype Class_Identifier is String;

   --| A class identifier is a string which contains the fully qualified name of the base type
   --| of an Ada class.

   type Class_Set is private;

   procedure Iterate (Set:     in Class_Set;
                      Process: not null access procedure (ID: in Clas_Identifier));

   function Is_In (Set: in Class_Set; ID: in Class_Identifier) return Boolean;

   --| A class set is a set of class identifiers. The Iterate procedure iterates over the
   --| members of a class set. The Is_In function returns true if a given identifier is a
   --| member of a class set.

   function Object_Classes (Container: in Object_Container;
                            OID:       in Object_Identifier) return Class_Set is abstract;

   --| The Object_Classes function returns the set of Ada classes that the object identified by
   --| the given OID will belong to (when engaged). This will include the name of every class
   --| that a class is derived from (but no name will be repeated). In other words, the set of
   --| every T'Class_Name for every type T for which "X in T'Class" returns True, where X is
   --| the object engaged by the given OID.

   --\
   --/ Engagement and disengagement:

   type Engagement_Token is private;

   --| An engagement token is a small opaque value passed out by an object container when one
   --| of its members is engaged, and used to identify the engaged object to the container when
   --| calling other operations (including disengaging the object).

   function Is_Engaged (Container: in Object_Container;
                        OID:       in Object_Identifier) return Boolean is abstract;

   function Is_Engaged (Container: in Object_Container;
                        Token:     in Engagement_Token) return Boolean is abstract;

   --| Both the Is_Engaged functions return True if object identified by the given OID or Token
   --| is currently engaged, or False otherwise.

   procedure Engage (Container: in  Object_Container;
                     OID:       in  Object_Identifier;
                     Token:     out Engagement_Token) is abstract;

   --| The Engage procedure engages the member persistent object identified by the given OID,
   --| if it is not already engaged, and passes out, in the parameter Token, an engagement
   --| token. If the object is not active, it is activated. Moreover, if is shelved it is
   --| unshelved.

   procedure Disengage (Container: in  Object_Container;
                        Token:     in  Engagement_Token) is abstract;

   --| The Disengage procedure disengages the member persistent object identified by the given
   --| Token.

   --| It is not guaranteed that using a token value after it has been disengaged will
   --| propagate an exception (the effect of doing so is undefined without limitation).

   function Object (Container: in  Object_Container;
                    Token:      in  Engagement_Token) return System_Object_Access is abstract;

   --| The Object function returns a (remote) access value that designates the engaged
   --| persistent object identified by the given Token. If the token value is invalid,
   --| Status_Error is propagated.

   --\
   --/ Membership functions:

   type Object_List is array (Positive range <>) of Object_Identifier;

   function All_Members (Container: in Object_Container) return Object_List is abstract;

   procedure Iterate_Members (Container: in Object_Container;
                              Process:   in not null access
                                            procedure (OID: in Object_Identifier)) is abstract;

   function Contains (Container: in Object_Container;
                      OID:       in Object_Identifier) return Boolean is abstract;

   --\
   --/ Deletion of members:

   procedure Delete_Member (Container: in Object_Container;
                            OID:       in Object_Identifier) is abstract;

   procedure Delete_Member (Container: in Object_Container;
                            Token:     in Engagement_Token) is abstract;

   --| ..... disengages the object and then deletes it, without the possibility of it being
   --| engaged in between.

   procedure Delete_All_Members (Container: in Object_Container) is abstract;

   --\
   --/ Object Links:

   type Object_Link is synchronized interface;

   --| An object link is an object which holds enough information to permanently identify some
   --| other object. The link is said to 'point' to the other object, which is called the
   --| 'target' of the link.

   function Is_Link (Container: in Object_Container;
                     OID:       in Object_Identifier) return Boolean is abstract;

   --| The function Is_Link returns True is the OID designates an object link, and False
   --| otherwise. This function is a convenient alternative to
   --| Is_In(Container.Classes(OID),"Object_Link"). It may also be more efficient.

   function Link_Description (Container: in Object_Container;
                              OID:       in Object_Identifier)
                                                             return Wide_String is abstract;

   function Link_Status (Container: in Object_Container;
                         OID:       in Object_Identifier) return Wide_String is abstract;

   function Link_Is_Engaged (Container: in Object_Container;
                             OID:       in Object_Identifier) return Boolean is abstract;

   function Link_Is_Engaged (Container: in Object_Container;
                             Token:     in Engagement_Token) return Boolean is abstract;

   procedure Engage_Link (Container: in  Object_Container;
                          OID:       in  Object_Identifier;
                          Token:     out Engagement_Token) is abstract;

   --| For an OID which identifies an object link, the subprograms Object_Description,
   --| Object_Status, Object_Classes, Is_Engaged, and Engage all operate on the target of the
   --| link, not on the link itself. The subprograms Link_Description, Link_Status,
   --| Link_Is_Engaged, and Engage_Link provide the same operations respectively on the link
   --| object itself. If the OID used for any of these subprograms does not identify an object
   --| link, Status_Error is propagated.

   type Link_Target_Descriptor (Chain_Length: Natural) is
      record
         Base:   Object_Container_Access;
         Chain:  array (Positive range 1..Chain_Length) of Object_Identifier;
         -- 1..Chain_Length-1 are containers, Chain(Chain_Length) is target object's OID
         -- if Chain_Length=0, Base references target object
      end record;

   function Target (Link: in Object_Link) return Link_Target_Descriptor is abstract;

   --| ..... target of a link .....

   --\
   --/ Exceptions:

   Identifier_Error: exception;

   --| ...

   Status_Error: exception;

   --| ...

   Data_Error: exception;

   --| ...

   --\

private

   --/ Private part:

   type Object_Identifier range 0..2**64-1;

   type Engagement_Token is range 0..2**32-1;

   --\

end;













--    --/ Saving state, activation, and deactivation:
--
--    --| A persistent object is either 'active' or 'inactive'. It cannot be used for its intended
--    --| purpose when it is inactive. A persistent object may or may not be initially inactive
--    --| when it is created.
--
--    function Is_Active (Object: in out Persistent_Object) return Boolean is abstract;
--
--    --| The Is_Active function returns True if the given persistent Object is active, or False
--    --| otherwise.
--
--    procedure Save (Object: in out Persistent_Object;
--                    To:     in out Ada.Streams.Root_Stream_Type'Class) is abstract;
--
--    --| The Save procedure writes the current state of Object into the stream To. If Object is
--    --| not active, Status_Error is propagated.
--
--    procedure Save_If_Changed (Object:  in out Persistent_Object;
--                               To:      in out Ada.Streams.Root_Stream_Type'Class;
--                               Changed: out    Boolean) is abstract;
--
--    --| The Save_If_Changed procedure saves the state of Object into the stream To only if
--    --| Object's state has changed since the most recent previous execution of Activate, Save,
--    --| Save_If_Changed or Deallocate. An object O's state at time t1 has not changed, compared
--    --| to time t2, if Save(O,S) at t2 writes the same data sequence into stream S as Save(O,S)
--    --| at time t1. Propagates Status_Error if Object is not active.
--
--    procedure Activate (Object: in out Persistent_Object;
--                        From:   in out Ada.Streams.Root_Stream_Type'Class) is abstract;
--
--    --| The Activate procedure reads data from the stream From, and uses the data to restore the
--    --| state of Object. If the Object is active, Status_Error is propagated, otherwise Object
--    --| becomes active. If the data cannot be interpreted as a valid state of Object, Data_Error
--    --| is propagated (and Object remains inactive).
--
--    --| The legality or meaning of the same state (data) applying to objects of different types
--    --| is undefined.
--
--    procedure Deactivate (Object: in out Persistent_Object;
--                          To:     in out Ada.Streams.Root_Stream_Type'Class) is abstract;
--
--    --| The Deactivate procedure saves the state of Object into the stream To as if
--    --| Save(Object,To) were called, and then changes Object to inactive.
--
--    --| Deactivate(O,S) followed, with no intervening operations on object O or stream S, by
--    --| Activate(O,S) must never have any effect on O (providing no exception is raised by
--    --| either call). The behaviour of a persistent object must depend only on its state: any
--    --| operation on object O1 must have the same effect as the same operation (with the same
--    --| input parameter values) on an object O2 if O1 and O2 are of the same type and have the
--    --| same state.
--
--    --\







--    type Link_Target_Descriptor_List (Length: Natural) is private;
--
--    function Link (List:  in Link_Target_Descriptor_List;
--                   Index: in Positive) return Link_Target_Descriptor;
--
--    --| ..... Link_Target_Descriptor_List .....
--
--    function Links (Container: in Object_Container;
--                    Token: in Engagement_Token) return Link_Target_Descriptor_List is abstract;
--
--    --| The Links function returns, for a certain object X, the complete list of links for which
--    --| X is the target. This function is always reasonably efficient (quick).

