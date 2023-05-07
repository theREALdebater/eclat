-----------------------------------------------------------------------------------------------
--/ AdaOS.Containers package specification

with AdaOS.Objects;

use AdaOS.Objects;

package AdaOS.Objects
with
   Remote_Types
is
-----------------------------------------------------------------------------------------------
--/ Containers

--| A system object can, in implementational terms, be deleted (whereby its existence ends) 
--| after its state is saved, and then created again and have that state restored, in such a 
--| way that the object's existence remains apparently unbroken and continuous. When a system 
--| object is saved and deleted, it is said to be _inactive_. When it is created and its state 
--| restored again, it is said to be _active_.

--| An _object container_ is a system object which conceptually contains a certain set of 
--| system objects. That set is termed the _membership_ of the container, and the objects it 
--| contains are its _members_. 

   type Object_Container is abstract limited new System_Object with private;

   type Container_Access is access all Object_Container'Class;

--| The type `Container_Access` designates all object contained types (all types derived from 
--| `Object_Container`). Since this package is declared as a remote types package, 
--| `Container_Access` is a remote access type. 

--\
-----------------------------------------------------------------------------------------------
--/ Container of an object

   function Container (Object: access System_Object) return Container_Access is abstract;

--| The function `Container` returns a remote access value which references the container of a 
--| given system object, or null if it has no container. 

--| Every system object is a member of at most one container. Only a few very special system 
--| objects, the _root objects_ of the system, have no container. 






--\


























-----------------------------------------------------------------------------------------------
--/ Type of an object

   function Object_Type (Container: access Object_Container;
                         OID:       in Object_Id) 
      return 
         Ada.Tags.Tag is abstract;

--| The function `Object_Type` returns the tag of the (usually most derived type) that the 
--| object has when it is when it is engaged. This function is intended to be used by programs 
--| as a test prior engaging the object. This function may return the value `No_Tag`, for 
--| example if the type of the object is unknown. 

--| If `OID` refers to an object link, this function applies to the target of the link. 

--\
-----------------------------------------------------------------------------------------------
--/ Description of an object

   function Object_Description (Container: access Object_Container;
                                OID:       in Object_Id) 
      return 
         Wide_String is abstract; 

--| The function `Object_Description` returns a textual description of a system object. The 
--| result should assume no other knowledge of the object, and should identify the object as 
--| much as reasonably possible. The result of this function will not normally change over 
--| time. This function should return text that is suitable for a programmer to use for 
--| debugging and system diagnostic purposes (not for end users or normal system uses). 

--| Naming containers (of a type derived from `Naming_Container`) maintain names as an 
--| alternate means of identifying members within the container. The function 
--| `Object_Description` is not intended to compete with or replicate such names. 
   
--\
-----------------------------------------------------------------------------------------------
--/ Engagement and disengagement:

   procedure Engage (Object: not null access System_Object) is abstract;

--| The procedure `Engage` engages the system object `Object`. 

--| A system object can be engaged only once at any one time. An attempt to engage an already engaged 
--| object blocks the caller until it becomes disengaged. 

   function Is_Engaged (Container: access Object_Container;
                        OID:       in Object_Id) return Boolean is abstract;

--| The function `Is_Engaged` returns `True` if object identified by the given OID is currently 
--| engaged, or `False` otherwise. 

--| This function could be subject to race conditions. It should not be used by  programs as a 
--| way to wait until an object can be engaged. Instead, programs should use an ATC form of 
--| call of the `Engage` procedure. 









   procedure Disengage (Object: not null access System_Object) is abstract;

--| The procedure `Disengage` disengages the given system object. 





???
   function Object (Container: access Object_Container;
                    Token:     in Engagement_Token) return System_Object_Access is abstract;

   --| The Object function returns a (remote) access value that designates the engaged
   --| system object identified by the given Token. If the token value is invalid,
   --| Status_Error is propagated. If the object is shelved when this function is called, it is
   --| unshelved (so as to create an in-memory object to be referenced).







--\
-----------------------------------------------------------------------------------------------
--/ Membership functions:

   function Is_Empty (Container: access Object_Container) return Boolean is abstract;
   
--| The function `Is_Empty` returns `True` only if a given container is empty (has no members). 

   function Member_Count (Container: access Object_Container) return Object_Count is abstract;
   
--| The function `Member_Count` returns the number of members a given container has. 

   function Members (Container: access Object_Container) return Object_Array is abstract;
   
   function Members (Container: access Object_Container;
                     Filter:    not null access function (OID: in Object_Id) return Boolean) 
      return 
         Object_Array is abstract;
   
--| The function `Members` returns an array of the object identifiers of the members of a given 
--| container. This array only includes the OIDs the objects which are directly members of the 
--| container, not ay objects which are members of a member etc. 

--| The overloading without a `Filter` parameter returns an array of the OIDs of all members of 
--| the container. 

--| The overloading with a `Filter` parameter executes the function `Filter` for each member 
--| and returns an array containing the OID of only those members for which the `Filter` 
--| function returns `True`. Any exception raised by the `Filter` function is propagated. 

   procedure Iterate_Members (Container: access Object_Container;
                              Process:   not null access
                                                    procedure (OID: in Object_Id)) is abstract;

--| The procedure `Iterate_Members` executes the given `Process` once for each member of a 
--| given container. Any exception raised by the `Process` procedure is propagated. 

   function Contains (Container: access Object_Container;
                      OID:       in Object_Id) return Boolean is abstract;

--| The function `Contains` returns `True` only if a given object identifier is that of a 
--| system object which is a member of a given container. 

   --\
   --------------------------------------------------------------------------------------------
   --/ Deletion of members:

   procedure Delete_Member (Container: access Object_Container;
                            OID:       in Object_Id) is abstract;
                            
--| The procedure `Delete_Member` deletes the system object with the given object identifier;  
--| the object ceases to exist. Any actions taken as a result of its deletion are dependent on 
--| the object's container. If the container does not contain an object with the given 
--| identifer, the call to `Delete_Member` does nothing (there is no error). After a call 
--| `Delete_Member (C, OID)` then `Contains (C, OID)` will always return `False`; the OID will 
--| never be re-used by the same container. 

   procedure Delete_All_Members (Container: access Object_Container) is abstract;
   
--| The procedure `Delete_All_Members` deletes all the members of a given container. After a 
--| call to this procedure, the container will be empty. 

--\
-----------------------------------------------------------------------------------------------
--/ Object Links:

   type Object_Link is synchronized interface and System_Object;

--| An _object link_ is a system object which holds enough information to permanently identify 
--| some other system object. The link is said to _point_ to the other object, which is called 
--| the _target_ of the link. 

--| All operations of an object link, except for the operations in this fragment 
--| (`Is_Link`, `Link_Description`, `Link_Status`, `Link_Type`, `Link_Is_Engaged`, 
--| `Engage_Link`, and `Target`), operate on the target of the link rather than on the link 
--| itself. 

   function Is_Link (Container: access Object_Container;
                     OID:       in Object_Id) return Boolean is abstract;

--| The function `Is_Link` returns `True` only if a system object with a given object identifer 
--| is an object link. This function is a convenient substitute for: 
--| 
--|     Ada.Tags.Is_Descendant_At_Same_Level (
--|        Object_Type (Container, OID), 
--|        AdaOS.Objects.Object_Link)

   function Link_Description (Container: access Object_Container;
                              OID:       in Object_Id) return Wide_String is abstract;

--| The function `Link_Description` does what `Object_Description` would if it were to operate 
--| directly on the link. If `OID` does not identify an object link, the exception 
--| `Status_Error` is propagated. 

   function Link_Status (Container: access Object_Container;
                         OID:       in Object_Id) return Wide_String is abstract;

--| The function `Link_Status` does what `Object_Status` would if it were to operate directly 
--| on the link. If `OID` does not identify an object link, the exception `Status_Error` is 
--| propagated. 

   function Link_Type (Container: access Object_Container;
                       OID:       in Object_Id) return Ada.Tags.Tag is abstract;

--| The function `Link_Type` does what `Object_Type` would if it were to operate directly on 
--| the link. If `OID` does not identify an object link, the exception `Status_Error` is 
--| propagated. 

   function Link_Is_Engaged (Container: access Object_Container;
                             OID:       in Object_Id) return Boolean is abstract;

--| The function `Link_Is_Engaged` does what `Is_Engaged` would if it were to operate directly 
--| on the link. If `OID` does not identify an object link, the exception `Status_Error` is 
--| propagated. 

   procedure Engage_Link (Container: access Object_Container;
                          OID:       in Object_Id;
                          Object:    out Object_Access) is abstract;

--| The procedure `Engage_Link` does what `Engage` would if it were to operate directly on the 
--| link. If `OID` does not identify an object link, the exception `Status_Error` is 
--| propagated. 

   function Target_Chain (Link: access Object_Link) return Object_Array is abstract;

   --| The function `Target_Chain` returns an array that identifies all the links that lead up 
   --| to the (ultimate) target of an object link. The last object identifier in the array is 
   --| that of the target of the link; the identifier immediately preceding it (if any) is that 
   --| of the link, L1, whose immediate target is the (ultimate) target; the identifier 
   --| immediately preceding that (if any) is of the link whose immediate target is L1, and so 
   --| on. The first OID of the array is the immediate target of the given link. 

   function Target (Link: access Object_Link) return Object_Id is abstract;

   --| The function `Target` returns the identifier of the object which is the (ultimate) 
   --| target of a given link. 

--\
-----------------------------------------------------------------------------------------------
--/ Copying a system object

--| A _copyable object_ is a system object that can be _deep copied_. 
--| 
--| In theory, if `Y` is a deep copy of an object `X`, then modifying any part (however deeply 
--| nested) of `X` will not change the corresponding part of `Y`, and vice versa. In practice, 
--| the deep copying of an object may not strictly obey this definition, either because it 
--| would not be practicable or because it wouldn't make sense (it would go against the likely 
--| expectations of people maintaining the software). 

   type Copyable_Object is synchronized interface and System_Object;

--| The interface type `Copyable_Object` represents copyable objects. 

   function Copy (Object: access Copyable_Object) return Copyable_Object is abstract; 

--| The function `Copy` makes a deep copy of a copyable object, and returns a reference to the 
--| copy. 

--\
-----------------------------------------------------------------------------------------------
--/ Deleting a system object

--| A _deletable object_ is a system object that can be made to delete itself (so it ceases to 
--| exist, meaning that afterwards nothing can interact with it and it does not use up any 
--| resources). 

   type Deletable_Object is synchronized interface and System_Object;

--| The interface type `Deletable_Object` represents deletable objects. 

   procedure Delete (Object: access Deletable_Object) is abstract; 

--| The procedure `Delete` deletes a deletable object. 

   --\
   --------------------------------------------------------------------------------------------
   --/ Exceptions:

   Identifier_Error: exception;

   --| ...

   Mode_Error: exception;

   --| ...

   Data_Error: exception;

   --| ...

   --\

private

   --------------------------------------------------------------------------------------------
   --/ Private part:

   type Object_Id range 0 .. 2**64-1; 
   
   --| .....

   --\

end;

--\
-----------------------------------------------------------------------------------------------
-- End of file



























--    --/ Saving state, activation, and deactivation:
--
--    --| A system object is either 'active' or 'inactive'. It cannot be used for its intended
--    --| purpose when it is inactive. A system object may or may not be initially inactive
--    --| when it is created.
--
--    function Is_Active (Object: in out System_Object) return Boolean is abstract;
--
--    --| The Is_Active function returns True if the given persistent Object is active, or False
--    --| otherwise.
--
--    procedure Save (Object: in out System_Object;
--                    To:     in out Ada.Streams.Root_Stream_Type'Class) is abstract;
--
--    --| The Save procedure writes the current state of Object into the stream To. If Object is
--    --| not active, Status_Error is propagated.
--
--    procedure Save_If_Changed (Object:  in out System_Object;
--                               To:      in out Ada.Streams.Root_Stream_Type'Class;
--                               Changed: out    Boolean) is abstract;
--
--    --| The Save_If_Changed procedure saves the state of Object into the stream To only if
--    --| Object's state has changed since the most recent previous execution of Activate, Save,
--    --| Save_If_Changed or Deallocate. An object O's state at time t1 has not changed, compared
--    --| to time t2, if Save(O,S) at t2 writes the same data sequence into stream S as Save(O,S)
--    --| at time t1. Propagates Status_Error if Object is not active.
--
--    procedure Activate (Object: in out System_Object;
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
--    procedure Deactivate (Object: in out System_Object;
--                          To:     in out Ada.Streams.Root_Stream_Type'Class) is abstract;
--
--    --| The Deactivate procedure saves the state of Object into the stream To as if
--    --| Save(Object,To) were called, and then changes Object to inactive.
--
--    --| Deactivate(O,S) followed, with no intervening operations on object O or stream S, by
--    --| Activate(O,S) must never have any effect on O (providing no exception is raised by
--    --| either call). The behaviour of a system object must depend only on its state: any
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












--    type Class_Set is private;
--
--    procedure Iterate (Set:     in Class_Set;
--                       Process: not null access procedure (ID: in Clas_Identifier));
--
--    function Is_In (Set: in Class_Set; ID: in Class_Identifier) return Boolean;
--
--    --| A class set is a set of class identifiers. The Iterate procedure iterates over the
--    --| members of a class set. The Is_In function returns true if a given identifier is a
--    --| member of a class set.
--
-- This will
--    include the name of every class
--    --| that a class is derived from (but no name will be repeated). In other words, the set of
--    --| every T'Class_Name for every type T for which "X in T'Class" returns True, where X is
--    --| the object engaged by the given OID.








   -- type Engagement_Token is private;

-- --| An engagement token is a small opaque value passed out by an object container when one of 
-- --| its members is engaged, and used to identify the engaged object to the container when 
-- --| calling other operations (including disengaging the object). 

   -- function Is_Engaged (Container: in Object_Container;
                        -- OID:       in Object_Id) return Boolean is abstract;

   -- function Is_Engaged (Container: in Object_Container;
                        -- Token:     in Engagement_Token) return Boolean is abstract;

   -- --| Both the Is_Engaged functions return True if object identified by the given OID or Token
   -- --| is currently engaged, or False otherwise.
   
   
   
-- --| An _emphemeral object_ is a system object that is never disengaged. It is only accessed 
-- --| through a token. When it is created, it is created already engaged, and immediately ceases 
-- --| to exist when it is disengaged. An emphemeral object never has an OID. 



   -- function Has_OID (Container: in Object_Container;
                     -- Token:     in Engagement_Token) return Boolean is abstract;
   
-- --| The function `Has_OID` returns `True` only if the object identified by the given `Token` 
-- --| is not emphemeral.    
   
   

   -- function OID (Container: in Object_Container;
                 -- Token:     in Engagement_Token) return Object_Id is abstract;

   -- --| The OID function .....

   -- procedure Engage (Container: in  Object_Container;
                     -- OID:       in  Object_Id;
                     -- Token:     out Engagement_Token) is abstract;

   -- --| The Engage procedure engages the member system object identified by the given OID,
   -- --| and passes out, in the parameter Token, an engagement token.

-- --| A member can be engaged only once at any one time. An attempt to engage an already engaged 
-- --| object blocks the caller until it becomes disengaged. 

-- --| Since this procedure is a primitive 
-- --| operation of a synchronised type (`Object_Container`), the caller can use an ATC to limit 
-- --| the amount of time it may be blocked. 

   -- procedure Disengage (Container: in  Object_Container;
                        -- Token:     in  Engagement_Token) is abstract;

   -- --| The Disengage procedure disengages the given Token. The member system object is
   -- --| disengaged only when there are no remaining engaged tokens.

   -- --| ????? It is not guaranteed that using a token value after it has been disengaged will
   -- --| propagate an exception (the effect of doing so is undefined).

   -- function Object (Container: in  Object_Container;
                    -- Token:     in  Engagement_Token) return System_Object_Access is abstract;

   -- --| The Object function returns a (remote) access value that designates the engaged
   -- --| system object identified by the given Token. If the token value is invalid,
   -- --| Status_Error is propagated. If the object is shelved when this function is called, it is
   -- --| unshelved (so as to create an in-memory object to be referenced).



























-----------------------------------------------------------------------------------------------
--/ OID consolidation

--| In theory, OIDs are eternal: no OID will ever be reallocated to a new system object in a domain, 
--| regardless of 
--| how long the object it originally designated has ceased to exist. 

--| However, containers must provide a mechanism that .....



--| ..... can only be done in system maintenance mode ......






   type Identifier_Consolidation_Map is private;
   
--| The private type 'Identifier_Consolidation_Map` represents an object that contains the 
--| mappings of a set of objects. For each object, its old identifier, called the _original_, 
--| is associated with its new identifier, called its _target_. 







   type Object_Count is range 0 .. 2**63 - 17; -- implementation-defined upper bound
   
--| .....

   subtype Object_Index is Object_Count range 1.. Object_Count'Last;
   
--| .....

   type Object_Array is array (Object_Index range <>) of Object_Id;
   
--| .....





   function Has (Map: in Identifier_Consolidation_Map; 
                 OID:     in Object_Id) return Boolean; 

--| The function `Has` returns `True` only if the `Map` has the given `OID` as an original. 

   function Target (Map: in Identifier_Consolidation_Map; 
                    OID:     in Object_Id) return Object_Id; 

--| The function `Target` returns the target of a given original `OID`. The function returns 
--| `No_Object` if the `Map` does not have the given `OID` as an original. 

   procedure Consolidate_Identifiers (Container: in out Object_Container;
                                      Mapping:   out    Identifier_Consolidation_Map);

--| The procedure `Consolidate_Identifiers` causes the members of a container to have their 
--| identifiers consolidated. The mapping from the previous OIDs to the new ones is passed out 
--| in the parameter `Mapping`.

--\










