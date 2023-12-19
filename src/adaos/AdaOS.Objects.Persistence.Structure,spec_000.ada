-----------------------------------------------------------------------------------------------
--/ AdaOS.Objects.Persistence.Structure package specification:

package AdaOS.Objects.Persistence.Structure is

   pragma Remote_Types;

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
                    Token:      in  Engagement_Token)
                                                   return Persistent_Object_Access is abstract;
 
   --| The Object function returns a (remote) access value that designates the engaged
   --| persistent object identified by the given Token. If the token value is invalid,
   --| Status_Error is propagated.

   --\
   --/ Membership functions:
 
   type Membership_List is array (Positive range <>) of Object_Identifier;

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
   --/ Links:
 
   type Object_Link is synchronized interface;
 
   --| An object link is an object which holds enough information to permanently identify some
   --| other object. The link is said to 'point' to the other object, which is called the
   --| 'target' of the link.

   type Link_Target_Descriptor (Chain_Length: Natural) is
      record
         Base:   Object_Container_Access;
         Chain:  array (Positive range 1..Chain_Length) of Object_Identifier;
         -- 1..Chain_Length-1 are containers, Chain(Chain_Length) is target object's OID
         -- if Chain_Length=0, Base references target object
      end record;
 
   function Target (Token: in Engagement_Token) return Link_Target_Descriptor is abstract;
 
   --| ..... target of a link .....

   type Link_Target_Descriptor_List (Length: Natural) is private;

   function Link (List:  in Link_Target_Descriptor_List;
                  Index: in Positive) return Link_Target_Descriptor;
 
   --| ..... Link_Target_Descriptor_List .....

   function Links (Token: in Engagement_Token) return Link_Target_Descriptor_List is abstract;
 
   --| The Links function returns, for a certain object X, the complete list of links for which
   --| X is the target. This function is always reasonably efficient (quick).

   --\
   --/ Exceptions:
 
   Identifier_Error: exception;
 
   --\

private

   --/ Private part:

   type Object_Identifier range 0..2**64-1;
 
   type Engagement_Token is range 0..2**32-1;
 
   type Link_Target_Descriptor_List (Length: Natural) is
      record
         Descs: array (Positive range 1..Length) of ???????;
      end record;

   --\
 
end;

