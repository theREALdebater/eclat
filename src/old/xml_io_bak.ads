-----------------------------------------------------------------------------------------------
--/ XML_IO package specification:

with Ada.Wide_Characters.Handling, Ada.Characters.Latin_1, Ada.Strings.Wide_Unbounded;
with Ada.Wide_Text_IO, Ada.IO_Exceptions;

package XML_IO is

   --------------------------------------------------------------------------------------------
   --/ Basic constants:

   Default_Line_Break: constant Wide_String :=
      ( 1 => Ada.Wide_Characters.Handling.To_Wide_Character(Ada.Characters.Latin_1.LF) );

   --\
   --------------------------------------------------------------------------------------------
   --/ String sets:

   type Wide_String_Set is private;

   function Member_Count (Set: in Wide_String_Set) return Natural;

   function Member (Set:    in Wide_String_Set;
                    Number: in Positive) return Wide_String;

   procedure Append (Set:   in out Wide_String_Set;
                     Value: in     Wide_String);

   procedure Clear (Set: in out Wide_String_Set);

   --\
   --------------------------------------------------------------------------------------------
   --/ Structural schema:
   
   type Structural_Schema is private;
   
   --| A structural schema defines ... etc
   
   --| Assignment is by deep copy; the type is controlled.

   Unrestricted_Structure: constant Structural_Schema;
   
   --| The unrestricted structural schema permits any document structure that is legal in XML.

   --\
   --------------------------------------------------------------------------------------------
   --/ Document type:

   type Structured_Document (Structure: access Structural_Schema) is private;
 
   --| A structured document contains (in general) many parts, organised in a strictly
   --| hierarchical tree of nodes.
   
   --| Assignment is by deep copy; the type is controlled.

   -- Null_Document: constant access Structured_Document;

   -- procedure Free (Document: access Structured_Document);

   -- function New_Empty_Document return access Structured_Document;

   --\
   --------------------------------------------------------------------------------------------
   --/ Namespaces:

   type Global_Namespace is private;
 
   --| A global namespace represents a single collection of names that are consistent with one
   --| another in respect of some logical schema. Each namespace is uniquely identified by a
   --| name (which, in practice, tends to be rather long and unwieldy).
   
   --| Token type; default Null_Namespace.
   
   function Name (Namespace: in Global_Namespace) return Wide_String;
   function Namespace (Name: in Wide_String) return Global_Namespace;

   Null_Namespace: constant Global_Namespace; -- name ""
   XML_Namespace:  constant Global_Namespace; -- name "http://www.w3.org/XML/1998/namespace"
 
   --\
   --------------------------------------------------------------------------------------------
   --/ Qualified names:

   type Qualified_Name is
      record
         Local_Part: Unbounded_Wide_String;
         Namespace:  Global_Namespace := Null_Namespace;
      end record;
 
   --| A qualified name is used to identify an element tag or an
   --| attribute within an element. It 
   is associated with a
   --| namespace and a local part. The local part is a string with certain syntactic restrictions.

   -- function Prefix     (Name: in Qualified_Name) return Wide_String;

   -- function To_Name (Local_Part: in Wide_String;
   --                   Namespace:  in Namespace_Ref := Null_Namespace_Ref) return Qualified_Name;

   --\
   --------------------------------------------------------------------------------------------
   --/ Element tags:

   type Element_Tag is limited private;
 
   --| An element tag defines a kind of element that can be a node of a structured document. It
   --| is identified by a qualified name.
   
   --| Token type; 

   -- Null_Structural_Element: constant Structural_Element;

   -- function Document (Element_Type: in Structural_Element) return Document_Ref;
   function Name   (Tag: in Element_Tag) return Qualified_Name;
   function Schema (Tag: in Element_Tag) return access Structural_Schema;

--    function Element_Type (Document: in Document_Ref;
--                           Name:     in Qualified_Name) return Element_Type_Ref; -- null if not found
--
--    generic
--       with procedure Process (Element_Type: in Element_Type_Ref);
--    procedure Iterate_Element_Types (Document: in Document_Ref);
--
   function Tag (Name:   in Qualified_Name;
                 Schema: in Structural_Schema := Unrestricted_Structure) return Element_Tag;

   procedure Rename_Tag (Tag:  in Element_Tag;
                         Name: in Qualified_Name);
                     
--    procedure Append_Element_Type (Document: in  Document_Ref;
--                                   Name:     in  Qualified_Name);

   --\
   --------------------------------------------------------------------------------------------
   --/ Elements:

   type Structural_Element is private;
 
   --| There are several different kinds of node in a structured document. One kind of node is
   --| the element. Only elements can have nodes beneath them in the structure. There is only
   --| one root node in the tree, which must be an element.
   
   --| Controlled type; assignment by deep copy.
   
   function Tag (Element: in Structural_Element) return Element_Tag;
   
   procedure Set_Tag (Element: in out Structural_Element;
                      Tag:     in     Element_Tag);

   function Has_Document (Element: in Structural_Element) return Boolean;
   
   --| A structural element can be either a 'document element', in which case it is part of the 
   --| hierarchy of elements within a document
   --| or it can be a 'non-document element', in which case it is outside any document 
   --| (existing in this state only as a part of the interim 
   --| processing of a document).
   
   --| The function Has_Document returns True if the given Element has a document, and returns 
   --| False if it doesn't.

   function Document (Element: in Structural_Element) return Structured_Document;

   --| The function Document returns the document of the given Element. The exception 
   --| ??? is propagated if the Element does not have a document.
   
                      --    function Document     (Element: in Structural_Element) return Document_Ref;
--    function Element_Type (Element: in Structural_Element) return Element_Type_Ref;

   function Root (Document: in Structured_Document) return Structural_Element;

--    procedure Delete_Root_Element (Document: in Document_Ref);

   procedure Set_Root (Document: in out Structured_Document;
                       Element:  in     Structural_Element);

--    procedure Set_Root_Element (Document:     in  Document_Ref;
--                                Element_Type: in  Element_Type_Ref;
--                                Reference:    out Structural_Element);

   --\
   --------------------------------------------------------------------------------------------
   --/ Namespace scope:

--    type Namespace_Scope is
--       record
--          Namespace: Global_Namespace;
--          Prefix:    Unbounded_Wide_String;
--       end record;
 
   --| A namespace scope ties a namespace to an element in a document and a prefix. Within the
   --| scope of the element, the prefix can be used to denote the namespace. The prefix will
   --| usually be a lot shorter than the name of the namespace, and so easier to use.
   
   
   function Prefix (Element:   in Structural_Element; 
                    Namespace: in Global_Namespace) return Wide_String;
   
   function Namespace (Element: in Structural_Element; 
                       Prefix:  in Wide_String) return Global_Namespace;
   
   function Scope_Root (Element:   in Structural_Element; 
                        Namespace: in Global_Namespace) return access Structural_Element;

   --| The scope of a ... etc
 
   -- function Document (Namespace: in Global_Namespace) return Document_Ref;
   -- function Name     (Namespace: in Global_Namespace) return Wide_String;
 
   -- function Namespace (Document: in Document_Ref;
   --                     Name:     in Wide_String) return Namespace_Ref; -- null if not found
 
   --| There are more operations on namespace scopes declared later in this specification
   --| (after the element type is declared).

   --\
   --------------------------------------------------------------------------------------------
   --/ Notations:

   type Notation_Ref is private;

   function Document (Notation: in Notation_Ref) return Document_Ref;
   function Name     (Notation: in Notation_Ref) return Wide_String;
   function ...      (Notation: in Notation_Ref) return Wide_String;

   procedure Append_Notation (Document:  in  Document_Ref;
                              Reference: out Notation_Ref;
                              ...
                              Name:      in  Wide_String);

   --\
   --------------------------------------------------------------------------------------------
   --/ Notation sets:

   type Notation_Set is private;

   function Member_Count (Set: in Notation_Set) return Natural;

   function Member (Set:    in Notation_Set;
                    Number: in Positive) return Notation_Ref;

   procedure Append (Set:      in out Notation_Set;
                     Notation: in     Notation_Ref);

   procedure Clear (Set: in out Notation_Set);

   --\
   --------------------------------------------------------------------------------------------
   --/ Attribute definitions:

   type Attribute_Type_Kind is (String,
                                Enumeration,
                                Notation,
                                ID,
                                IDREF,
                                IDREFS,
                                Entity,
                                Entities,
                                Name_Token,
                                Name_Tokens);

   type Attribute_Default_Kind is (Implied, Fixed, Required, Optional);

   type Attribute_Definition (Type_Kind:    Attribute_Type_Kind;
                              Default_Kind: Attribute_Default_Kind) is
      record
         Default
         case Kind is
            when Enumeration =>
               Allowed_Strings: Wide_String_Set;
               case Default_Kind is
                  when Implied => null;
                  when others  => Default_Enumeration: Positive;
               end case;
            when Notation =>
               Allowed_Notations: Notation_Set;
               case Default_Kind is
                  when Implied => null;
                  when others  => Default_Notation: Positive;
               end case;
            when others =>
               case Default_Kind is
                  when Implied => null;
                  when others  => Default_Value: Ada.Strings.Unbounded.Unbounded_Wide_String;
               end case;
         end case;
      end record;

   --\
   --------------------------------------------------------------------------------------------
   --/ Attributes:
   
   --| ...
   
   type Element_Attribute is private;

   function Document   (Attribute: in Element_Attribute) return Document_Ref;
   function Name       (Attribute: in Element_Attribute) return Qualified_Name;
   function Definition (Attribute: in Element_Attribute) return Attribute_Definition;
   function Tag        (Attribute: in Element_Attribute) return Element_Tag;

   function Has_Attribute (Tag:  in Element_Tag;
                           Name: in Qualified_Name) return Boolean;

   function Attribute (Tag:  in Element_Tag;
                       Name: in Qualified_Name) return Element_Attribute; -- exception if not found

   type Attribute_List is array (Positive range <>) of Element_Attribute;

   function Attributes (Tag: in Element_Tag) return Attribute_List;

   --| The Attributes function ...
   
   --| The attributes of an element tag have an order. This order is not usually significant, 
   --| but the list returned by the Attributes functions retains this order. The 
   --| Append_Attribute procedures (below) always add new attributes to the end of the list.

   procedure Append_Attribute (Tag:        in  Element_Tag;
                               Attribute:  out Element_Attribute;
                               Definition: in  Attribute_Definition;
                               Name:       in  Qualified_Name);

   procedure Append_Attribute (Tag:        in  Element_Tag;
                               Definition: in  Attribute_Definition;
                               Name:       in  Qualified_Name);

   procedure Clear_Attributes (Tag: in  Element_Tag);

   procedure Set_Attributes (Tag:  in  Element_Tag;
                             List: in Attribute_List);

   --\
   --------------------------------------------------------------------------------------------
   --/ Attribute value associations:

   type Attribute_Association is private; 
   
   --| ... associates a value with an attribute of an element.

   function Attribute (Association: in Attribute_Association) return Element_Attribute;

   function Value (Association: in Attribute_Association) return Wide_String;

   function Association (Element:   in Structural_Element;
                         Attribute: in Element_Attribute) return Attribute_Association;

   function Is_Default (Element:   in Structural_Element;
                        Attribute: in Element_Attribute) return Boolean;

   procedure Associate (Element:   in Structural_Element;
                        Attribute: in Element_Attribute;
                        Value:     in Wide_String);

   procedure Set_Default (Element:   in Structural_Element;
                          Attribute: in Element_Attribute);

   --\
   --------------------------------------------------------------------------------------------
   --/ Nodes:

   type Node_Kind is (Element,
                      Processing_Instruction,
                      Entity_Reference, -- typed (non-character)
                      Character_Data);

   type Structural_Node is private;
   
   --| ...
   
   function Kind (Node: in Structural_Node) return Node_Kind;

   function As_Element (Node: in Structural_Node) return Structural_Element;   -- Kind=Element
   function Text       (Node: in Structural_Node) return Multiline_Text;     -- Kind=Character_Data
   function Target     (Node: in Structural_Node) return Wide_String;        -- Kind=Processing_Instruction
   function Parameter  (Node: in Structural_Node) return Multiline_Text;     -- Kind=Processing_Instruction
   function System_ID  (Node: in Structural_Node) return Wide_String;        -- Kind=Entity_Reference
   function Public_ID  (Node: in Structural_Node) return Wide_String;        -- Kind=Entity_Reference
   function Notation   (Node: in Structural_Node) return Notation_Ref;       -- Kind=Entity_Reference

   --\
   --------------------------------------------------------------------------------------------
   --/ Element children:

   function Has_Document (Node: in Structural_Node) return Boolean;

   function Document (Node: in Structural_Node) return Structural_Document;

   --| A structural node can be either a 'document node', in which case it is part of the 
   --| hierarchy of nodes within a document
   --| or it can be a 'non-document node', in which case it is outside any document 
   --| (existing in this state only as a part of the interim 
   --| processing of a document).
   
   --| The function Has_Document ...
   
   function Has_Parent (Element: in Structural_Element) return Boolean;
   function Parent     (Element: in Structural_Element) return Structural_Element; -- exception if root

   --| ...
   
   --| A document element which does not have a parent must be the (one and only) root of the 
   --| document. There are no restrictions on which non-document elements can have no parent.
   
   function Copy (Node: in Structural_Node) return Structural_Node;
   
   --| The Copy function returns a new structural node that is a copy of the given Node. The
   --| returned node is always a non-document node; if it is an element, it has no parent.
   
   function Child_Count (Element: in Structural_Element) return Natural;
   
   --| The function Child_Count returns how many child nodes the given Element has.
   
   type Node_Processor is access procedure Process (Node: in Structural_Node);

   procedure Iterate_Children (Element:   in Structural_Element;
                               Processor: in Node_Processor);

   type Node_List is array (Positive range <>) of Structural_Element;
   
   --| ...
   
   --| The nodes in a node list will all be non-document nodes, and those which are elements 
   --| will have no parent.
   
   function Children (Element: in Structural_Element) return Node_List;
   
   --| The function Children returns an array which contains the child nodes of the given 
   --| Element. It returns an empty array (range 1..0) if the Element has no children.
   
   function Copy_Children (Element: in Structural_Element) return Node_List;
   
   --| The function Copy_Children returns an array which contains a copy (as if copied using 
   --| the Copy function) of each of the child nodes of the given 
   --| Element. It returns an empty array (range 1..0) if the Element has no children.
   
   procedure Append_Child (Parent:    in  Structural_Element;
                           Node:      in  Structural_Node;
                           Reference: out Structural_Node);

   procedure Append_Child (Parent: in  Structural_Element;
                           Node:   in  Structural_Node);

   procedure Clear_Children (Parent: in Structural_Element);

   procedure Set_Children (Parent: in Structural_Element;
                           List:   in Node_List);

   --\
   --------------------------------------------------------------------------------------------
   --/ Node constructors:

   --| ...
   
   --| 
   
   function Element (Tag: in  Element_Tag) return Structural_Element;

   function Element (Tag:          in Element_Tag;
                     Associations: in Association_List) return Structural_Element;

   function Processing_Instruction (Target: in Wide_String;
                                    Data:   in Multiline_Text) return Structural_Node; --Kind=Processing_Instruction

   function Entity (System_Ref: in  Wide_String;
                    Global_Ref: in  Wide_String) return Structural_Node; -- Kind=Entity

   function Character_Data (Text: in Multiline_Text) return Structural_Node -- Kind=Character_Data

   function Character_Data (Text: in Wide_String) return Structural_Node -- single line, Kind=Character_Data

                    
                    
                    
--    procedure Append_Textual_Child (Parent:    in  Structural_Element;
--                                    Text:      in  Multiline_Text;
--                                    Kind:      in  Textual_Node_Kind := Text;
--                                    Reference: out Structural_Node);
-- 
--    procedure Append_Textual_Child (Parent: in Structural_Element;
--                                    Text:   in Multiline_Text;
--                                    Kind:   in Textual_Node_Kind := Text);




   --------------------------------------------------------------------------------------------
   --------------------------------------------------------------------------------------------



   --------------------------------------------------------------------------------------------
   --/ Reading and writing a document:

   type Error_Action is (Exception_Only,
                         Report_and_Exception,
                         Report_and_Recovery,
                         Recovery_Only);

   type File_Opener is access procedure (File: in out Ada.Wide_Text_IO.File_Type;
                                         Mode: in     Ada.Wide_Text_IO.File_Mode;
                                         Name: in     String;
                                         Form: in     String);

   procedure Read (File:     in out Ada.Wide_Text_IO.File_Type;
                   Item:     out    Document_Ref;
                   On_Error: in     Error_Action := Exception_Only;
                   Opener:   in     File_Opener  := Ada.Wide_Text_IO.Open);

   procedure Write (File: in out Ada.Wide_Text_IO.File_Type;
                    Item: in     Document_Ref);

   --\
   --------------------------------------------------------------------------------------------







































   --/ String tokens:

   type String_Token is private;

   Null_String_Token: constant String_Token;

   function To_String (Source: in String_Token) return Wide_String
   function To_Token  (Source: in Wide_String)  return String_Token;

   procedure Clear (Token: in out String_Token);

   --\



   --/ :

   type Document_Type_Declaration is private;

   procedure Clear (DTD: in out Document_Type_Declaration);

   --\


   --/ :

   procedure Read (File: in out Ada.Wide_Text_IO.File_Type;
                   Item: out    Document_Type_Declaration);

   procedure Write (File: in out Ada.Wide_Text_IO.File_Type;
                    Item: in     Document_Type_Declaration);

   --\




   --/ :

   function DTD (Doc: in Document) return Document_Type_Declaration;

   --\




   function Element_Type_Count (Source: in Document) return Natural;

   function El_Type (Source: in Document;
                     N:      in Positive) return Element_Type;

   function Elmt_N (Source: in Document_Type_Declaration;
                    Name:   in Wide_String;
                    NS:     in Namespace := Default_Namespace) return Natural; -- 0 if not found




   function Attribute_Count (Source: in Element) return Natural;

   function Attr (Source: in Element;
                  N:      in Positive) return Attribute;

   function Attr_N (Source: in Element;
                    Name:   in Wide_String;
                    NS:     in Namespace) return Natural; -- 0 if not found

   function Attr_N (Source: in Element;
                    Name:   in Wide_String) return Natural; -- namespace of element




   function Child_Count (Element: in Structural_Element) return Natural;

   function Child_Form (Element: in Structural_Element; N: in Positive) return Node_Form;

   function Child (Element: in Structural_Element; N: in Positive) return Instance; -- Element_Node
   function Child (Element: in Structural_Element; N: in Positive) return Wide_String; -- Text_Node
   function Child (Element: in Structural_Element; N: in Positive) return Instruction; -- Instruction_Node

   procedure Insert_Child_Element (Source: in  Instance;
                                   Before: in  Positive;
                                   Elmt:   in  Element;
                                   Inst:   out Instance);

   procedure Append_Child_Element (Source: in  Instance;
                                   Elmt:   in  Element;
                                   Inst:   out Instance);




   procedure Append_Child_Text         (Element: in Structural_Element; Text: in Wide_String);

   procedure Append_Child_Comment      (Element: in Structural_Element; Text: in Wide_String);

   procedure Append_Child_Instruction  (Element: in Structural_Element; ...);

   procedure Append_Child_...


   procedure Clear_Children (Source: in Element);

   
   --------------------------------------------------------------------------------------------
   --/ Multiline text:

   type Multiline_Text is private;

   Null_Text: constant Multiline_Text;

   function Lines (Text: in Multiline_Text) return Natural;

   function Line (Text:   in Multiline_Text;
                  Number: in Positive) return Wide_String;

???   function Normalize (Text: in Multiline_Text) return Multiline_Text;
   -- remove leading and trailing blank lines & leading and trailing spaces from each line

???   function Linearize (Text:  in Multiline_Text;
                       Break: in Wide_String := Default_Line_Break) return Wide_String;
   -- converts line breaks to characters

   procedure Append (Text:       in out Multiline_Text;
                     Additional: in     Wide_String);
   
   procedure Append (Text:       in out Multiline_Text;
                     Additional: in     Multiline_Text);

   type Multiline_Array is array (Positive range <>) of Wide_String;
   
   function To_Array (Text: in Multiline_Text) return Multiline_Array;

   --\