--------------------
--#Module:  Ada.Strings.Unbounded [package specification]
--#Product: TW-ADA-WIN32-00
--#Host:    TW-ADA-WIN32-00
--#Target:  WIN32
--#Created: 1998-03-22
--#Update:  0 [initial version]
--#Author:  NJR
--#Status:  2 [ready]

-- Copyright 1998 of the entire contents of this file is owned by:
-- ThoughtWing Software, 3 Brambledown Road, South Croydon, United Kingdom.
-- All rights reserved.

-- See RM A.4.5.



--------------------
with Ada.Strings.Maps;

package Ada.Strings.Unbounded is

   pragma Restricted_Hierarchy(Unbounded,Standard);

   pragma Preelaborate(Unbounded);

   -----
   -- The basic type etc:

   type Unbounded_String is private;

   Null_Unbounded_String: constant Unbounded_String;

   function Length (Source: in Unbounded_String) return Natural;

   type String_Access is access all String;

   procedure Free (X: in out String_Access);

   -----
   -- Conversion, Concatenation, and Selection functions:

  function To_Unbounded_String (Source: in String) return Unbounded_String;

   function To_Unbounded_String (Length: in Natural)
                                                   return Unbounded_String;

   function To_String (Source: in Unbounded_String) return String;

   procedure Append (Source:   in out Unbounded_String;
                     New_Item: in Unbounded_String);

   procedure Append (Source:   in out Unbounded_String;
                     New_Item: in String);

   procedure Append (Source:   in out Unbounded_String;
                     New_Item: in Character);

   function "&" (Left, Right: in Unbounded_String) return Unbounded_String;

   function "&" (Left:  in Unbounded_String;
                 Right: in String) return Unbounded_String;

   function "&" (Left:  in String;
                 Right: in Unbounded_String) return Unbounded_String;

   function "&" (Left:  in Unbounded_String;
                 Right: in Character) return Unbounded_String;

   function "&" (Left:  in Character;
                 Right: in Unbounded_String) return Unbounded_String;

   function Element (Source: in Unbounded_String;
                     Index:  in Positive) return Character;

   procedure Replace_Element (Source: in out Unbounded_String;
                              Index:  in     Positive;
                              By:     in     Character);

   function Slice (Source: in Unbounded_String;
                   Low:    in Positive;
                   High:   in Natural) return String;

   -----
   -- Comparison functions:

   function "="  (Left, Right: in Unbounded_String) return Boolean;


   function "="  (Left:  in Unbounded_String;
                  Right: in String) return Boolean;

   function "="  (Left:  in String;
                  Right: in Unbounded_String) return Boolean;

   function "<"  (Left, Right: in Unbounded_String) return Boolean;

   function "<"  (Left:  in Unbounded_String;
                  Right: in String) return Boolean;

   function "<"  (Left:  in String;
                  Right: in Unbounded_String) return Boolean;

   function "<=" (Left, Right: in Unbounded_String) return Boolean;

   function "<="  (Left:  in Unbounded_String;
                   Right: in String) return Boolean;

   function "<="  (Left:  in String;
                   Right: in Unbounded_String) return Boolean;

   function ">"  (Left, Right: in Unbounded_String) return Boolean;

   function ">"  (Left:  in Unbounded_String;
                  Right: in String) return Boolean;

   function ">"  (Left:  in String;
                  Right: in Unbounded_String) return Boolean;

   function ">=" (Left, Right: in Unbounded_String) return Boolean;

   function ">="  (Left:  in Unbounded_String;
                   Right: in String) return Boolean;

   function ">="  (Left:  in String;
                   Right: in Unbounded_String) return Boolean;

   -----
   -- Search subprograms:

   function Index (Source:  in Unbounded_String;
                   Pattern: in String;
                   Going:   in Direction := Forward;
                   Mapping: in Maps.Character_Mapping := Maps.Identity)
                                                            return Natural;

   function Index (Source:  in Unbounded_String;
                   Pattern: in String;
                   Going:   in Direction := Forward;
                   Mapping: in Maps.Character_Mapping_Function)
                                                            return Natural;

   function Index (Source: in Unbounded_String;
                   Set:    in Maps.Character_Set;
                   Test:   in Membership := Inside;
                   Going:  in Direction  := Forward) return Natural;

   function Index_Non_Blank (Source: in Unbounded_String;
                             Going:  in Direction := Forward)
                                                            return Natural;

   function Count (Source:  in Unbounded_String;
                   Pattern: in String;
                   Mapping: in Maps.Character_Mapping := Maps.Identity)
                                                            return Natural;

   function Count (Source:  in Unbounded_String;
                   Pattern: in String;
                   Mapping: in Maps.Character_Mapping_Function)
                                                            return Natural;

   function Count (Source: in Unbounded_String;
                   Set:    in Maps.Character_Set) return Natural;

   procedure Find_Token (Source: in  Unbounded_String;
                         Set:    in  Maps.Character_Set;
                         Test:   in  Membership;
                         First:  out Positive;
                         Last:   out Natural);

   -----
   -- String translation subprograms:

   function Translate (Source:  in Unbounded_String;
                       Mapping: in Maps.Character_Mapping)
                                                   return Unbounded_String;

   procedure Translate (Source:  in out Unbounded_String;
                        Mapping: in Maps.Character_Mapping);

   function Translate (Source:  in Unbounded_String;
                       Mapping: in Maps.Character_Mapping_Function)
                                                   return Unbounded_String;

   procedure Translate (Source:  in out Unbounded_String;
                        Mapping: in Maps.Character_Mapping_Function);

   -----
   -- String transformation subprograms:

   function Replace_Slice (Source: in Unbounded_String;
                           Low:    in Positive;
                           High:   in Natural;
                           By:     in String) return Unbounded_String;

   procedure Replace_Slice (Source: in out Unbounded_String;
                            Low:    in Positive;
                            High:   in Natural;
                            By:     in String);

   function Insert (Source:   in Unbounded_String;
                    Before:   in Positive;
                    New_Item: in String) return Unbounded_String;

   procedure Insert (Source:   in out Unbounded_String;
                     Before:   in     Positive;
                     New_Item: in     String);

   function Overwrite (Source:   in Unbounded_String;
                       Position: in Positive;
                       New_Item: in String) return Unbounded_String;

   procedure Overwrite (Source:    in out Unbounded_String;
                        Position:  in     Positive;
                        New_Item:  in     String);

   function Delete (Source:  in Unbounded_String;
                    From:    in Positive;
                    Through: in Natural) return Unbounded_String;

   procedure Delete (Source:  in out Unbounded_String;
                     From:    in     Positive;
                     Through: in     Natural);

   function Trim (Source: in Unbounded_String;
                  Side:   in Trim_End) return Unbounded_String;

   procedure Trim (Source: in out Unbounded_String;
                   Side:   in     Trim_End);

   function Trim (Source: in Unbounded_String;
                  Left:   in Maps.Character_Set;
                  Right:  in Maps.Character_Set) return Unbounded_String;

   procedure Trim (Source: in out Unbounded_String;
                   Left:   in     Maps.Character_Set;
                   Right:  in     Maps.Character_Set);

   function Head (Source: in Unbounded_String;
                  Count:  in Natural;
                  Pad:    in Character := Space) return Unbounded_String;

   procedure Head (Source: in out Unbounded_String;
                   Count:  in     Natural;
                   Pad:    in     Character := Space);

   function Tail (Source: in Unbounded_String;
                  Count:  in Natural;
                  Pad:    in Character := Space) return Unbounded_String;

   procedure Tail (Source: in out Unbounded_String;
                   Count:  in     Natural;
                   Pad:    in     Character := Space);

   -----
   -- String constructor functions:

   function "*" (Left:  in Natural;
                 Right: in Character) return Unbounded_String;

   function "*" (Left:  in Natural;
                 Right: in String) return Unbounded_String;

   function "*" (Left:  in Natural;
                 Right: in Unbounded_String) return Unbounded_String;

private

   type Unbounded_String is access String;

   Null_Unbounded_String: constant Unbounded_String := null;

end Ada.Strings.Unbounded;



--------------------
-- End of File.

