--------------------
--#Module:  Ada.Strings.Bounded [package specification]
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

-- See RM A.4.4.



--------------------
with Ada.Strings.Maps;

package Ada.Strings.Bounded is

   pragma Restricted_Hierarchy(Bounded,Standard);

   pragma Preelaborate(Bounded);

   -----
   -- Generic package encapsualting functionality for Bounded_Strings of
   -- one particular maximum length:

   generic
      Max: Positive; -- maximum length of a Bounded_String

   package Generic_Bounded_Length is

      Max_Length: constant Positive := Max;

      type Bounded_String is private;

      Null_Bounded_String: constant Bounded_String;

      subtype Length_Range is Natural range 0..Max_Length;

      function Length (Source: in Bounded_String) return Length_Range;

      -----
      -- Conversion, Concatenation, and Selection functions:

      function To_Bounded_String (Source: in String;
                                  Drop:   in Truncation := Error)
                                                     return Bounded_String;

      function To_String (Source: in Bounded_String) return String;

      function Append (Left, Right: in Bounded_String;
                       Drop:        in Truncation := Error)
                                                     return Bounded_String;

      function Append (Left:  in Bounded_String;
                       Right: in String;
                       Drop: in Truncation := Error) return Bounded_String;

      function Append (Left:  in String;
                       Right: in Bounded_String;
                       Drop: in Truncation := Error) return Bounded_String;

      function Append (Left:  in Bounded_String;
                       Right: in Character;
                       Drop: in Truncation := Error) return Bounded_String;

      function Append (Left:  in Character;
                       Right: in Bounded_String;
                       Drop: in Truncation := Error) return Bounded_String;

      procedure Append (Source:   in out Bounded_String;
                        New_Item: in Bounded_String;
                        Drop:     in Truncation: = Error);

      procedure Append (Source:   in out Bounded_String;
                        New_Item: in String;
                        Drop:     in Truncation: = Error);

      procedure Append (Source:   in out Bounded_String;
                        New_Item: in Character;
                        Drop:     in Truncation: = Error);

      function "&" (Left, Right: in Bounded_String) return Bounded_String;

      function "&" (Left:  in Bounded_String;
                    Right: in String) return Bounded_String;

      function "&" (Left:  in String;
                    Right: in Bounded_String) return Bounded_String;

      function "&" (Left:  in Bounded_String;
                    Right: in Character) return Bounded_String;

      function "&" (Left:  in Character;
                    Right: in Bounded_String) return Bounded_String;

      function Element (Source: in Bounded_String;
                        Index:  in Positive) return Character;

      procedure Replace_Element (Source: in out Bounded_String;
                                 Index:  in Positive;
                                 By:     in Character);

      function Slice (Source: in Bounded_String;
                      Low:    in Positive;
                      High:   in Natural) return String;

      -----
      -- Comparison functions:

      function "="  (Left, Right: in Bounded_String) return Boolean;

      function "="  (Left:  in Bounded_String;
                     Right: in String) return Boolean;

      function "="  (Left:  in String;
                     Right: in Bounded_String) return Boolean;

      function "<"  (Left, Right: in Bounded_String) return Boolean;

      function "<"  (Left:  in Bounded_String;
                     Right: in String) return Boolean;

      function "<"  (Left:  in String;
                     Right: in Bounded_String) return Boolean;

      function "<=" (Left, Right: in Bounded_String) return Boolean;

      function "<="  (Left:  in Bounded_String;
                      Right: in String) return Boolean;

      function "<="  (Left:  in String;
                      Right: in Bounded_String) return Boolean;

      function ">"  (Left, Right: in Bounded_String) return Boolean;

      function ">"  (Left:  in Bounded_String;
                     Right: in String) return Boolean;

      function ">"  (Left:  in String;
                     Right: in Bounded_String) return Boolean;

      function ">=" (Left, Right: in Bounded_String) return Boolean;

      function ">="  (Left:  in Bounded_String;
                      Right: in String) return Boolean;

      function ">="  (Left:  in String;
                      Right: in Bounded_String) return Boolean;

      -----
      -- Search functions:

      function Index (Source:  in Bounded_String;
                      Pattern: in String;
                      Going:   in Direction := Forward;
                      Mapping: in Maps.Character_Mapping :=
                                             Maps.Identity) return Natural;

      function Index (Source:  in Bounded_String;
                      Pattern: in String;
                      Going:   in Direction := Forward;
                      Mapping: in Maps.Character_Mapping_Function)
                                                            return Natural;

      function Index (Source: in Bounded_String;
                      Set:    in Maps.Character_Set;
                      Test:   in Membership := Inside;
                      Going:  in Direction := Forward) return Natural;

      function Index_Non_Blank (Source: in Bounded_String;
                                Going:  in Direction := Forward)
                                                            return Natural;

      function Count (Source:  in Bounded_String;
                      Pattern: in String;
                      Mapping: in Maps.Character_Mapping
                                          := Maps.Identity) return Natural;

      function Count (Source:  in Bounded_String;
                      Pattern: in String;
                      Mapping: in Maps.Character_Mapping_Function)
                                                            return Natural;

      function Count (Source: in Bounded_String;
                      Set:    in Maps.Character_Set) return Natural;

      procedure Find_Token (Source: in Bounded_String;
                            Set:    in Maps.Character_Set;
                            Test:   in Membership;
                            First:  out Positive;
                            Last:   out Natural);

      -----
      -- String translation subprograms:

      function Translate (Source:  in Bounded_String;
                          Mapping: in Maps.Character_Mapping)
                                                     return Bounded_String;

      procedure Translate (Source:  in out Bounded_String;
                           Mapping: in Maps.Character_Mapping);

      function Translate (Source:  in Bounded_String;
                          Mapping: in Maps.Character_Mapping_Function)
                                                     return Bounded_String;

      procedure Translate (Source:  in out Bounded_String;
                           Mapping: in Maps.Character_Mapping_Function);

      -----
      -- String transformation subprograms:

      function Replace_Slice (Source:   in Bounded_String;
                              Low:      in Positive;
                              High:     in Natural;
                              By:       in String;
                              Drop:     in Truncation := Error)
                                                     return Bounded_String;

      procedure Replace_Slice (Source:   in out Bounded_String;
                               Low:      in Positive;
                               High:     in Natural;
                               By:       in String;
                               Drop:     in Truncation := Error);

      function Insert (Source:   in Bounded_String;
                       Before:   in Positive;
                       New_Item: in String;
                       Drop:     in Truncation := Error)
                                                     return Bounded_String;

      procedure Insert (Source:   in out Bounded_String;
                        Before:   in Positive;
                        New_Item: in String;
                        Drop:     in Truncation := Error);

      function Overwrite (Source:   in Bounded_String;
                          Position: in Positive;
                          New_Item: in String;
                          Drop:     in Truncation := Error)
                                                     return Bounded_String;

      procedure Overwrite (Source:   in out Bounded_String;
                           Position: in Positive;
                           New_Item: in String;
                           Drop:     in Truncation := Error);

      function Delete (Source:  in Bounded_String;
                       From:    in Positive;
                       Through: in Natural) return Bounded_String;

      procedure Delete (Source:  in out Bounded_String;
                        From:    in Positive;
                        Through: in Natural);

      -----
      -- String selector subprograms:

      function Trim (Source: in Bounded_String;
                     Side:   in Trim_End) return Bounded_String;

      procedure Trim (Source: in out Bounded_String;
                      Side:   in Trim_End);

      function Trim (Source: in Bounded_String;
                     Left:   in Maps.Character_Set;
                     Right:  in Maps.Character_Set) return Bounded_String;

      procedure Trim (Source: in out Bounded_String;
                      Left:   in Maps.Character_Set;
                      Right:  in Maps.Character_Set);

      function Head (Source: in Bounded_String;
                     Count:  in Natural;
                     Pad:    in Character := Space;
                     Drop:   in Truncation := Error) return Bounded_String;

      procedure Head (Source: in out Bounded_String;
                      Count:  in Natural;
                      Pad:    in Character := Space;
                      Drop:   in Truncation := Error);

      function Tail (Source: in Bounded_String;
                     Count:  in Natural;
                     Pad:    in Character := Space;
                     Drop:   in Truncation := Error) return Bounded_String;

      procedure Tail (Source: in out Bounded_String;
                      Count:  in Natural;
                      Pad:    in Character := Space;
                      Drop:   in Truncation := Error);

      -----
      -- String constructor subprograms:

      function "*" (Left:  in Natural;
                    Right: in Character) return Bounded_String;

      function "*" (Left:  in Natural;
                    Right: in String) return Bounded_String;

      function "*" (Left:  in Natural;
                    Right: in Bounded_String) return Bounded_String;

      function Replicate (Count: in Natural;
                          Item:  in Character;
                          Drop:  in Truncation := Error)
                                                     return Bounded_String;

      function Replicate (Count: in Natural;
                          Item:  in String;
                          Drop:  in Truncation := Error)
                                                     return Bounded_String;

      function Replicate (Count: in Natural;
                          Item:  in Bounded_String;
                          Drop:  in Truncation := Error)
                                                     return Bounded_String;

   private

      type Bounded_String is
         record
            Length: Length_Range;
            Value:  array (Positive range 1..Max_Length) of Character;
         end record;

      Null_Bounded_String: constant Bounded_String := (0, (others=>Space));

   end Generic_Bounded_Length;

end Ada.Strings.Bounded;



--------------------
-- End of File.

