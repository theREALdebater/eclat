--------------------
--#Module:  Ada.Strings.Bounded [package body]
--#Product: TW-ADA-WIN32-00
--#Host:    TW-ADA-WIN32-00
--#Target:  WIN32
--#Created: 1998-03-22
--#Update:  0 [initial version]
--#Author:  NJR
--#Status:  0 [incomplete]

-- Copyright 1998 of the entire contents of this file is owned by:
-- ThoughtWing Software, 3 Brambledown Road, South Croydon, United Kingdom.
-- All rights reserved.

-- See RM A.4.4.



--------------------
with Ada.Strings.Maps;

package body Ada.Strings.Bounded is


   -----------
   package body Generic_Bounded_Length is


      ----------
      function Length (Source: in Bounded_String) return Length_Range is
      begin
         return Source.Length;
      end;


      ----------
      function To_Bounded_String (Source: in String;
                                  Drop:   in Truncation := Error)
                                                   return Bounded_String is
         Result: Bounded_String;
      begin
         if Source'Length <= Max_Length then
            Result.Length := Source'Length;
            Result.Value(1..Source'Length) := Source;
         else
            Result.Length := Max_Length;
            case Drop is
               when Left  => Result.Value :=
                           Source(Source'First..Source'First-1+Max_Length);
               when Right => Result.Value :=
                             Source(Source'Last-Max_Length+1..Source'Last);
               when Error => raise Length_Error;
            end case;
         end if;
         return Result;
      end To_Bounded_String;


      ----------
      function To_String (Source: in Bounded_String) return String is
         Result: constant String(1..Source.Length) :=
                                            Source.Value(1..Source.Length);
      begin
         return Result;
      end;


      ----------
      function Append (Left, Right: in Bounded_String;
                       Drop:        in Truncation := Error)
                                                   return Bounded_String is
      begin
         if Max_Length - Left.Length >= Right.Length then
            return (Left.Length+Right.Length,
                    Left.Value(1..Left.Length) &
                                             Right.Value(1..Right.Length));
         else
            case Drop is
               when Left  => return (Max_Length,
                                     Left.Value(Left.Length-
                                                Max_Length+
                                                Right.Length+
                                                1..Left.Length) &
                                             Right.Value(1..Right.Length));
               when Right => return (Max_Length,
                                     Left.Value(1..Left.Length) &
                                     Right.Value(1..Max_Length-
                                                    Left.Length));
               when Error => raise Length_Error;
            end case;
         end if;
      end Append;


      ----------
      function Append (Left:  in Bounded_String;
                       Right: in String;
                       Drop:  in Truncation := Error)
                                                   return Bounded_String is
      begin
         if Max_Length - Left.Length >= Right'Length then
            return (Left.Length+Right'Length,
                    Left.Value(1..Left.Length) & Right);
         else
            case Drop is
               when Left  => return (Max_Length,
                                     Left.Value(Left.Length-
                                                Max_Length+
                                                Right'Length+
                                                1..Left.Length) & Right);
               when Right => return (Max_Length,
                                     Left.Value(1..Left.Length) &
                                     Right(Right'First..Right'First+
                                                        Max_Length-
                                                        Left.Length));
               when Error => raise Length_Error;
            end case;
         end if;
      end Append;


      ----------
      function Append (Left:  in String;
                       Right: in Bounded_String;
                       Drop:  in Truncation := Error) return Bounded_String is


      ----------
      function Append (Left:  in Bounded_String;
                       Right: in Character;
                       Drop: in Truncation := Error) return Bounded_String is


      ----------
      function Append (Left:  in Character;
                       Right: in Bounded_String;
                       Drop: in Truncation := Error) return Bounded_String is


      ----------
      procedure Append (Source:   in out Bounded_String;
                        New_Item: in Bounded_String;
                        Drop:     in Truncation: = Error) is


      ----------
      procedure Append (Source:   in out Bounded_String;
                        New_Item: in String;
                        Drop:     in Truncation: = Error) is


      ----------
      procedure Append (Source:   in out Bounded_String;
                        New_Item: in Character;
                        Drop:     in Truncation: = Error) is


      ----------
      function "&" (Left, Right: in Bounded_String) return Bounded_String is


      ----------
      function "&" (Left:  in Bounded_String;
                    Right: in String) return Bounded_String is


      ----------
      function "&" (Left:  in String;
                    Right: in Bounded_String) return Bounded_String is


      ----------
      function "&" (Left:  in Bounded_String;
                    Right: in Character) return Bounded_String is


      ----------
      function "&" (Left:  in Character;
                    Right: in Bounded_String) return Bounded_String is


      ----------
      function Element (Source: in Bounded_String;
                        Index:  in Positive) return Character is


      ----------
      procedure Replace_Element (Source: in out Bounded_String;
                                 Index:  in Positive;
                                 By:     in Character) is


      ----------
      function Slice (Source: in Bounded_String;
                      Low:    in Positive;
                      High:   in Natural) return String is


      ----------
      function "="  (Left, Right: in Bounded_String) return Boolean is


      ----------
      function "="  (Left:  in Bounded_String;
                     Right: in String) return Boolean is


      ----------
      function "="  (Left:  in String;
                     Right: in Bounded_String) return Boolean is


      ----------
      function "<"  (Left, Right: in Bounded_String) return Boolean is


      ----------
      function "<"  (Left:  in Bounded_String is


      ----------
      function "<"  (Left:  in String;
                     Right: in Bounded_String) return Boolean is


      ----------
      function "<=" (Left, Right: in Bounded_String) return Boolean is


      ----------
      function "<="  (Left:  in Bounded_String;
                      Right: in String) return Boolean is


      ----------
      function "<="  (Left:  in String;
                      Right: in Bounded_String) return Boolean is


      ----------
      function ">"  (Left, Right: in Bounded_String) return Boolean is


      ----------
      function ">"  (Left:  in Bounded_String;
                     Right: in String) return Boolean is


      ----------
      function ">"  (Left:  in String;
                     Right: in Bounded_String) return Boolean is


      ----------
      function ">=" (Left, Right: in Bounded_String) return Boolean is


      ----------
      function ">="  (Left:  in Bounded_String;
                      Right: in String) return Boolean is


      ----------
      function ">="  (Left:  in String;
                      Right: in Bounded_String) return Boolean is


      ----------
      function Index (Source:  in Bounded_String;
                      Pattern: in String;
                      Going:   in Direction := Forward;
                      Mapping: in Maps.Character_Mapping :=
                                             Maps.Identity) return Natural is


      ----------
      function Index (Source:  in Bounded_String;
                      Pattern: in String;
                      Going:   in Direction := Forward;
                      Mapping: in Maps.Character_Mapping_Function)
                                                            return Natural is


      ----------
      function Index (Source: in Bounded_String;
                      Set:    in Maps.Character_Set;
                      Test:   in Membership := Inside;
                      Going:  in Direction := Forward) return Natural is


      ----------
      function Index_Non_Blank (Source: in Bounded_String;
                                Going:  in Direction := Forward)
                                                            return Natural is


      ----------
      function Count (Source:  in Bounded_String;
                      Pattern: in String;
                      Mapping: in Maps.Character_Mapping
                                          := Maps.Identity) return Natural is


      ----------
      function Count (Source:  in Bounded_String;
                      Pattern: in String;
                      Mapping: in Maps.Character_Mapping_Function)
                                                            return Natural is


      ----------
      function Count (Source: in Bounded_String;
                      Set:    in Maps.Character_Set) return Natural is


      ----------
      procedure Find_Token (Source: in Bounded_String;
                            Set:    in Maps.Character_Set;
                            Test:   in Membership;
                            First:  out Positive;
                            Last:   out Natural) is


      ----------
      function Translate (Source:  in Bounded_String;
                          Mapping: in Maps.Character_Mapping)
                                                     return Bounded_String is


      ----------
      procedure Translate (Source:  in out Bounded_String;
                           Mapping: in Maps.Character_Mapping) is


      ----------
      function Translate (Source:  in Bounded_String;
                          Mapping: in Maps.Character_Mapping_Function)
                                                     return Bounded_String is


      ----------
      procedure Translate (Source:  in out Bounded_String;
                           Mapping: in Maps.Character_Mapping_Function) is


      ----------
      function Replace_Slice (Source:   in Bounded_String;
                              Low:      in Positive;
                              High:     in Natural;
                              By:       in String;
                              Drop:     in Truncation := Error)
                                                     return Bounded_String is


      ----------
      procedure Replace_Slice (Source:   in out Bounded_String;
                               Low:      in Positive;
                               High:     in Natural;
                               By:       in String;
                               Drop:     in Truncation := Error) is


      ----------
      function Insert (Source:   in Bounded_String;
                       Before:   in Positive;
                       New_Item: in String;
                       Drop:     in Truncation := Error)
                                                     return Bounded_String is


      ----------
      procedure Insert (Source:   in out Bounded_String;
                        Before:   in Positive;
                        New_Item: in String;
                        Drop:     in Truncation := Error) is


      ----------
      function Overwrite (Source:   in Bounded_String;
                          Position: in Positive;
                          New_Item: in String;
                          Drop:     in Truncation := Error)
                                                     return Bounded_String is


      ----------
      procedure Overwrite (Source:   in out Bounded_String;
                           Position: in Positive;
                           New_Item: in String;
                           Drop:     in Truncation := Error) is


      ----------
      function Delete (Source:  in Bounded_String;
                       From:    in Positive;
                       Through: in Natural) return Bounded_String is


      ----------
      procedure Delete (Source:  in out Bounded_String;
                        From:    in Positive;
                        Through: in Natural) is


      ----------
      function Trim (Source: in Bounded_String;
                     Side:   in Trim_End) return Bounded_String is


      ----------
      procedure Trim (Source: in out Bounded_String;
                      Side:   in Trim_End) is


      ----------
      function Trim (Source: in Bounded_String;
                     Left:   in Maps.Character_Set;
                     Right:  in Maps.Character_Set) return Bounded_String is


      ----------
      procedure Trim (Source: in out Bounded_String;
                      Left:   in Maps.Character_Set;
                      Right:  in Maps.Character_Set) is


      ----------
      function Head (Source: in Bounded_String;
                     Count:  in Natural;
                     Pad:    in Character := Space;
                     Drop:   in Truncation := Error) return Bounded_String is


      ----------
      procedure Head (Source: in out Bounded_String;
                      Count:  in Natural;
                      Pad:    in Character := Space;
                      Drop:   in Truncation := Error) is


      ----------
      function Tail (Source: in Bounded_String;
                     Count:  in Natural;
                     Pad:    in Character := Space;
                     Drop:   in Truncation := Error) return Bounded_String is


      ----------
      procedure Tail (Source: in out Bounded_String;
                      Count:  in Natural;
                      Pad:    in Character := Space;
                      Drop:   in Truncation := Error) is


      ----------
      function "*" (Left:  in Natural;
                    Right: in Character) return Bounded_String is


      ----------
      function "*" (Left:  in Natural;
                    Right: in String) return Bounded_String is


      ----------
      function "*" (Left:  in Natural;
                    Right: in Bounded_String) return Bounded_String is


      ----------
      function Replicate (Count: in Natural;
                          Item:  in Character;
                          Drop:  in Truncation := Error)
                                                     return Bounded_String is


      ----------
      function Replicate (Count: in Natural;
                          Item:  in String;
                          Drop:  in Truncation := Error)
                                                     return Bounded_String is


      ----------
      function Replicate (Count: in Natural;
                          Item:  in Bounded_String;
                          Drop:  in Truncation := Error)
                                                     return Bounded_String is


   end Generic_Bounded_Length;

end Ada.Strings.Bounded;



--------------------
-- End of File.

