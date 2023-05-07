--------------------
--#Module:  Ada.Strings.Maps [package body]
--#Product: TW-ADA-WIN32-00
--#Host:    TW-ADA-WIN32-00
--#Target:  WIN32
--#Created: 1998-03-13
--#Update:  0 [initial version]
--#Author:  NJR
--#Status:  2 [ready]

-- Copyright 1998 of the entire contents of this file is owned by:
-- ThoughtWing Software, 3 Brambledown Road, South Croydon, United Kingdom.
-- All rights reserved.

-- See RM A.4.2.



--------------------
package body Ada.Strings.Maps is

   ----------
   function To_Set (Ranges: in Character_Ranges) return Character_Set is
      Set: Character_Set := Null_Set;
   begin
      for R in Ranges'Range loop
         for C in Ranges(R).Low .. Ranges(R).High loop
            Set(C) := True;
         end loop;
      end loop;
      return Set;
   end To_Set;
   
   ----------
   function To_Set (Span: in Character_Range) return Character_Set is
      Set: Character_Set := Null_Set;
   begin
      for C in Span.Low .. Span.High loop
         Set(C) := True;
      end loop;
      return Set;
   end To_Set;

   ----------
   function To_Ranges (Set: in Character_Set) return Character_Ranges is
      Ranges:  Character_Ranges(1..(Character_Set'Length-1)/3+1);
      Count:   Natural := 0;
      Running: Boolean := False;
   begin
      for C in Character'Range loop
         if Set(C) /= Running then
            if Running then
               Ranges(Count).High := Character'Pred(C);
            else
               Count := Count+1;
               Ranges(Count).Low := C;
            end if;
         end if;
      end loop;
      if Running then
         Ranges(Count).High := Character'Last;
      end if;
      return Ranges(1..Count);
   end To_Ranges;

   ----------
   function "=" (Left, Right: in Character_Set) return Boolean renames "=";
   
   ----------
   function "not" (Right: in Character_Set) return Character_Set
                                                             renames "not";
   
   ----------
   function "and" (Left, Right: in Character_Set) return Character_Set
                                                             renames "and";

   ----------
   function "or" (Left, Right: in Character_Set) return Character_Set
                                                              renames "or";

   ----------
   function "xor" (Left, Right: in Character_Set) return Character_Set
                                                             renames "xor";

   ----------
   function "–" (Left, Right: in Character_Set) return Character_Set is
   begin
      return Left and not Right;
   end;

   ----------
   function Is_In (Element: in Character;
                   Set:     in Character_Set) return Boolean is
   begin
      return Set(Element);
   end;

   ----------
   function Is_Subset (Elements: in Character_Set;
                       Set:      in Character_Set) return Boolean is
   begin
      return Set or Elements = Set;
   end;

   ----------
   function To_Set (Sequence: in Character_Sequence)
                                                    return Character_Set is
      Set: Character_Set := Null_Set;
   begin
      for i in Sequence'Range loop
         Set(Sequence(i)) := True;
      end loop;
      return Set;
   end To_Set;

   ----------
   function To_Set (Singleton: in Character) return Character_Set is
   begin
      return Character_Set'(Singleton => True, others => False);
   end;

   ----------
   function To_Sequence (Set: in Character_Set)
                                               return Character_Sequence is
      Sequence: Character_Sequence(1..Set'Length);
      Count:    Natural := 0;
   begin
      for C in Set'Range loop
         if Set(C) then
            Count := Count+1;
            Sequence(Count) := C;
         end if;
      end loop;
      return Sequence(1..Count);
   end To_Sequence;

   ----------
   function Value (Map:     in Character_Mapping;
                   Element: in Character) return Character is
   begin
      return Map(Element);
   end;

   ----------
   function To_Mapping (From, To: in Character_Sequence)
                                                return Character_Mapping is
      Map: Character_Mapping := Identity;
      Set: Character_Set := Null_Set; -- characters already mapped
   begin
      if From'Length /= To'Length then raise Translation_Error; end if;
      for i in From'Range loop
         if Set(From(i)) then raise Translation_Error; end if;
         Set(From(i)) := True;
         Map(From(i)) := To(i+(To'First-From'First));
      end loop;
      return Map;
   end To_Mapping;

   ----------
   function To_Domain (Map: in Character_Mapping)
                                               return Character_Sequence is
      Sequence: Character_Sequence(1..Map'Length);
      Count:    Natural := 0;
   begin
      for C in Map'Range loop
         if Map(C) /= C then
            Count := Count+1;
            Sequence(Count) := C;
         end if;
      end loop;
      return Sequence(1..Count);
   end To_Domain;

   ----------
   function To_Range (Map: in Character_Mapping)
                                               return Character_Sequence is
      Sequence: Character_Sequence(1..Map'Length);
      Count:    Natural := 0;
   begin
      for C in Map'Range loop
         if Map(C) /= C then
            Count := Count+1;
            Sequence(Count) := Map(C);
         end if;
      end loop;
      for C in Map'Range loop
         if Map(C) = C then
            Count := Count+1;
            Sequence(Count) := Map(C);
         end if;
      end loop;
      if Count /= Map'Length then raise Program_Error; end if;
      return Sequence;
   end To_Range;  

end Ada.Strings.Maps;



--------------------
-- End of File.

