--------------------
--#Module:  Ada.Strings.Fixed [package body]
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

-- See RM A.4.3.



--------------------
package body Ada.Strings.Fixed is


   ----------
   procedure Fill (Target: out String; Filler: in Character) is
   begin
      for i in Target'Range loop Target(i) := Filler; end loop;
   end;


   ----------
   function Is_All (Source: String; Item: Character) return Boolean is
   begin
      for i in Source loop
         if Source(i) /= Item then return False;
      end loop;
      return True;
   end Is_All;


   ----------
   procedure Move (Source:  in  String;
                   Target:  out String;
                   Drop:    in  Truncation := Error;
                   Justify: in  Alignment  := Left;
                   Pad:     in  Character  := Space) is
      S_First:  constant Positive := Source'First;
      S_Last:   constant Positive := Source'Last;
      S_Length: constant Natural  := Source'Length;
      T_First:  constant Positive := Target'First;
      T_Last:   constant Positive := Target'Last;
      T_Length: constant Natural  := Target'Length;
      T_Start:  Positive;
   begin
      if S_Length = T_Length then
         -- this test & arm not necessary but for speed & clarity
         Target := Source;
      elsif S_Length < T_Length then
         case Justify is
            when Left =>
               Target(T_First..T_First-1+S_Length) := Source;
               Fill(Target(T_First+S_Length..T_Last),Pad);
            when Right =>
               Target(T_Last-S_Length+1..T_Last) := Source;
               Fill(Target(T_First..T_Last-S_Length),Pad);
            when Center =>
               T_Start := (T_Length-S_Length)/2;
               Fill(Target(T_First..T_Start-1),Pad);
               Target(T_Start..T_Start-1+S_Length) := Source;
               Fill(Target(T_Start+S_Length..T_Last),Pad);
         end case;
      else -- S_Length > T_Length
         case Drop is
            when Left =>
               Target := Source(S_Last-T_Length+1..S_Last);
            when Right =>
               Target:= Source(S_First..S_First-1+T_Length);
            when Error =>
               case Justify is
                  when Left =>
                     if Is_All(Source(S_First+T_Length..S_Last),Pad) then
                        Target := Source(S_First..S_First-1+T_Length);
                     else
                        raise Length_Error;
                     end if;
                  when Right =>
                     if Is_All(Source(S_First..S_Last-T_Length),Pad) then
                        Target := Source(S_Last-T_Length+1..S_Last);
                     else
                        raise Length_Error;
                     end if;
                  when Center =>
                     raise Length_Error;
               end case;
         end case;
      end if;
   end Move;


   ----------
   function Index (Source:  in String;
                   Pattern: in String;
                   Going:   in Direction: = Forward;
                   Mapping: in Maps.Character_Mapping := Maps.Identity)
                                                          return Natural is
      Comp: constant String(Source'Range) := Translate(Source, Mapping);
   begin
      if Pattern'Length = 0 then
         raise Pattern_Error;
      end if;
      case Going is
         when Forward =>
            for i in Comp'First .. Comp'Last-Pattern'Length+1 loop
               if Comp(i..i+Pattern'Length-1) = Pattern then
                  return i;
               end if;
            end loop;
         when Backward =>
            for i in reverse Comp'First .. Comp'Last-Pattern'Length+1 loop
               if Comp(i..i+Pattern'Length-1) = Pattern then
                  return i;
               end if;
            end loop;
      end case;
      return 0;
   end Index;


   ----------
   function Index (Source:  in String;
                   Pattern: in String;
                   Going:   in Direction := Forward;
                   Mapping: in Maps.Character_Mapping_Function)
                                                          return Natural is
      Comp: constant String(Source'Range) := Translate(Source, Mapping);
   begin
      if Pattern'Length = 0 then
         raise Pattern_Error;
      end if;
      case Going is
         when Forward =>
            for i in Comp'First .. Comp'Last-Pattern'Length+1 loop
               if Comp(i..i+Pattern'Length-1) = Pattern then
                  return i;
               end if;
            end loop;
         when Backward =>
            for i in reverse Comp'First .. Comp'Last-Pattern'Length+1 loop
               if Comp(i..i+Pattern'Length-1) = Pattern then
                  return i;
               end if;
            end loop;
      end case;
      return 0;
   end Index;


   ----------
   function Index (Source: in String;
                   Set:    in Maps.Character_Set;
                   Test:   in Membership := Inside;
                   Going:  in Direction  := Forward) return Natural is
   begin
      case Going is
         when Forward =>
            for i in Source'Range loop
               case Test is
                  when Inside =>
                     if Is_In(Source(i), Set) then
                        return i;
                     end if;
                  when Outside =>
                     if not Is_In(Source(i), Set) then
                        return i;
                     end if;
               end case;
            end loop;
         when Backward =>
            for i in reverse Source'Range loop
               case Test is
                  when Inside =>
                     if Is_In(Source(i), Set) then
                        return i;
                     end if;
                  when Outside =>
                     if not Is_In(Source(i), Set) then
                        return i;
                     end if;
               end case;
            end loop;
      end case;
      return 0;
   end Index;


   ----------
   function Index_Non_Blank (Source: in String;
                             Going:  in Direction: = Forward)
                                                          return Natural is
   begin
      return Index(Source, Maps.To_Set(Space), Outside, Going);
   end;
   

   ----------
   function Count (Source:  in String;
                   Pattern: in String;
                   Mapping: in Maps.Character_Mapping := Maps.Identity)
                                                          return Natural is
      Mapped: constant String(Source'Range) := Translate(Source, Mapping);
      N: Natural := 0;
      i: Natural := Source'First;
   begin
      loop
         i := Index(Mapped(i..Mapped'Last), Pattern);
            -- 'Going' defaults to 'Forward', and 'Mapping' to 'Identity'
            -- propagates Pattern_Error if Pattern is null (as required)
         exit when i = 0;
         N := N+1;
         exit when i > Source'Last - Pattern'Length*2 + 1;
         i := i + Pattern'Length;
      end loop;
      return N;
   end Count;


   ----------
   function Count (Source:  in String;
                   Pattern: in String;
                   Mapping: in Maps.Character_Mapping_Function)
                                                          return Natural is
      Mapped: constant String(Source'Range) := Translate(Source, Mapping);
      N: Natural := 0;
      i: Natural := Source'First;
   begin
      loop
         i := Index(Mapped(i..Mapped'Last), Pattern);
            -- 'Going' defaults to 'Forward', and 'Mapping' to 'Identity'
            -- propagates Pattern_Error if Pattern is null (as required)
         exit when i = 0;
         N := N+1;
         exit when i > Source'Last - Pattern'Length*2 + 1;
         i := i + Pattern'Length;
      end loop;
      return N;
   end Count;


   ----------
   function Count (Source: in String;
                   Set:    in Maps.Character_Set) return Natural is
      N: Natural := 0;
   begin
      for i in Source'Range loop
         if Is_In(Source(i), Set) then
            N := N+1;
         end if;
      end loop;
      return N;
   end Count;


   ----------
   function "not"(Right: Membership) return Membership is
   begin
      case Right is
         when Inside  => return Outside;
         when Outside => return Inside;
      end case;
   end "not";


   ----------
   procedure Find_Token (Source: in  String;
                         Set:    in  Maps.Character_Set;
                         Test:   in  Membership;
                         First:  out Positive;
                         Last:   out Natural) is
      i: Natural;
   begin
      i := Index(Source, Set, Test);
      if i = 0 then
         First := Source'First;
         Last := 0;
         return;
      else
         Last := Index(Source(i+1..Source'Last), Set, not Test);
         if Last = 0 then
            Last := Source'Last;
         end if;
      end if;
   end Find_Token;


   ----------
   -- String translation subprograms:

   function Translate (Source:  in String;
                       Mapping: in Maps.Character_Mapping) return String is
      Result: String(1..Source'Length);
   begin
      for i in Source'Range loop
         Result(i-Source'First+1) := Value(Source(i), Mapping);
      end loop;
      return Result;
   end Translate;


   ----------
   procedure Translate (Source:  in out String;
                        Mapping: in     Maps.Character_Mapping) is
   begin
      for i in Source'Range loop
         Source(i) := Value(Source(i), Mapping);
      end loop;
      return Result;
   end Translate;


   ----------
   function Translate (Source:  in String;
                       Mapping: in Maps.Character_Mapping_Function)
                                                           return String is
      Result: String(1..Source'Length);
   begin
      for i in Source'Range loop
         Result(i-Source'First+1) := Value(Source(i), Mapping);
      end loop;
      return Result;
   end Translate;


   ----------
   procedure Translate (Source:  in out String;
                        Mapping: in     Maps.Character_Mapping_Function) is
   begin
      for i in Source'Range loop
         Source(i) := Value(Source(i), Mapping);
      end loop;
      return Result;
   end Translate;


   ----------
   -- String transformation subprograms:

   function Replace_Slice (Source: in String;
                           Low:    in Positive;
                           High:   in Natural;
                           By:     in String) return String is
   begin
      if Low < Source'First or High > Source'Last then
         raise Index_Error;
      end if;
      if High < Low then
         return Insert(Source, Before=>Low, New_Item=>By);
      else
         declare
            subtype Return_Subtype is
            String(1..Source'Length-(High-Low+1)+By'Length);
         begin
            return Return_Subtype'(Source(Source'First..Low–1) & By &
                                              Source(High+1..Source'Last));
         end;
      end if;
   end Replace_Slice;


   ----------
   procedure Replace_Slice (Source:  in out String;
                            Low:     in     Positive;
                            High:    in     Natural;
                            By:      in     String;
                            Drop:    in     Truncation := Error;
                            Justify: in     Alignment  := Left;
                            Pad:     in     Character  := Space) is
   begin
      Move(Replace_Slice(Source, Low, High, By),
           Source,
           Drop,
           Justify,
           Pad);
   end;


   ----------
   function Insert (Source:   in String;
                    Before:   in Positive;
                    New_Item: in String) return String is
   begin
      if Before < Source'First or Before-1 > Source'Last then
         raise Index_Error;
      end if;
      declare
        subtype Return_Subtype is String(1..Source'Length+New_Item'Length);
      begin
         return Result_Subtype'(Source(Source'First..Before–1) & New_Item &
                                              Source(Before..Source'Last));
      end;
   end Insert;


   ----------
   procedure Insert (Source:   in out String;
                     Before:   in     Positive;
                     New_Item: in     String;
                     Drop:     in     Truncation := Error) is
   begin
      Move(Insert(Source, Before, New_Item), Source, Drop);
   end;


   ----------
   function Overwrite (Source:   in String;
                       Position: in Positive;
                       New_Item: in String) return String is
   begin
      if Position not in Source'First.. Source'Last+1 then
         raise Index_Error;
      end if;
      declare
         subtype Return_Subtype is
           String(1..Integer'Max(Position+New_Item'Length-1, Source'Last));
      begin
         return Return_Subtype'(Source(Source'First..Position-1) &
                 New_Item & Source(Position+New_Item'Length..Source'Last));
   end Overwrite;


   ----------
   procedure Overwrite (Source:   in out String;
                        Position: in     Positive;
                        New_Item: in     String;
                        Drop:     in     Truncation := Right) is
   begin
      Move(Overwrite(Source, Position, New_Item), Source, Drop);
   end;


   ----------
   function Delete (Source:  in String;
                    From:    in Positive;
                    Through: in Natural) return String is
   begin
      if From <= Through then
         declare
            subtype Return_Subtype is
                                 String(1..Source'Length-(Through-From+1));
         begin
            return Source(Source'First..From-1) &
                                            Source(Through+1..Source'Last);
         end;
      else
         declare
            subtype Return_Subtype is String(1..Source'Length);
         begin
            return Return_Subtype'(Source);
         end;
      end if;
   end Delete;


   ----------
   procedure Delete (Source:  in out String;
                     From:    in     Positive;
                     Through: in     Natural;
                     Justify: in     Alignment := Left;
                     Pad:     in     Character := Space) is
   begin
      Move(Delete(Source, From, Through),
           Source,
           Justify => Justify,
           Pad => Pad);
   end Delete;


   ----------
   -- String selector subprograms:

   function Trim (Source: in String;
                  Side:   in Trim_End) return String is
      i: Positive := Source'First;
      j: Natural  := Source'Last;
   begin
      if Side=Left or Side=Both then
         while i<j and then Source(i) = Space loop
            i := i+1;
         end loop;
         if i=j and then Source(i) = Space then
            return "";
         end if;
      end if;
      if Side=Right or Side=Both then
         while j >= i and then Source(j) = Space loop
            j := j-1;
         end loop;
      end if;
      declare
         subtype Return_Subtype is String(1..(j-i+1));
      begin
         return Return_Subtype'(Source(i..j));
      end;
   end Trim;               


   ----------
   procedure Trim (Source:  in out String;
                   Side:    in     Trim_End;
                   Justify: in     Alignment: = Left;
                   Pad:     in     Character := Space) is
   begin
      Move(Trim(Source, Side), Source, Justify=>Justify, Pad=>Pad);
   end;


   ----------
   function Trim (Source: in String;
                  Left:   in Maps.Character_Set;
                  Right:  in Maps.Character_Set) return String is
      i: Positive := Source'First;
      j: Natural  := Source'Last;
   begin
      while i<j and then Is_In(Source(i), Left) loop
         i := i+1;
      end loop;
      if i=j and then Is_In(Source(i), Left) then
         return "";
      end if;
      while j >= i and then Is_In(Source(j), Right) loop
         j := j-1;
      end loop;
      declare
         subtype Return_Subtype is String(1..(j-i+1));
      begin
         return Return_Subtype'(Source(i..j));
      end;
   end Trim;               


   ----------
   procedure Trim (Source:  in out String;
                   Left:    in     Maps.Character_Set;
                   Right:   in     Maps.Character_Set;
                   Justify: in     Alignment := Strings.Left;
                   Pad:     in     Character := Space) is
   begin
      Move(Trim(Source, Left, Right),
           Source,
           Justify => Justify,
           Pad=>Pad);
   end Trim;


   ----------
   function Head (Source: in String;
                  Count:  in Natural;
                  Pad:    in Character: = Space) return String is
   begin
      declare
         subtype Return_Subtype is String(1..Count);
      begin
         if Count <= Source'Length then
            return
               Return_Subtype'(Source(Source'First..Source'First-1+Count));
         else
            return Return_Subtype'(Source & (Count-Source'Length)*Pad);
         end if;
      end;
   end Head;


   ----------
   procedure Head (Source:  in out String;
                   Count:   in     Natural;
                   Justify: in     Alignment: = Left;
                   Pad:     in     Character: = Space) is
   begin
      Move(Head(Source, Count, Pad),
           Source,
           Drop=>Error,
           Justify=>Justify,
           Pad=>Pad);
   end;


   ----------
   function Tail (Source: in String;
                  Count:  in Natural;
                  Pad:    in Character: = Space) return String is
   begin
      declare
         subtype Return_Subtype is String(1..Count);
      begin
         if Count <= Source'Length then
            return
                 Return_Subtype'(Source(Source'Last-Count+1..Source'Last));
         else
            return Return_Subtype'((Count-Source'Length)*Pad & Source);
         end if;
      end;
   end Tail;


   ----------
   procedure Tail (Source:  in out String;
                   Count:   in     Natural;
                   Justify: in     Alignment := Left;
                   Pad:     in     Character := Space) is
   begin
      Move(Tail(Source, Count, Pad),
           Source,
           Drop=>Error,
           Justify=>Justify,
           Pad=>Pad);
   end Tail;


   ----------
   -- String constructor functions:

   function "*" (Left:  in Natural;
                 Right: in Character) return String is
      Result: String(1..Left);
   begin
      for i in Result'Range loop
         Result(i) := Right;
      end loop;
      return Result;
   end "*";


   ----------
   function "*" (Left:  in Natural;
                 Right: in String) return String is
      Result: String(1..Left*Right'Length);
      P: Positive := 1;
   begin
      for i in 1..Left loop
         Result(P..P-1+Right'Length) := Right;
         P := P+1;
      end loop;
      return Result;
   end "*";

end Ada.Strings.Fixed;



--------------------
-- End of File.

