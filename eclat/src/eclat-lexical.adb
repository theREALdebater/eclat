-----------------------------------------------------------------------------------------------
-- 
-- Copyright (C) 2019 The AdaOS Project
-- 
-- This file is part of ECLAT.
-- 
-- ECLAT is free software: you can redistribute it and/or modify it under the terms of the GNU 
-- General Public License as published by the Free Software Foundation, either version 3 of 
-- the License, or (at your option) any later version. 
-- 
-- ECLAT is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without 
-- even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
-- GNU General Public License for more details. 
-- 
-- You should have received a copy of the GNU General Public License along with ECLAT.  If 
-- not, see <http://www.gnu.org/licenses/>. 
-- 
-----------------------------------------------------------------------------------------------

--/ Lexical Analysis (implementation)

with Ada.Text_IO;
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;

use Ada.Text_IO;
use Ada.Characters.Handling;
with Ada.Characters.Latin_1;

package body ECLAT.Lexical
is

   --/ Character categories
   
   --| RM 2.1:

   --| * letter_uppercase
   --|   Any character whose General Category is defined to be “Letter, Uppercase”.
   --| * letter_lowercase
   --|   Any character whose General Category is defined to be “Letter, Lowercase”.
   --| * letter_titlecase
   --|   Any character whose General Category is defined to be “Letter, Titlecase”.
   --| * letter_modifier
   --|   Any character whose General Category is defined to be “Letter, Modifier”.
   --| * letter_other
   --|   Any character whose General Category is defined to be “Letter, Other”.
   --| * mark_non_spacing
   --|   Any character whose General Category is defined to be “Mark, Non-Spacing”.
   --| * mark_spacing_combining
   --|   Any character whose General Category is defined to be “Mark, Spacing Combining”.
   --| * number_decimal
   --|   Any character whose General Category is defined to be “Number, Decimal”.
   --| * number_letter
   --|   Any character whose General Category is defined to be “Number, Letter”.
   --| * punctuation_connector
   --|   Any character whose General Category is defined to be “Punctuation, Connector”.
   --| * other_format
   --|   Any character whose General Category is defined to be “Other, Format”.
   --| * separator_space
   --|   Any character whose General Category is defined to be “Separator, Space”.
   --| * separator_line
   --|   Any character whose General Category is defined to be “Separator, Line”.
   --| * separator_paragraph
   --|   Any character whose General Category is defined to be “Separator, Paragraph”.
   --| * format_effector
   --|   The characters whose code points are 16#09# (CHARACTER TABULATION), 16#0A#
   --|   (LINE FEED), 16#0B# (LINE TABULATION), 16#0C# (FORM FEED), 16#0D#
   --|   (CARRIAGE RETURN), 16#85# (NEXT LINE), and the characters in categories
   --|   separator_line and separator_paragraph .
   --| * other_control
   --|   Any character whose General Category is defined to be “Other, Control”, and which is not
   --|   defined to be a format_effector .
   --| * other_private_use
   --|   Any character whose General Category is defined to be “Other, Private Use”.
   --| * other_surrogate
   --|   Any character whose General Category is defined to be “Other, Surrogate”.
   --| * graphic_character
   --|   Any character that is not in the categories other_control, other_private_use,
   --|   other_surrogate, format_effector, and whose relative code point in its plane is neither
   --|   16#FFFE# nor 16#FFFF#.

   type Character_Category is (Unknown,
                               letter_uppercase,
                               letter_lowercase,
                               letter_titlecase,
                               letter_modifier,
                               letter_other,
                               mark_non_spacing,
                               mark_spacing_combining,
                               number_decimal,
                               number_letter,
                               punctuation_connector,
                               other_format,
                               separator_space,
                               separator_line,
                               separator_paragraph,
                               format_effector,
                               other_control,
                               other_private_use,
                               other_surrogate,
                               graphic_character);
   
   function Category (Char: in Character) return Character_Category
   is
   begin
      if Is_Lower(Char) then return letter_lowercase; end if;
      if Is_Upper(Char) then return letter_uppercase; end if;
      if Is_Digit(Char) then return number_decimal; end if;
      if Char = HT then return format_effector; end if; -- 16#09# (CHARACTER TABULATION)
      if Char = LF then return format_effector; end if; -- 16#0A# (LINE FEED)
      if Char = VT then return format_effector; end if; -- 16#0B# (LINE TABULATION)
      if Char = FF then return format_effector; end if; -- 16#0C# (FORM FEED)
      if Char = CR then return format_effector; end if; -- 16#0D# (CARRIAGE RETURN)
      if Char = NEL then return format_effector; end if; -- 16#85# (NEXT LINE)
      return Unknown;
   end;

   --\
   --/ Delimiters:
   
   --| RM 2.1
   
   









   --\
   --/ 





   --\
   --/ 




   --\
   --/ Parse a source text (file) into a list of lexical elements
   
   --| ...

   procedure Open (Parser: in out Lexical_Parser;
                   File:   in out Ada.Text_IO.File_Type)
   is
   begin
      ...
   end;
                   
   procedure Close (Parser: in out Lexical_Parser)
   is
   begin
      ...
   end;
   
   function File_Lexical_Parser (Source: in Source_Descriptor) return Lexical_Parser
   is
   begin
      Location.Source := new Source_Reference'(Source);
   end;

   function Input (Parser: in out Lexical_Parser) return Lexical_Element'Class
   is
   
      procedure Fetch (Count: in Buffer_Count := 1)
      is
         N: Buffer_Count renames Parser.Filled;
      begin
         if N + Count > Buffer_Limit then raise Program_Error; end if;
         for i in 1..Count loop
            N := N + 1;
            if Ada.Text_IO.End_Of_File(Parser.File) then
               Parser.Buffer(N) := Latin_1.Nul;
               Parser.At_End_Of_Line := True;
               
            Ada.Text_IO.Get (Parser.File, Parser.Buffer(N));
         end loop;
      end;
      
      procedure Discard (Count: in Buffer_Count := 1)
      is
         N: Buffer_Count renames Parser.Filled;
      begin
         if N < Count then raise Program_Error; end if;
         N := N - Count;
         Parser.Buffer(1..N-Count) := Parser.Buffer(Count+1..N)
      end;
   
      function Current return Character 
      is
      begin
         if Parser.Filled < 1 then Fetch; end if;
         return Parser.Buffer(1);
      end;
      
      subtype Lookahead_Range is Buffer_Index range 1 .. Buffer_Index'Last - 1;
   
      function Lookahead (N: Lookahead_Range := 1) return Character 
      is
      begin
         if Parser.Filled < N+1 then Fetch; end if;
         return Parser.Buffer(N+1);
      end;
      
      -- function At_End_Of_Line return Boolean
         -- is ( Current = Latin_1.LF 
         
      procedure Discard_End_Of_Line
      is
      begin
         if Current = Latin_1.LF and Lookahead = 
      
   begin
      
   
   
         case Current 
         is
            when '&' | '''' | '(' | ')' | '*' |
                 '+' | '-' | '.' | '/' | ':',
                 ';' | '<' | '=' | '>' '|' => 
               Discard;
               return new Lexical_Element'( Delimiter, Ada_Delimiter(Current) );
               
            when '-' => 
               case Lookahead is 
                  when '-' => 
                     Discard(2); 
                     while not Parser.End_Of_File and not End_Of_Line loop
                        Discard;
                     end loop;
                     Discard_End_Of_Line;
                     return 
                        
                  when others => 
                     Discard; 
                     return new Lexical_Element'( Delimiter, Ada_Delimiter(Current) );
               end case;
            
            when '' => 
               case Lookahead = '' is 
                  when '' => Discard(2); return new Lexical_Element'(Delimiter, ); 
                  when others => <error>; 
               end case;
            
            when '' => 
               case Lookahead = '' is 
                  when '' => Discard(2); return new Lexical_Element'(Delimiter, ); 
                  when others => <error>; 
               end case;
            
            
            when '' => Advance (2, new Delimiter_Element'());

            when others =>
               


=> arrow
.. double dot
** double star, exponentiate
:= assignment (pronounced: “becomes”)
/= inequality (pronounced: “not equal”)
>= greater than or equal
<= less than or equal
<< left label bracket
>> right label bracket
<> box








   --\
   
   
   
   
   
   


end ECLAT.Lexical;

--\
-----------------------------------------------------------------------------------------------

