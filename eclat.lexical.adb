-- ECLAT (Experimental Compiler Library And Tools)
-- Copyright (C) 2019 Nicholas James Roberts (Reigate, UK)

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

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

   procedure Parse (...)
   
   
   
   
   is
   
   
   
   
         case Char is
            when '&' | '''' | '(' | ')' | '*' |
                 '+' | '-' | '.' | '/' | ':',
                 ';' | '<' | '=' | '>' '|' => 
               Advance (1, new Delimiter_Element'(Char));
               
            
'&', '''', '(', ')', '*',
                               '+', '-', '.', '/', ':',
                               ';', '<', '=', '>' '|',
            
            
            
            when '' => 
               case Lookahead_Char(1) = '' is 
                  when '' => Advance(1, new Delimiter_Element'()); 
                  when others => <error>; 
               end case;
            
            when '' => 
               case Lookahead_Char(1) = '' is 
                  when '' => Advance(1, new Delimiter_Element'()); 
                  when others => <error>; 
               end case;
            
            when '' => 
               case Lookahead_Char(1) = '' is 
                  when '' => Advance(1, new Delimiter_Element'()); 
                  when others => <error>; 
               end case;
            
            when '' => 
               case Lookahead_Char(1) = '' is 
                  when '' => Advance(1, new Delimiter_Element'()); 
                  when others => <error>; 
               end case;
            
            when '' => 
               case Lookahead_Char(1) = '' is 
                  when '' => Advance(1, new Delimiter_Element'()); 
                  when others => <error>; 
               end case;
            
            when '' => 
               case Lookahead_Char(1) = '' is 
                  when '' => Advance(1, new Delimiter_Element'()); 
                  when others => <error>; 
               end case;
            
            when '' => 
               case Lookahead_Char(1) = '' is 
                  when '' => Advance(1, new Delimiter_Element'()); 
                  when others => <error>; 
               end case;
            
            when '' => 
               case Lookahead_Char(1) = '' is 
                  when '' => Advance(1, new Delimiter_Element'()); 
                  when others => <error>; 
               end case;
            
            when '' => 
               case Lookahead_Char(1) = '' is 
                  when '' => Advance(1, new Delimiter_Element'()); 
                  when others => <error>; 
               end case;
            
            when others =>
               
            when '' => Advance (2, new Delimiter_Element'());



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