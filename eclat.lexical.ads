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

--/ Lexical Analysis (specification)

with Ada.Text_IO;

package ECLAT.Lexical is

   type Ada_Reserved_Word is (

RW_,
RW_,
RW_,
RW_,
RW_,

RW_,
RW_,
RW_,
RW_,
RW_,

RW_,
RW_,
RW_,
RW_,
RW_,

RW_,
RW_,
RW_,
RW_,
RW_,

RW_,
RW_,
RW_,
RW_,
RW_,

RW_,
RW_,
RW_,
RW_,
RW_,

RW_,
RW_,
RW_,
RW_,
RW_,

RW_,
RW_,
RW_,
RW_,
RW_,

);


   type Ada_Delimiter is ('&', '''', '(', ')', '*',
                          '+', '-', '.', '/', ':',
                          ';', '<', '=', '>' '|',
                          arrow
                          double_dot,
                          double_star,
                          assignment,
                          inequality,
                          greater_than_or_equal,
                          less_than_or_equal,
                          left_label_bracket,
                          right_label_bracket,
                          box);


   type Lexical_Category is (Delimiter,
                             Reserved_Word,
                             Identifer,
                             

   type Lexical_Element (Cat: Lexical_Category) is
      record
         case Cat is
            when Delimiter       => Del: Ada_Delimiter;
            when Reserved_Word   => RW:  Ada_Reserved_Word;
            when Identifier      => ID:  String_Token;
            




   procedure Parse (File: in out Ada.Text_IO.File_Type;
                    List: out





