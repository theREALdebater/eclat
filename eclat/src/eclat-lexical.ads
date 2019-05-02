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

--/ Lexical Analysis (specification)

--| 
--| 
--| 
--| 
--| 
--| 
--| 
--| 
--| 

with Ada.Text_IO;
with ECLAT.Tokens;
private with Ada.Finalization.Limited_Controlled;
private with Ada.Characters.Latin_1;

package ECLAT.Lexical is

   --/ Lexical Parser
   
   --| ...
   
   type Lexical_Parser is tagged with private;
   
   Tokens: ECLAT.Tokens.Map;

   --\
   --/ Reserved Words
   
   --| RM 2.9

   type Ada_Reserved_Word is (rw_abort,
                              rw_abs,
                              rw_abstract,
                              rw_accept,
                              rw_access,
                              rw_aliased,
                              rw_all,
                              rw_and,
                              rw_array,
                              rw_at,
                              rw_begin,
                              rw_body,
                              rw_case,
                              rw_constant,
                              rw_declare,
                              rw_delay,
                              rw_delta,
                              rw_digits,
                              rw_do,
                              rw_else,
                              rw_elsif,
                              rw_end,
                              rw_entry,
                              rw_exception,
                              rw_exit,
                              rw_for,
                              rw_function,
                              rw_generic,
                              rw_goto,
                              rw_if,
                              rw_in,
                              rw_interface,
                              rw_is,
                              rw_limited,
                              rw_loop,
                              rw_mod,
                              rw_new,
                              rw_not,
                              rw_null,
                              rw_of,
                              rw_or,
                              rw_others,
                              rw_out,
                              rw_overriding,
                              rw_package,
                              rw_pragma,
                              rw_private,
                              rw_procedure,
                              rw_protected,
                              rw_raise,
                              rw_range,
                              rw_record,
                              rw_rem,
                              rw_renames,
                              rw_requeue,
                              rw_return,
                              rw_reverse,
                              rw_select,
                              rw_separate,
                              rw_some,
                              rw_subtype,
                              rw_synchronized,
                              rw_tagged,
                              rw_task,
                              rw_terminate,
                              rw_then,
                              rw_type,
                              rw_until,
                              rw_use,
                              rw_when,
                              rw_while,
                              rw_with,
                              rw_xor);

   Reserved_Word_Tokens: 
      constant array (Positive range <>) of Identifier_Token :=
      ( rw_abort        => Tokenize(Tokens, "abort");
        rw_abs          => Tokenize(Tokens, "abs");
        rw_abstract     => Tokenize(Tokens, "abstract");
        rw_accept       => Tokenize(Tokens, "accept");
        rw_access       => Tokenize(Tokens, "access");
        rw_aliased      => Tokenize(Tokens, "aliased");
        rw_all          => Tokenize(Tokens, "all");
        rw_and          => Tokenize(Tokens, "and");
        rw_array        => Tokenize(Tokens, "array");
        rw_at           => Tokenize(Tokens, "at");
        rw_begin        => Tokenize(Tokens, "begin");
        rw_body         => Tokenize(Tokens, "body");
        rw_case         => Tokenize(Tokens, "case");
        rw_constant     => Tokenize(Tokens, "constant");
        rw_declare      => Tokenize(Tokens, "declare");
        rw_delay        => Tokenize(Tokens, "delay");
        rw_delta        => Tokenize(Tokens, "delta");
        rw_digits       => Tokenize(Tokens, "digits");
        rw_do           => Tokenize(Tokens, "do");
        rw_else         => Tokenize(Tokens, "else");
        rw_elsif        => Tokenize(Tokens, "elsif");
        rw_end          => Tokenize(Tokens, "end");
        rw_entry        => Tokenize(Tokens, "entry");
        rw_exception    => Tokenize(Tokens, "exception");
        rw_exit         => Tokenize(Tokens, "exit");
        rw_for          => Tokenize(Tokens, "for");
        rw_function     => Tokenize(Tokens, "function");
        rw_generic      => Tokenize(Tokens, "generic");
        rw_goto         => Tokenize(Tokens, "goto");
        rw_if           => Tokenize(Tokens, "if");
        rw_in           => Tokenize(Tokens, "in");
        rw_interface    => Tokenize(Tokens, "interface");
        rw_is           => Tokenize(Tokens, "is");
        rw_limited      => Tokenize(Tokens, "limited");
        rw_loop         => Tokenize(Tokens, "loop");
        rw_mod          => Tokenize(Tokens, "mod");
        rw_new          => Tokenize(Tokens, "new");
        rw_not          => Tokenize(Tokens, "not");
        rw_null         => Tokenize(Tokens, "null");
        rw_of           => Tokenize(Tokens, "of");
        rw_or           => Tokenize(Tokens, "or");
        rw_others       => Tokenize(Tokens, "others");
        rw_out          => Tokenize(Tokens, "out");
        rw_overriding   => Tokenize(Tokens, "overriding");
        rw_package      => Tokenize(Tokens, "package");
        rw_pragma       => Tokenize(Tokens, "pragma");
        rw_private      => Tokenize(Tokens, "private");
        rw_procedure    => Tokenize(Tokens, "procedure");
        rw_protected    => Tokenize(Tokens, "protected");
        rw_raise        => Tokenize(Tokens, "raise");
        rw_range        => Tokenize(Tokens, "range");
        rw_record       => Tokenize(Tokens, "record");
        rw_rem          => Tokenize(Tokens, "rem");
        rw_renames      => Tokenize(Tokens, "renames");
        rw_requeue      => Tokenize(Tokens, "requeue");
        rw_return       => Tokenize(Tokens, "return");
        rw_reverse      => Tokenize(Tokens, "reverse");
        rw_select       => Tokenize(Tokens, "select");
        rw_separate     => Tokenize(Tokens, "separate");
        rw_some         => Tokenize(Tokens, "some");
        rw_subtype      => Tokenize(Tokens, "subtype");
        rw_synchronized => Tokenize(Tokens, "synchronized");
        rw_tagged       => Tokenize(Tokens, "tagged");
        rw_task         => Tokenize(Tokens, "task");
        rw_terminate    => Tokenize(Tokens, "terminate");
        rw_then         => Tokenize(Tokens, "then");
        rw_type         => Tokenize(Tokens, "type");
        rw_until        => Tokenize(Tokens, "until");
        rw_use          => Tokenize(Tokens, "use");
        rw_when         => Tokenize(Tokens, "when");
        rw_while        => Tokenize(Tokens, "while");
        rw_with         => Tokenize(Tokens, "with");
        rw_xor          => Tokenize(Tokens, "xor") ); 

   --\
   --/ Delimiters
   
   --| RM 

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



   --\
   --/ Source Descriptors
   
   type Source_Descriptor is
      record
         Moniker: Unbounded_String; -- used in error messages
         Name:    Unbounded_String; -- used to actually open the file
      end record;

   type Source_Reference is access Source_Descriptor;

   --\
   --/ Locations
   
   type Parsing_Location is
      record
         Col:    Ada.Text_IO.Count_Type;
         Line:   Ada.Text_IO.Count_Type;
         Page:   Ada.Text_IO.Count_Type;
         Source: Source_Reference;
      end record;
      
   type Element_Location is
      record
         First, Last: Parsing_Location;
      end record;
   
   --\
   --/ Lexical Elements

   type Lexical_Category is (Delimiter,
                             Reserved_Word,
                             Identifer,
                             ...
                             Error
                             ...
                             );
                             

   type Lexical_Element (Cat: Lexical_Category) is
      record

         case Cat is
            when Delimiter       => Delim: Ada_Delimiter;
            when Reserved_Word   => RW:    Ada_Reserved_Word;
            when Identifier      => ID:    Identifier_Token;
            ...
            when Error           => Msg: Unbounded_String;
         end case;

         Location: Element_Location;
         
      end record;



   function File_Lexical_Parser (Source: in Source_Descriptor) return Lexical_Parser;

   procedure Open (Parser: in out Lexical_Parser;
                   File:   in out Ada.Text_IO.File_Type);
                   
   function End_Of_File (Parser: in Lexical_Parser) return Boolean;

   function Input (Parser: in out Lexical_Parser) return Lexical_Element;
   
   procedure Close (Parser: in out Lexical_Parser);

private

   use Ada.Characters;

   --| ...
   
   --| 'Buffer(1)' contains the character that has just been 'consumed' by the parser. It 
   --| also contains the next character (index 2) and the character after that (index 3) 
   --| just about to be consumed.
   
   --| `Filled` contains how many characters are actually in the `Buffer`. Only
   --| `Buffer(1..Filled)` will be valid.
   
   type Character_Status is (Normal, At_Line_End, At_Page_End, At_File_End);
   
   Buffer_Capacity: constant := 4;
   
   type Buffer_Index is mod Buffer_Capacity;
   subtype Buffer_Offset is Buffer_Index;
   subtype Buffer_Count is Integer range 0 .. Buffer_Limit;
   
   type Buffer_Element is
      record
         Char:   Character := Latin_1.NUL;
         Status: Character_Status := At_File_End;
      end record;

   type Lexical_Parser is Ada.Finalization.Limited_Controlled with
      record      
         Source:   Source_Reference;
         Tokens:   ECLAT.Tokens.Map;
         File:     Ada.Text_IO.File_Type;
         Is_Open:  Boolean := False;
         Buffer:   array (Buffer_Index) of Buffer_Element;
         Offset:   Buffer_Offset := 0;
         Filled:   Buffer_Count := 0;
         Location: Parsing_Location := (1, 1, 1, null);         
      end record;
      
   overriding
   procedure Finalize (Parser: in out Lexical_Parser)
   is
   begin
      if Parser.Is_Open then
         Ada.Text_IO.Close (Parser.File);
      end if;
   end;
   
end;

--\
-----------------------------------------------------------------------------------------------

