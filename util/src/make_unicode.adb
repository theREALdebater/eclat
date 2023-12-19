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
-- As a special exception, if other files instantiate generics from this unit, or you link 
-- this unit with other files to produce an executable, this unit does not by itself cause 
-- the resulting executable to be covered by the GNU General Public License. This exception 
-- does not however invalidate any other reasons why the executable file might be covered by 
-- the GNU General Public License. 
-- 
-----------------------------------------------------------------------------------------------

pragma License(Modified_GPL);

--/ Unicode categorisation package generator

--| This procedure generates the specification and body of the package 
--| `ECLAT.Characters.Handling` using the `UnicodeData.txt` file, which can be 
--| downloaded from <ftp://ftp.unicode.org/Public/UNIDATA/UnicodeData.txt>

with Ada.Text_IO;
with Ada.

procedure Make_Unicode is

   type General_Category is (...);
   
   type General_Category_Short_Name is String(1..2);
   
   Gen_Cat_Label: constant array (General_Category) of General_Category_Short_Name :=
      (  => "",
         => "",
         => "",
         => "",
         => "",
         => "",
         => "",
         => "",
         => "",
         => "",
         => "",
         => "",
         => "",
         => "",
         => "",
         => "",
         => "",
         => "",
         => "",
         => "",
         => "",
         => "",
         => "",
         => "",
         => "");

--| The fields of the file are separated by `;` semicolon and are:

--| *  0 	Code value 	normative 	Code value in 4-digit hexadecimal format.
--| *  1 	Character name 	normative 	These names match exactly the names published in Chapter 14 of the Unicode Standard, Version 3.0.
--| *  2 	General Category 	normative / informative
--| *  (see below) 	This is a useful breakdown into various "character types" which can be used as a default categorization in implementations. See below for a brief explanation.
--| *  3 	Canonical Combining Classes 	normative 	The classes used for the Canonical Ordering Algorithm in the Unicode Standard. These classes are also printed in Chapter 4 of the Unicode Standard.
--| *  4 	Bidirectional Category 	normative 	See the list below for an explanation of the abbreviations used in this field. These are the categories required by the Bidirectional Behavior Algorithm in the Unicode Standard. These categories are summarized in Chapter 3 of the Unicode Standard.
--| *  5 	Character Decomposition Mapping 	normative 	In the Unicode Standard, not all of the mappings are full (maximal) decompositions. Recursive application of look-up for decompositions will, in all cases, lead to a maximal decomposition. The decomposition mappings match exactly the decomposition mappings published with the character names in the Unicode Standard.
--| *  6 	Decimal digit value 	normative 	This is a numeric field. If the character has the decimal digit attribute, as specified in Chapter 4 of the Unicode Standard, the value of that digit is represented with an integer value in this field
--| *  7 	Digit value 	normative 	This is a numeric field. If the character represents a digit, not necessarily a decimal digit, the value is here. This covers digits which do not form decimal radix forms, such as the compatibility superscript digits
--| *  8 	Numeric value 	normative 	This is a numeric field. If the character has the numeric attribute, as specified in Chapter 4 of the Unicode Standard, the value of that character is represented with an integer or rational number in this field. This includes fractions as, e.g., "1/5" for U+2155 VULGAR FRACTION ONE FIFTH Also included are numerical values for compatibility characters such as circled numbers.
--| *  9 	Mirrored 	normative 	If the character has been identified as a "mirrored" character in bidirectional text, this field has the value "Y"; otherwise "N". The list of mirrored characters is also printed in Chapter 4 of the Unicode Standard.
--| * 10 	Unicode 1.0 Name 	informative 	This is the old name as published in Unicode 1.0. This name is only provided when it is significantly different from the Unicode 3.0 name for the character.
--| * 11 	10646 comment field 	informative 	This is the ISO 10646 comment field. It is in parantheses in the 10646 names list.
--| * 12 	Uppercase Mapping 	informative 	Upper case equivalent mapping. If a character is part of an alphabet with case distinctions, and has an upper case equivalent, then the upper case equivalent is in this field. See the explanation below on case distinctions. These mappings are always one-to-one, not one-to-many or many-to-one. This field is informative.
--| * 13 	Lowercase Mapping 	informative 	Similar to Uppercase mapping
--| * 14 	Titlecase Mapping 	informative 	Similar to Uppercase mapping

   type Codepoint_Info is
      record
         Char: Wide_Wide_Character;
         Name: Unbounded_String;
         Cat:  General_Category;
         
         
         
      end record;

   Datafile: File_Type; -- input
   Adafile:  File_Type; -- output
   
begin
   Open (
   










end;

--\
-----------------------------------------------------------------------------------------------

