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

--/ Identifier Tokenisation (specification)

private with Ada.Strings.Hash;

package ECLAT.Tokens is

   --| An `Identifier_Token` is a small opaque value that uniquely represents an identifier.

   type Identifier_Token is private;
   
   --| A `Null_Identifier` is a value (the only one) which represents 'no identifier'.
   
   Null_Identifier: constant Identifier_Token;
   
   --| A `Token_Map` contains a set of mappings from a token value to its image (string
   --| containing the characters of the identifier).

   type Token_Map is tagged private;
   
   --| The function `Exists` return `True` if the token for a given `Image` is already in
   --| the given `Map`.

   function Exists (Map: in Token_Map; Image: in String) return Boolean;
   
   --| The function `Tokenize` adds a token with the given `Image` to the given `Map`,
   --| if it doesn't already exist, and (in either case) returns the token value.
   --| Propagates the exception `Token_Image_Error` if the given `Image` is empty.

   function Tokenize (Map: in out Token_Map; Image: in String) return Identifier_Token;
   
   --| The function `To_String` returns the image of a given token value.
   --| Propagates the exception `Toekn_Error` if the token value does not exist.
   
   function To_String (Map: in Token_Map; Token: in Identifier_Token) return String;
   
   --| The procedure `Tokenize` adds a token with the given `Image` to the given `Map`,
   --| if it doesn't already exist, and (in either case) returns the token value in
   --| `Token`. If if it did previously exist, return `True` in `Preexisted`.
   --| Propagates the exception `Token_Image_Error` if the given `Image` is empty.

   procedure Tokenize (Map:        in  Token_Map;
                       Image:      in  String; 
                       Token:      out Identifier_Token; 
                       Preexisted: out Boolean);

   Token_Error, Token_Image_Error: exception;
   
private

      type Identifier_Token is
         record
            Value: Integer;
         end record;
         
      Null_Identifier: constant Identifier_Token := 0;
      
      type Token_Image_Pair is
         record
            Token: Identifier_Token;
            Image: Unbounded_String;
         end record;
         
      function Token_Hash (Token: in Identifier_Token) 
         return Ada.Containers.Hash_Type
         is ( Ada.Containers.Hash_Type(Token) );
         
      function Image_Hash (Image: in String) 
         return Ada.Containers.Hash_Type 
         renames Ada.Strings.Hash_Case_Insensitive;
         
      function Equivalent_Keys (Left, Right: in Identifier_Token) 
         return Boolean 
         is ( Left = Right );

      function Equivalent_Keys (Left, Right: in String) 
         return Boolean 
         renames Ada.Strings.Equal_Case_Insensitive;

      package Token_Image_Maps is 
         new Ada.Containers.Hashed_Maps (Identifier_Token, 
                                         Token_Image_Pair, 
                                         Token_Hash, 
                                         Equivalent_Keys);

      package Image_Token_Maps is 
         new Ada.Containers.Hashed_Maps (String, 
                                         Token_Image_Pair, 
                                         Image_Hash, 
                                         Equivalent_Keys);

      type Token_Map is tagged
         record
            By_Token: Token_Image_Maps.Map;
            By_Image: Image_Token_Maps.Map;
            Last_Token: Identifier_Token := 0;
         end record;

end;

--\
-----------------------------------------------------------------------------------------------

