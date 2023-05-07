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

--/ Identifier Tokenisation (implementation)

package body ECLAT.Tokens is

   function Exists (Map: in Token_Map; Image: in String) return Boolean
   is
   begin
      Validate_Image (Image);
      return Map.By_Image.Contains(Image);
   end;
   
   function Tokenize (Map: in out Token_Map; Image: in String) return Identifier_Token
   is
      Token: Identifier_Token;
      Preexisted: Boolean; -- not used but needed to received an out-value
   begin
      Validate_Image (Image);
      Tokenize (Map, Image, Token, Preexisted);
      return Token;
   end;
   
   function To_String (Map: in Token_Map; Token: in Identifier_Token) return String
   is
      Cursor: Token_Image_Maps.Cursor;
   begin
      Cursor := Map.By_Token.Contains(Token);
      if Cursor = No_Element then
         raise Token_Error;
      end if;
      return To_String( Map.By_Token.Element(Cursor).Image );
   end;
   
   procedure Tokenize (Map:        in  Token_Map;
                       Image:      in  String; 
                       Token:      out Identifier_Token; 
                       Preexisted: out Boolean)
   is
      Cursor: Image_Token_Maps.Cursor;
      Pair: Token_Image_Pair;
   begin
      Validate_Image (Image);
      Cursor := Map.By_Image.Contains(Image);
      Preexisted := Cursor /= No_Element;
      if Preexisted then
         Token := Map.By_Image.Element(Cursor).Token;
      else
         Map.Last_Token := Map.Last_Token + 1;
         Token := Map.Last_Token;
         Pair.Token := Token;
         Pair.Image := To_Unbounded_String(Image);
         Map.By_Token.Insert (Token, Pair);
         Map.By_Image.Insert (Image, Pair);
      end if;
   end;
   
   function Is_Image_Valid (Image: in String) 
      return Boolean
      is ( Image'Length > 0 );
      
   procedure Validate_Image (Image: in String) 
   is
   begin
      if not Is_Image_Valid (Image) then
         raise Token_Image_Error;
      end if;
   end;

end;

--\
-----------------------------------------------------------------------------------------------

