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

--/ Ada.Streams

with System;

--| Specification for the standard Ada package `Ada.Streams`.
--| 
--| Please see the accompanying documentation.

package Ada.Streams is

   pragma Pure (Streams);

   type Root_Stream_Type is abstract tagged limited private;

   pragma Preelaborable_Initialization (Root_Stream_Type);

   type Stream_Element is mod System.???;
   type Stream_Element_Offset is range System.???;

   subtype Stream_Element_Count is
      Stream_Element_Offset range 0 .. Stream_Element_Offset'Last;

   type Stream_Element_Array is
      array(Stream_Element_Offset range <>) of aliased Stream_Element;

   procedure Read (Stream: in out Root_Stream_Type;
                   Item:   out    Stream_Element_Array;
                   Last:   out    Stream_Element_Offset) is abstract;

   procedure Write(
   Stream : in out Root_Stream_Type;
   Item : in Stream_Element_Array) is abstract;

private
... -- not specified by the language
end Ada.Streams;
--\
-----------------------------------------------------------------------------------------------

