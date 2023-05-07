-----------------------------------------------------------------------------------------------
-- 
-- Copyright (C) 2022 The AdaOS Project
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
--/ Ada package specification

--| See RM E.5.

with Ada.Streams;
with Ada.Containers.Vectors;

package System.RPC
with 
    Nonblocking => False, 
    Global => in out synchronized 
is
   type Partition_Id is range 0 .. implementation-defined;

   Communication_Error : exception;

   type Params_Stream_Type (
      Initial_Size : Ada.Streams.Stream_Element_Count) is new
      Ada.Streams.Root_Stream_Type with private;

   procedure Read(
      Stream : in out Params_Stream_Type;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);

   procedure Write(
      Stream : in out Params_Stream_Type;
      Item : in Ada.Streams.Stream_Element_Array);

   -- Synchronous call
   procedure Do_RPC(
      Partition  : in Partition_Id;
      Params     : access Params_Stream_Type;
      Result     : access Params_Stream_Type);

   -- Asynchronous call
   procedure Do_APC(
      Partition  : in Partition_Id;
      Params     : access Params_Stream_Type);

   -- The handler for incoming RPCs
   type RPC_Receiver is access procedure(
      Params     : access Params_Stream_Type;
      Result     : access Params_Stream_Type);

   procedure Establish_RPC_Receiver(
      Partition : in Partition_Id;
      Receiver  : in RPC_Receiver);

private

   type Element_Vectors is 
      new Ada.Containers.Vectors (Ada.Streams.Stream_Element_Offset, Ada.Streams.Stream_Element); 

   type Params_Stream_Type (Initial_Size: Ada.Streams.Stream_Element_Count) 
   is 
      new Ada.Streams.Root_Stream_Type 
   with
      record
         Elements: Element_Vectors.Vector (0 .. Initial_Size - 1);
         Length:   Ada.Streams.Stream_Element_Count := 0;
         Next:     Ada.Streams.Stream_Element_Offset := 0;
      end record;

end System.RPC;

--\
-----------------------------------------------------------------------------------------------
-- End of file

