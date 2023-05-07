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

package body System.RPC
is
   subtype Count is Ada.Streams.Stream_Element_Count;

   type Operation_Id is 





   procedure Read(
      Stream : in out Params_Stream_Type;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
      Remaining: Count := Stream.Length - Stream.Next;
      N: Count := (if Item'Length > Remaining then Remaining else Item'Length);
   begin
      Item := Stream.Elements (Next .. Stream.Next + N - 1);
      Last := Item'First + N - 1;
      Stream.Next := @ + N;
   end;

   procedure Write(
      Stream : in out Params_Stream_Type;
      Item : in Ada.Streams.Stream_Element_Array)
   is
      Remaining: Count := Stream.Length - Stream.Next;
      Last: Ada.Streams.Stream_Element_Offset; -- last in Stream.Elements
   begin
      if Item'Length > Remaining
      then
         raise Constraint_Error with "Stream end reached unexpectedly";
      else
         Last := Stream.Next + Item'Length - 1;
         Stream.Elements (Next .. Last);
         Stream.Next := @ + Item'Length;
      end if;
   end;

   -- Synchronous call
   procedure Do_RPC(
      Partition  : in Partition_Id;
      Params     : access Params_Stream_Type;
      Result     : access Params_Stream_Type)
   is
   begin



   end;

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

   ... -- not specified by the language

end System.RPC;

--\
-----------------------------------------------------------------------------------------------
-- End of file

