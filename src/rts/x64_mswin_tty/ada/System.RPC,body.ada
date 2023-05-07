

package body AdaOS.RPC is


   function New_Memory_Region (Initial_Size: in Storage_Element_Count) is
   begin
      .....
   end;



   procedure Read (Stream: in out Params_Stream_Type;
                   Item:   out    Ada.Streams.Stream_Element_Array;
                   Last:   out    Ada.Streams.Stream_Element_Offset) is
   begin
      if Stream.Mode /= Being_Read then
         Stream.Mode /= Being_Read;
         Stream.Current := 0;
      end if;
      if Stream.Current >= Stream.Count then
         raise ???;
      end if;
      AdaOS.Storage_Elements.Copy(Stream.Region.Start + Stream.Current, Item'Address)
      Stream.Count := Stream.Count + Item'Length;
      Stream.Current := Stream.Current + Storage_Elements_Per_Stream_Element * Item'Length;
   end;


   procedure Write (Stream: in out Params_Stream_Type;
                    Item:   in     Ada.Streams.Stream_Element_Array) is
   begin
      if Stream.Mode /= Being_Written then
         Stream.Mode /= Being_Written;
         Stream.Current := 0;
         Stream.Count := 0;
      end if;
      if Stream.Count + Item'Length > Stream.Current_Size then
         .....
      end if;
      AdaOS.Storage_Elements.Copy(Item'Address, Stream.Region.Start + Stream.Current)
      Stream.Count := Stream.Count + Item'Length;
      Stream.Current := Stream.Current + Storage_Elements_Per_Stream_Element * Item'Length;
   end;


   -- Synchronous call


   procedure Do_RPC (Partition: in     Partition_Id;
                     Params:    access Params_Stream_Type;
                     Result:    access Params_Stream_Type) is
   begin
      .....
   end;


   -- Asynchronous call


   procedure Do_APC (Partition: in     Partition_Id;
                     Params:    access Params_Stream_Type) is
   begin
      .....
   end;


   -- The handler for incoming RPCs


   procedure Establish_RPC_Receiver (Partition: in Partition_Id;
                                     Receiver:  in RPC_Receiver) is
   begin
      .....
   end;


begin
   .....

end AdaOS.RPC;

