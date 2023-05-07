

with Ada.Streams;

package AdaOS.RPC is

   type Partition_Id is private;

   Communication_Error: exception;

   type Params_Stream_Type (Initial_Size: Ada.Streams.Stream_Element_Count) is
      new Ada.Streams.Root_Stream_Type with private;

   procedure Read (Stream: in out Params_Stream_Type;
                   Item:   out    Ada.Streams.Stream_Element_Array;
                   Last:   out    Ada.Streams.Stream_Element_Offset);

   procedure Write (Stream: in out Params_Stream_Type;
                    Item:   in     Ada.Streams.Stream_Element_Array);

   -- Synchronous call

   procedure Do_RPC (Partition: in     Partition_Id;
                     Params:    access Params_Stream_Type;
                     Result:    access Params_Stream_Type);

   -- Asynchronous call

   procedure Do_APC (Partition: in     Partition_Id;
                     Params:    access Params_Stream_Type);

   -- The handler for incoming RPCs

   type RPC_Receiver is access procedure (Params: access Params_Stream_Type;
                                          Result: access Params_Stream_Type);

   procedure Establish_RPC_Receiver (Partition: in Partition_Id;
                                     Receiver:  in RPC_Receiver);

private

   type Partition_Id is AdaOS.ECLAT.Module_Id;

   Storage_Elements_Per_Stream_Element: constant := Stream_Element'Size / Storage_Element'Size;
   -- assumes Storage_Element'Size is an exact multiple of Stream_Element'Size

      type Stream_Mode is (Being_Read, Being_Written);

   type Memory_Region is
      record
         RID:   AdaOS.Bachar.Resource_Id := AdaOS.Bachar.Null_Resource_Id;
         Start: AdaOS.Address;
      end record;

   function New_Memory_Region (Initial_Size: in Storage_Element_Count);

   type Params_Stream_Type (Initial_Size: Ada.Streams.Stream_Element_Count) is
                                                          new Ada.Streams.Root_Stream_Type with
      record
         Region:  Memory_Region := New_Memory_Region(Initial_Size * Storage_Elements_Per_Stream_Element);
         Current: AdaOS.Storage_Offset := 0;
         Count:   Ada.Streams.Stream_Element_Count;
         Mode:    Stream_Mode := Being_Written;
         Current_Size: Storage_Element_Count := Initial_Size * Storage_Elements_Per_Stream_Element;
      end record;

end AdaOS.RPC;

