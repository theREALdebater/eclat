

package body System.RPC is


   procedure Read (Stream: in out Params_Stream_Type;
                   Item:   out    Ada.Streams.Stream_Element_Array;
                   Last:   out    Ada.Streams.Stream_Element_Offset) is
   begin
      .....
   end;


   procedure Write (Stream: in out Params_Stream_Type;
                    Item:   in     Ada.Streams.Stream_Element_Array) is
   begin
      .....
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

end System.RPC;

