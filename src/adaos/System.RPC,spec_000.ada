

with Ada.Streams; -- see 13.13.1

package System.RPC is
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
7
7.a
8/1
9
10
11
12
13
1/2
1.a
2
3
4
5
6
7
8
ISO/IEC 8652:2007(E) Ed. 3 � Annotated Ada Reference Manual
843 10 November 2006 Partition Communication Subsystem E.5
-- Synchronous call
procedure Do_RPC(
Partition : in Partition_Id;
Params : access Params_Stream_Type;
Result : access Params_Stream_Type);
-- Asynchronous call
procedure Do_APC(
Partition : in Partition_Id;
Params : access Params_Stream_Type);
-- The handler for incoming RPCs
type RPC_Receiver is access procedure(
Params : access Params_Stream_Type;
Result : access Params_Stream_Type);
procedure Establish_RPC_Receiver(
Partition : in Partition_Id;
Receiver : in RPC_Receiver);
private
... -- not specified by the language
end System.RPC;