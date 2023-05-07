-----
--/ AdaOS.Remote_Streams package specification:

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with AdaOS.Objects; use AdaOS.Objects;

package AdaOS.Remote_Streams is

   pragma Remote_Types;

   type Remote_Stream is synchronized interface and System_Object;

   type Remote_Stream_Access is access all Remote_Stream'Class;

   type Local_Stream is abstract new Root_Stream_Type with private;

   type Local_Stream_Access is access all Local_Stream'Class;

   procedure Open (Remote: in out Remote_Stream;
                   Mode:   in     File_Mode; -- in or out
                   Local:  out    Local_Stream_Access) is abstract;

   procedure Close (Local: in out Local_Stream) is abstract;

private

   type Local_Stream is abstract new Root_Stream_Type with null record;

end;

