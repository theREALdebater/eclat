--/ AdaOS package specification:

package AdaOS is

   --/ USIDs:

   type Universal_System_ID is private;

   Null_Universal_System_ID: constant Universal_System_ID;

   --\
   --/ kjh





   --\

private

   --/ USID implementation:

   type Universal_System_ID is mod 2**64;

   for Universal_System_ID'Size use 64;

   Null_Universal_System_ID: constant Universal_System_ID := 0;

   --\

end AdaOS;

--\


