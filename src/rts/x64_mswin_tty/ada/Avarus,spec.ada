---------------------
--/ Avarus package specification

--| Avarus is the default object manager for AdaOS. 
--| ...

with AdaOS;
with AdaOS.RPC.Services;

package Avarus
is
   pragma Remote_Types( Avarus );

   type Avarus_Service is new AdaOS.RPC.Services.Remote_Service with private;
   
   






