

package System.Monitoring is

   type Flowpoint_ID is private;
   
   type Flowpoint_Category is (Profiling, Debugging); -- implementation can add further values
   
   type Flowpoint_Ambit is array (Flowpoint_Category) of Boolean;
   
   All_Categories: constant Flowpoint_Ambit := (others => True);
   
   type Flowpoint_Monitor is access procedure (Point: in Flowpoint_ID);
   
   type Monitor_Roster is array (Positive range <>) of Flowpoint_Monitor;
   
   function Active_Monitors (Categories: in Flowpoint_Ambit := All_Categories) return Monitor_Roster;
   
   function Ambit (Flowpoint: in Flowpoint_ID) return Flowpoint_Ambit;
   
   function Ambit (Monitor: in Flowpoint_Monitor) return Flowpoint_Ambit;
   
   procedure Activate_Monitor (Monitor: in Flowpoint_Monitor; 
                               Ambit:   in Flowpoint_Ambit);
   
   procedure Deactivate_Monitor (Monitor: in Flowpoint_Monitor);
   
   Monitoring_Error: exception;
   
private

   type Flowpoint_ID is Interfaces.Unsigned_32;   
   
end;
