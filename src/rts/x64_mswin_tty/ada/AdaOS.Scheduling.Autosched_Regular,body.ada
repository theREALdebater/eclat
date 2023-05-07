with Ada.Calendar;
with AdaOS.Objects.Persistence;
with AdaOS.Execution;

use AdaOS.Objects.Persistence, AdaOS.Execution.Current_Context;

procedure AdaOS.Scheduling.Autosched_Regular (Period: in Duration) is
   Sched_Obj: constant Wide_String := Self.Environment_Variable("SCHED");
   Timepoint: Ada.Calendar.Time;
   Rescheduling_Required: Boolean;
begin
   if Sched_Obj /= null then
      -- we have been executed by the scheduler - do one unit of work and then reschedule
      Timepoint := Sched_Obj.Timepoint;
      Sched_Obj.Disengage;
      Do_Unit_Of_Work( Timepoint, Rescheduling_Required );
      if Rescheduling_Required then
         Sched_Obj.Reschedule( Timepoint + Period );
      else
         Finish_Off_After_Last_Scheduling;
      end if;
   else
      declare -- we have been executed on the command line - schedule for the first time
         Files: constant File_Directory_Access := Self.Files;
         Scheduler: Scheduling_Master_Access;
         Scheduler_Token: Object_Engagement_Token;
         UMID: constant ??? := Self.Program.UMID;
         Arguments: Argument_Array(1..Self.Argument_Count);
      begin
         Files.Engage( "~/sched", Scheduler, Scheduler_Token ); -- ??????
         if Scheduler_Token = Null_Engagement_Token then
            Handle_Scheduler_Not_Found;
            raise Scheduler_Unavailable;
         else
            Prepare_For_First_Scheduling;
            for i in 1..Self.Argument_Count loop
               Arguments(i) := new Wide_String(Self.Argument(i));
            end loop; -- gather arguments into array
            Scheduler.Insert( new Scheduling_Descriptor'(Schedule_Name,
                                                         Clock, -- i.e. execute immediately
                                                         UMID,
                                                         Arguments) ); -- create schedule
         end if;
      end;
   end if;
end AdaOS.Scheduling.Autosched_Regular;
