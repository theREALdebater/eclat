generic
   Schedule_Name: constant Wide_String;
   procedure Do_Unit_Of_Work (Scheduled_Timepoint:   in  Ada.Calendar_Time;
                              Rescheduling_Required: out Boolean) is <>;
   procedure Prepare_For_First_Scheduling is null;
   procedure Finish_Off_After_Last_Scheduling is null;
   procedure Handle_Scheduler_Not_Found is null;

procedure AdaOS.Scheduling.Autosched_Regular (Period: in Duration);

