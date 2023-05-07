-----------------------------------------------------------------------------------------------

with .....;

use .....;

package body AdaOS.Instances
with
    Remote_Types
is
-----------------------------------------------------------------------------------------------
    .....



-----------------------------------------------------------------------------------------------

    task body Environment_Task
    is
    begin
        Self.Main;
    end;

-----------------------------------------------------------------------------------------------



    function Name (Mode: in Execution_Mode) return Wide_String 
    is
        (case Mode is when Setup => "Setup", .....);

    function Mode_Transition_Error_Message (From, To: in Execution_Mode) 
    return
        Wide_String
    is
        ("Cannot transition from mode " & Name(From) & " to " & Name(To) & ". ");
            



-----------------------------------------------------------------------------------------------


    task body Event_Receiver
    is
        function MTEM (From, To: in Execution_Mode) return Wide_String 
        renames 
            Mode_Transition_Error_Message;

    begin
        S := (instance signal id);
        C := System_Broker.Event_Class("system.signals." & S);
        E: System_Signal_Access;
        loop
            select
                E := new System_Signal'Class(C.Receive);
            then abort
                delay 1.3;
                raise Heartbeat_Failure;
            end select;

            Process_Signal(E.all);

            or

            or
                
            end select;
        end loop;







    end;

-----------------------------------------------------------------------------------------------

    protected body Shutdown_State
    is
        Current_Level: Shutdown_Level := Normal;

        function Level return Shutdown_Level is (Current_Level);

        procedure Set_Level (Level: in Shutdown_Level)
        is
        begin
            Current_Level := Level;
        end;

        when Current_Level >= Level =>
        entry Await_Level (Level: in Shutdown_Level) is begin null; end;
    end;



    procedure Process_Shutdown (Instance: in out Executional_Instance'Class;
                                E:        in     System_Shutdown'Class)
    is
    begin
        Instance.Shutdown.Set_Level (E.Level);
    end;

-----------------------------------------------------------------------------------------------









-----------------------------------------------------------------------------------------------







-----------------------------------------------------------------------------------------------







-----------------------------------------------------------------------------------------------








-----------------------------------------------------------------------------------------------


    procedure Process_Signal (Instance: in out Executional_Instance'Class;
                              E:        in     System_Signal'Class)
    is

    begin
        if E in System_Shutdown'Class then Process_Shutdown (System_Shutdown'Class(E)); end if;


                accept Change_Mode (New_Mode: in Execution_Mode) 
                do
                    -- Check that the transition is legitimate:
                    case Mode is
                        when Setup => 
                            case New_Mode is
                                when Setup      => null;
                                when Stopped    => Check_Change_From_Setup_To_Stopped (Self);
                                when Running   => 
                                when Terminated => 
                            end case;
                        when Stopped => 
                            case New_Mode is
                                when Setup      => 
                                when Stopped    => null;
                                when Running   => 
                                when Terminated => 
                            end case;
                        when Running => 
                            case New_Mode is
                                when Setup      => 
                                when Stopped    => 
                                when Running   => null;
                                when Terminated => 
                            end case;
                        when Terminated => 
                            case New_Mode is
                                when Setup      => raise Mode_Error with MTEM (Mode, New_Mode);
                                when Stopped    => raise Mode_Error with MTEM (Mode, New_Mode);
                                when Running   => raise Mode_Error with MTEM (Mode, New_Mode);
                                when Terminated => null;
                            end case;
                    end case;
                end;
                -- Actually perform the mode transition:
                case Mode is
                    when Setup => 
                        case New_Mode is
                            when Setup      => null;
                            when Stopped    => Change_From_Setup_To_Running (Self);
                            when Running   => Change_From_Setup_To_Running (Self);
                            when Terminated => Change_From_Setup_To_Terminated (Self);
                        end case;
                    when Stopped => 
                        case New_Mode is
                            when Setup      => raise Program_Error;
                            when Stopped    => null;
                            when Running   => Change_From_Stopped_To_Running (Self);
                            when Terminated => Change_From_Stopped_To_Terminated (Self);
                        end case;
                    when Running => 
                        case New_Mode is
                            when Setup      => raise Program_Error;
                            when Stopped    => Change_From_Running_To_Stopped (Self);
                            when Running   => null;
                            when Terminated => Change_From_Running_To_Terminated (Self);
                        end case;
                    when Terminated => 
                        case New_Mode is
                            when Setup      => raise Program_Error;
                            when Stopped    => raise Program_Error;
                            when Running   => raise Program_Error;
                            when Terminated => null;
                        end case;
                end case;





-----------------------------------------------------------------------------------------------





    procedure Change_From_Setup_To_Running (Instance: in Executional_Instance)
    is
    begin
        Instance.Mode := Running;
        Instance.Main_Task := new Environment_Task(Instance);
    end;

    procedure Change_From_Setup_To_Stopped (Instance: in Executional_Instance)
    is
    begin
        Instance.Mode := Stopped;
    end;

    procedure Change_From_Setup_To_Terminated (Instance: in Executional_Instance)
    is
    begin
        Instance.Mode := Terminated;
    end;

    procedure Change_From_Stopped_To_Running (Instance: in Executional_Instance)
    is
    begin
        Instance.Mode := Running;
        if Instance.Main_Zombie = null -- if instance has never really been run
        then
            -- just make a new environment task
            Instance.Main_Task := new Environment_Task(Instance);
        else
            -- rehydrate the environment task
            Instance.Main_Task := Environment_Task'Read_And_Resume(Instance.Main_Zombie.Open); 
            Instance.Main_Zombie.Close;
            Instance.Main_Zombie := null; -- don't need the zombie any more
        end if;
    end;

    procedure Change_From_Stopped_To_Terminated (Instance: in Executional_Instance)
    is
    begin
        Instance.Mode := Terminated;
        if Instance.Main_Zombie = null -- if instance has never really been run
        then
            -- we can successfully become terminated
            Instance.Exit_Status := Success_Exit_Status;
        else
            -- we must assume failure
            Instance.Exit_Status := Failure_Exit_Status;
            Instance.Main_Zombie := null; -- don't need the zombie any more
        end if;
    end;

    procedure Change_From_Running_To_Stopped (Instance: in Executional_Instance)
    is
    begin
        Instance.Mode := Stopped;
        Instance.Main_Zombie := new Serialized_Task;
        Instance.Main_Task'Suspend_And_Write(Instance.Main_Zombie.Create);
        Instance.Main_Task := null;
        Instance.Main_Zombie.Close;
    end;

    procedure Change_From_Running_To_Terminated (Instance: in Executional_Instance)
    is
    begin
        Instance.Mode := Terminated;
        if Instance.Main_Task = null -- if instance has never really been run
        then
            -- we can successfully become terminated
            Instance.Exit_Status := Success_Exit_Status;
        else
            -- we must assume failure
            Instance.Exit_Status := Failure_Exit_Status;
            Instance.Main_Task := null; -- don't need the environment task any more
        end if;
    end;




-----------------------------------------------------------------------------------------------






    procedure Run (Instance: not null access Executional_Instance)
    is
    begin
        Instance.Receiver.Change_Mode (Running);
    end;

-----------------------------------------------------------------------------------------------


-----------------------------------------------------------------------------------------------
end AdaOS.Instances;

