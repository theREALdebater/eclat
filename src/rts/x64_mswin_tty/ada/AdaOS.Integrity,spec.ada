-----
--/ Package specification AdaOS.Integrity:

with AdaOS.Interop;


package AdaOS.Integrity is

   pragma Remote_Call_Interface;

   --/ Transaction manager:

   type Transaction_Controller is new Interop.Dynamic_Object with private;

   --| ....

   type Transaction_Controller_Access is access all Transaction_Controller'Class;

   --| ....

   --\
   --/ Basic states:

   type Basic_Transaction_State is (Pending,
                                    Committing,
                                    Aborting,
                                    Committed,
                                    Aborted);

   --| ....

   function Basic_State (Controller: in Transaction_Controller) return Transaction_State;

   --| ....

   function Supertransaction (Controller: in Transaction_Controller)
                                                          return Transaction_Controller_Access;

   --| ....

   function New_Subtransaction (Controller: access Transaction_Controller)
                                                          return Transaction_Controller_Access;

   --| ....

   function New_Transaction return Transaction_Controller_Access;

   --| The New_Transaction function creates a new top-level transaction manager, and returns a
(remote)
access value designating it.

   --\
   --/ Ending a transaction:

   procedure Commit_Transaction (Controller: in out Transaction_Controller);
   procedure Abort_Transaction  (Controller: in out Transaction_Controller);

   --| ....

   type Transaction_ID is private;

   --| ....

   function ID (Controller: in Transaction_Controller) return Transaction_ID;

   --| ....

   type Transaction_Subscriber is synchronized interface;

   type Transaction_Subscriber_Access is access all Transaction_Subscriber'Class;

   --| ....

   procedure Commit_Changes (Subscriber: in out Transaction_Subscriber;
                             Controller: in     Transaction_ID) is abstract;

   procedure Abort_Changes  (Subscriber: in out Transaction_Subscriber;
                             Controller: in     Transaction_ID) is abstract;

   --| Either the Commit_Changes or the Abort_Changes procedure is called by a transaction
manager to cause one of its subscribers to either commit or roll back changes that have been
made to it under the auspices of the manager's transaction. The subscriber should complete
the required action fully before returning.

   --| ......



   procedure Subscribe (Controller: in out Transaction_Controller;
                        Subscriber: in     Transaction_Subscriber_Access);

   --| ....


   procedure Iterate_Subscribers (Controller: in Transaction_Controller;
                                  Process:
                                 access procedure (Subscriber: Transaction_Subscriber_Access));

   --| ....


   Transaction_Error: exception;

   --| ....

private

   type Transaction_Controller is new Interop.Dynamic_Object with
      record
         ...
      end record;


end;

