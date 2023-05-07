
-- Accounting and limiting.



package AdaOS.Accounting is



   type Resource_Account_ID is private;



   type Resource_Request is
      record
         Account:  Resource_Account_ID;
         Quantity: Natural := 1;
      end record;


   type Resource_Request_Set is ...;




   procedure Request_Resources (Requests: Resource_Request_Set







