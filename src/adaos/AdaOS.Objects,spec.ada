package AdaOS.Objects
with
    Remote_Types
is
    type Object_Id is private;

    No_Object: constant Object_Id;

    type System_Object is synchronized interface;

    type Object_Access is access all System_Object'Class;

    










private

    type Object_Id is mod 2**64;

    No_Object: constant Object_Id := 0;





end;

