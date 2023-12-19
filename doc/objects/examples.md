-----------------------------------------------------------------------------------------------
# Implementing a System Object

.....




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 








-----------------------------------------------------------------------------------------------
## Example: Safe Keeper

.....

```ada
with Ada.Calendar;
use  Ada.Calendar;

package Temporary_Keeping
is
   task type Temporary_Keeper 
   is 
      new AdaOS.Objects.System_Object
   with
      entry Engage (Object:     not null access System_Object;
                     Controller: access Transaction_Controller'Class := Task_Transaction;
                     Authority:  in     Security_Authority           := Task_Authority;
                     Identity:   in     Security_Identity            := Task_Identity);

      entry Disengage (Object: access System_Object);

      entry Keep (Item:   in Unbounded_String; 
                  Expiry: in Time := Clock + 24*60*60.0;
                  Call:   in Keep_Service_Call'Class);

      entry Reveal (Item: out Unbounded_String;
                    Call: in  Reveal_Service_Call'Class);
   private
      ?????
   end;

   function Is_Engaged (Keeper: in Temporary_Keeper) return Boolean
   with 
      Endorse,
      Synchronization => By_Entry;

   function Engagement_Transaction (Keeper: in Temporary_Keeper) 
   return 
      access Transaction_Controller'Class
   with 
      Endorse;

   function Engagement_Authority (Keeper: in Temporary_Keeper) return Security_Authority
   with 
      Endorse;

   function Engagement_Identity (Keeper: in Temporary_Keeper) return Security_Identity
   with 
      Endorse;

private
   ?????
end;
```






```ada
with Ada.Calendar;
use  Ada.Calendar;

package AdaOS.Security.Guardians.Objects.Temporary_Keeper
is
   type TK_Service_Call is new Service_Call with null record;

   function Categories (Call: in TK_Service_Call) return Name_Set;

   type Keep_Service_Call is new TK_Service_Call
   with
      record
         Expiry: Time;         
      end record;
      
   function Categories (Call: in Keep_Service_Call) return Name_Set;

   function Properties (Call: in Keep_Service_Call) return Name_Set;

   function Property_Value (Call: in Keep_Service_Call; 
                              Name: in String) return String;

   function Set_Property (Call: in out Keep_Service_Call; 
                              Name: in String; 
                              Value: in String);

   type Reveal_Service_Call is new TK_Service_Call with null record;

   function Categories (Call: in Reveal_Service_Call) return Name_Set;
end;

package body AdaOS.Security.Guardians.Objects.Temporary_Keeper
is
   function Categories (Call: in TK_Service_Call) return Name_Set
   is
      Result: Name_Set;
   begin
      for Cat of Categories(Service_Call(Call)) loop Result.Append (Cat); end loop;
      Result.Append ("Temporary_Keeper");
      return Result;
   end;

   function Categories (Call: in Keep_Service_Call) return Name_Set
   is
      Result: Name_Set;
   begin
      for Cat of Categories(TK_Service_Call(Call)) loop Result.Append (Cat); end loop;
      Result.Append ("Temporary_Keeper.Keep");
      return Result;
   end;

   function Categories (Call: in Reveal_Service_Call) return Name_Set
   is
      Result: Name_Set;
   begin
      for Cat of Categories(TK_Service_Call(Call)) loop Result.Append (Cat); end loop;
      Result.Append ("Temporary_Keeper.Reveal");
      return Result;
   end;

   function Properties (Call: in Keep_Service_Call) return Name_Set
   is
      Result: Name_Set;
   begin
      Result.Append ("Expiry");
      return Result;
   end;

   function Property_Value (Call: in Keep_Service_Call; 
                              Name: in String) return String
   is
   begin
      if Name = "Expiry" then return Time'Image (Call.Expiry); end if;
      raise ?????;      
   end;

   function Set_Property (Call: in out Keep_Service_Call; 
                              Name: in String; 
                              Value: in String)
   is
   begin
      if Name = "Expiry" then Call.Expiry := Time'Value (Value); return; end if;
      raise ?????;
   end;

end AdaOS.Security.Guardians.Objects.Temporary_Keeper;
```






```ada
with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;
with Ada.Calendar;
use  Ada.Calendar;
with AdaOS.Security.Guardians.Objects.Safe_Keeper;
use  AdaOS.Security.Guardians.Objects.Safe_Keeper;

task body Temporary_Keeper 
is
   Kept: Unbounded_String := Null_Unbounded_String;
   Expiry: Time := Maximum_Time; -- Maximum_Time?????
begin
   loop
      select
         accept Keep (Item:   in Unbounded_String; 
                        Expiry: in Time := Clock + 24*60*60.0;
                        Call:   in Keep_Service_Call'Class)
         when
            Kept = Null_Unbounded_String
         do
            Call.Expiry := Expiry;
            Call.Endorse;
            Kept := Item;
            Temporary_Keeper.Expiry := Expiry;
         end;
      or
         accept Reveal (Item: out Unbounded_String;
                        Call: in  Reveal_Service_Call'Class)
         when
            Kept /= Null_Unbounded_String
         do
            Call.Endorse;
            Item := Kept;
            Kept := Null_Unbounded_String;
            Expiry := Maximum_Time;
         end;
      or
         delay until Expiry;
         Kept := Null_Unbounded_String;
         Expiry := Maximum_Time;
      or
         terminate;
      end select;
   end loop;
end Temporary_Keeper;
```








-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




