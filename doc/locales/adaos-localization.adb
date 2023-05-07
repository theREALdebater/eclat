-----------------------------------------------------------------------------------------------
-- 

-----------------------------------------------------------------------------------------------
--/ `AdaOS.Localization` package body

package body AdaOS.Localization 
is

   Current: Locale_ID := en_US;

   function Includes (Super, Sub: in Locale_ID) return Boolean
   is
      (Super = en_US and Sub = en_GB) or
      (Super = en_US and Sub = en_UK);
      
   function Canonical (Original: in Locale_ID) return Locale_ID
   is
      (if Original = en_GB then en_UK else Original);

   function Current_Locale return Locale_ID
   is
      (Current);

   procedure Set_Current_Locale (ID: in Locale_ID)
   is
   begin
      Current := ID;
   end;

end;

--\

