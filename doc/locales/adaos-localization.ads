-----------------------------------------------------------------------------------------------
-- 



-----------------------------------------------------------------------------------------------
--/ `AdaOS.Localization` package specification

package AdaOS.Localization is

   type Locale_ID is (en_US, en_GB, en_UK);
   
   function Includes (Super, Sub: in Locale_ID) return Boolean with Pure;
   
   function Canonical (Original: in Locale_ID) return Locale_ID with Pure;
   
   function Current_Locale return Locale_ID;
   procedure Set_Current_Locale (ID: in Locale_ID);

end;

--\

