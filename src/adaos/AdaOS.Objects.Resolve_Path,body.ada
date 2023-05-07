-----------------------------------------------------------------------------------------------
-- 
-- Copyright (C) 2022 The AdaOS Project
-- 
-- This file is part of ECLAT.
-- 
-- ECLAT is free software: you can redistribute it and/or modify it under the terms of the GNU 
-- General Public License as published by the Free Software Foundation, either version 3 of 
-- the License, or (at your option) any later version. 
-- 
-- ECLAT is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without 
-- even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
-- GNU General Public License for more details. 
-- 
-- You should have received a copy of the GNU General Public License along with ECLAT.  If 
-- not, see <http://www.gnu.org/licenses/>. 
-- 
-- As a special exception, if other files instantiate generics from this unit, or you link 
-- this unit with other files to produce an executable, this unit does not by itself cause 
-- the resulting executable to be covered by the GNU General Public License. This exception 
-- does not however invalidate any other reasons why the executable file might be covered by 
-- the GNU General Public License. 
-- 
-----------------------------------------------------------------------------------------------
with AdaOS.Compartments, AdaOS.Instances;
use AdaOS.Compartments, AdaOS.Instances;

function AdaOS.Objects.Resolve_Path
   (Path:        in Path_String;
    Base:        access Object_Directory'Class := null; -- null for CWD
    Compartment: not null access Program_Compartment'Class := Task_Instance.Compartment;
    Trial:       in Boolean := False) 
return 
   access Object_Type'Class
is
   Names: Path_Vectors.Vector := 
      Split_Path (Path, Compartment.Directory_Path_Separator); -- normalises each name

   Temp, Result: access System_Object'Class := null;

   subtype WWS is Wide_Wide_String;
   type Object_Access is access all Object_Type'Class;

   function To_WWS (Path: in Path_String) return Wide_Wide_String renames To_Wide_Wide_String;
   
   function Invalid_Path_Message (Message: in WWS) return WWS is 
      ("Invalid path """ & To_WWS(Path) & """: " & Message & ". ");

begin
   if Length(Names) = 1 and then Length(Names(1)) = 0 -- i.e. path is completely empty
   then
      if Trial
      then
         Result := null;
      else
         raise Path_Error with "Invalid path: Path may not be empty. ";
      end if;
   else
      if Length(Names) > 1 and then Names(1) = Compartment.Root_Directory_Alias 
         -- "", so matching a leading "/" separator in the path
      then
         Result := Compartment.Root_Directory;
         Names.Delete (1);
      elsif 
         Names(1) = Compartment.Home_Directory_Alias -- "~"
      then
         Result := Compartment.Home_Directory;
         Names.Delete (1);
      else
         Result := (if Base = null then Compartment.Current_Directory else Base);
      end if;

      for i in Names'Range
      loop
         if Names(i) = Compartment.Super_Directory_Alias -- ".."
         then
            Result := Result.Super_Directory; -- note the super of root is root
         elsif 
            Names(i) = Compartment.Self_Directory_Alias -- "."
         then

            if not Result.all in Object_Directory'Class
            then
               if Trial
               then
                  Result := null;
                  exit; 
               else
                  raise Path_Error with Invalid_Path_Message 
                     ("Node """ & To_WWS(Names(i)) & """ is not a directory");
               end if;
            end if;

         else

            Temp := Result;
            Temp.Engage;
            Result := Object_Directory(Temp).Find(Names(i));
            Temp.Disengage;
            Temp := null; -- to facilitate early GC finalization

            if Result = null
            then
               exit when Trial; -- name not found, so just return null
               raise Path_Error with Invalid_Path_Message
                  ("Node """ & To_WWS(Names(i)) & """ does not exist");
            end if;

            if i < Names'Last and not Result.all in Object_Directory'Class
            then
               if Trial
               then
                  Result := null;
                  exit; 
               else
                  raise Path_Error with Invalid_Path_Message
                     ("Node """ & To_WWS(Names(i)) & """ is not a directory");
               end if;
            end if;

         end if;
      end loop;
   end if;

   if Result /= null and then Result in Object_Access
   then
      return Object_Access(Result);
   else
      if Trial
      then
         return null;
      else
         raise Path_Error with Invalid_Path_Message
            ("Terminal node """ & To_WWS(Names(Names'Last)) & """ is not of expected type");
      end if;
   end if;

exception
   when others =>
      if Temp /= null then Temp.Disengage; end if;
      raise;
end;

