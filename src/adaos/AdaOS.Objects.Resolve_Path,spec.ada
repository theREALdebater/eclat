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

generic
   type Object_Type is new System_Object with private;

function AdaOS.Objects.Resolve_Path 
   (Path:        in Path_String;
    Base:        access Object_Directory'Class := null; -- null for CWD
    Compartment: not null access Program_Compartment'Class := Task_Instance.Compartment;
    Trial:       in Boolean := False) 
return 
   access Object_Type'Class;

--| An instantiation of the generic function `Resolve_Path` resolves the given `Path` according 
--| to the conventions of a specific `Compartment`. 
--| 
--| If the the path is relative, it is relative to the given `Base` directory; if the base is 
--| null, the path is relative to the compartment's current working directory. If the path is 
--| absolute, it is relative to the compartment's root directory. 
--| 
--| The resolved system object must be in `Object_Type'Class`. 
--| 
--| If resolution of the path fails, then: if `Trial` is `False`, the exception `Path_Error`
--| is propagated; if `Trial` is `True`, the value `null` is returned. 
--| 
--| The default compartment is the compartment of the calling task's executional instance. 
--| `Trial` defaults to `False`. `Base` defaults to 'null'. 

