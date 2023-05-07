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




package AdaOS.Compartments
is



function Directory_Path_Separator (Compartment: in Program_Compartment) return Path_String;



function Self_Directory_Alias (Compartment: in Program_Compartment) return Path_String;
function Super_Directory_Alias (Compartment: in Program_Compartment) return Path_String;
function Home_Directory_Alias (Compartment: in Program_Compartment) return Path_String; -- first node only
function Root_Directory_Alias (Compartment: in Program_Compartment) return Path_String; -- first node only








