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
--/ Realizor




package Realizor
is
   type Supported_Target is (ia64, ARM_v7, SPARC_v5); -- etc. 

--| The enumeration type `Supported_Target` has values for all the possible target 
--| architectures supported by the Realizor.

   type Root_Integer_Type is range -2**63 - 1 .. 2**63; 

--| The type `Root_Integer_Type` is the overall effective root integer type understood by the 
--| Realizor and ECLAT. 

   package Current
   is
      Target: constant Supported_Target; 
      with
         Import, 
         External_Name => "pxcr.rtsch.target";

--| The constant `Current.Target` is the value of `Supported_Targets` which is the target 
--| architecture of the currently executing machine code. Note that this could vary from 
--| segment to segment. 

      Min_Int:                constant Root_Integer_Type
      with
         Import, 
         External_Name => "pxcr.rtsch.int_min";

      Max_Int:                constant := ?????;
      Max_Binary_Modulus:     constant := ?????;
      Max_Nonbinary_Modulus:  constant := ?????;
      Max_Base_Digits:        constant := ?????;
      Max_Digits:             constant := ?????;
      Max_Mantissa:           constant := ?????;
      Fine_Delta:             constant := ?????;
      Tick:                   constant := ?????;

      Address_Size: constant Root_Integer_Type
      with
         Import, 
         External_Name => "pxcr.rtsch.addr_bits";

      type Address is mod 2**(Address_Size);

--| The type `Current.Address` is a modulus type of the correct size (number of bits) to cover
--| an address of the ......

      Null_Address:  constant Address
      with
         Import, 
         External_Name => "pxcr.rtsch.addr_null";

      Storage_Unit:           constant := ?????;
      Words_Per_Storage_Unit: constant := ?????;
      Memory_Size:            constant := ?????;

      Little_Endian: constant Boolean
      with
         Import, 
         External_Name => "pxcr.rtsch.is_little_endian";

--| * `Target.` is 
--| 



--| * `Target.` is 
--| 



--| * `Target.` is 
--| 



--| * `Target.` is 
--| 



--| * `Target.` is 
--| 
--| 

   end Current;

end Realizor;

--\
-----------------------------------------------------------------------------------------------
-- End of file

