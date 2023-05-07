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
--/ System package

--| See RM 13.7.


with Realizor.Targets;

package System 
with
   Pure
is
   subtype Name is Realizor.Supported_Target;
   
   System_Name: constant Name := Realizor.Current.Target;

   -- System-Dependent Named Numbers:

   Min_Int:                constant := Realizor.Current.Min_Int;
   Max_Int:                constant := Realizor.Current.Max_Int;
   Max_Binary_Modulus:     constant := Realizor.Current.Max_Binary_Modulus;
   Max_Nonbinary_Modulus:  constant := Realizor.Current.Max_Nonbinary_Modulus;
   Max_Base_Digits:        constant := Realizor.Current.Root_Real'Digits;
   Max_Digits:             constant := Realizor.Current.Max_Digits;
   Max_Mantissa:           constant := Realizor.Current.Max_Mantissa;
   Fine_Delta:             constant := Realizor.Current.Fine_Delta;
   Tick:                   constant := Realizor.Current.Tick;

   -- Storage-related Declarations:

   type Address is Realizor.Current.Address;

   Null_Address:  constant Address;
   Storage_Unit:  constant := Realizor.Current.Storage_Unit;
   Word_Size:     constant := Realizor.Current.Words_Per_Storage_Unit * Storage_Unit;
   Memory_Size:   constant := Realizor.Current.Memory_Size;

   -- Address Comparison:

   function "<"  (Left, Right: Address) return Boolean with Convention => Intrinsic;
   function "<=" (Left, Right: Address) return Boolean with Convention => Intrinsic;
   function ">"  (Left, Right: Address) return Boolean with Convention => Intrinsic;
   function ">=" (Left, Right: Address) return Boolean with Convention => Intrinsic;
   function "="  (Left, Right: Address) return Boolean with Convention => Intrinsic;
-- function "/=" (Left, Right: Address) return Boolean; -- "/=" is implicitly defined

   -- Other System-Dependent Declarations:

   type Bit_Order is (High_Order_First, Low_Order_First);
   
   Default_Bit_Order: constant Bit_Order := 
      (if Target.Little_Endian then Low_Order_First else High_Order_First);

--| In a record representation clause, the bit numbers used correspond to the 'Word Bit' in the examples below:
--| 
--| High order first (big-endian), 32-bit example:
--|     Word Bit:   0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31
--|     Bit Value: 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
--|     Byte:       0  0  0  0  0  0  0  0  1  1  1  1  1  1  1  1  2  2  2  2  2  2  2  2  3  3  3  3  3  3  3  3
--|     Byte Bit:   0  1  2  3  4  5  6  7  0  1  2  3  4  5  6  7  0  1  2  3  4  5  6  7  0  1  2  3  4  5  6  7  
--|     Bit Value:  7  6  5  4  3  2  1  0  7  6  5  4  3  2  1  0  7  6  5  4  3  2  1  0  7  6  5  4  3  2  1  0  
--| 
--| Low order first (little-endian), 32-bit example:
--|     Word Bit:  31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
--|     Bit Value: 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
--|     Byte:       0  0  0  0  0  0  0  0  1  1  1  1  1  1  1  1  2  2  2  2  2  2  2  2  3  3  3  3  3  3  3  3
--|     Byte Bit:   7  6  5  4  3  2  1  0  7  6  5  4  3  2  1  0  7  6  5  4  3  2  1  0  7  6  5  4  3  2  1  0  
--|     Bit Value:  7  6  5  4  3  2  1  0  7  6  5  4  3  2  1  0  7  6  5  4  3  2  1  0  7  6  5  4  3  2  1  0  
--| 
--| Thse examples assume the `Storage_Unit` is 32. 

   -- Priority-related declarations (see D.1):

   subtype Any_Priority       is Integer        range Realizor.Current.Any_Priority;
   subtype Priority           is Any_Priority   range Any_Priority'First .. Realizor.Current.Interrupt_Priority_First;
   subtype Interrupt_Priority is Any_Priority   range Priority'Last + 1 .. Any_Priority'Last;
   
   Default_Priority: constant Priority := (Priority'First + Priority'Last)/2;

private

   Null_Address: constant Address := Target.Null_Address;

   function "<" ( Left, Right: Address ) return Boolean renames Target."<";
   function "<="( Left, Right: Address ) return Boolean renames Target."<=";
   function ">" ( Left, Right: Address ) return Boolean renames Target.">";
   function ">="( Left, Right: Address ) return Boolean renames Target.">=";
   function "=" ( Left, Right: Address ) return Boolean renames Target."=";

end System;

--\
-----------------------------------------------------------------------------------------------
-- End of file


