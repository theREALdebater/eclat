/*********************************************************************************************
 * 
 * Copyright (C) 2019 The AdaOS Project
 * 
 * This file is part of ECLAT. 
 * 
 * ECLAT is free software: you can redistribute it and/or modify it under the terms of the 
 * GNU General Public License as published by the Free Software Foundation, either version 3 
 * of the License, or (at your option) any later version. 
 * 
 * ECLAT is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details. 
 * 
 * You should have received a copy of the GNU General Public License along with ECLAT.  If 
 * not, see <http://www.gnu.org/licenses/>.
 * 
 *********************************************************************************************/

//: 

#ifndef __LIMITS_H__
#define __LIMITS_H__

#define CHAR_BIT     __builtin(BITS_PER_CHAR) // >= 8
#define SCHAR_MIN    __builtin(SCHAR_MIN) // <= -127
#define SCHAR_MAX    __builtin(SCHAR_MAX) // >= 127
#define UCHAR_MAX    (2**CHAR_BIT − 1)
#define CHAR_MIN     0
#define CHAR_MAX     UCHAR_MAX
#define MB_LEN_MAX   8 // maximum number of bytes in a multibyte character, for any supported locale
#define SHRT_MIN     __builtin(SHORT_MIN) // <= -32767
#define SHRT_MAX     __builtin(SHORT_MAX) // >= 32767
#define USHRT_MAX    __builtin(USHORT_MAX) // >= 65535
#define INT_MIN      __builtin(INT_MIN) // <= -32767
#define INT_MAX      __builtin(INT_MAX) // >= 32767
#define UINT_MAX     __builtin(UINT_MAX) // >= 65535
#define LONG_MIN     __builtin(LONG_MIN) // <= -2147483647
#define LONG_MAX     __builtin(LONG_MAX) // >= 2147483647
#define ULONG_MAX    __builtin(ULONG_MAX) // >= 4294967295
#define LLONG_MIN    __builtin(LONGLONG_MIN) // <= -9223372036854775807
#define LLONG_MAX    __builtin(LONGLONG_MAX) // >= 9223372036854775807
#define ULLONG_MAX   __builtin(ULONGLONG_MAX) // >= 18446744073709551615

#endif

//;
/*********************************************************************************************/

