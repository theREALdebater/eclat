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

//: Standard strings

#ifndef __STRING_H__
#define __STRING_H__

//: Description of an error number (as `errno` would be set to)

import char* strerror( int errnum );

//. The function `strerror` returns a string describing the error number in argument `errnum`.

#if __STDC_WANT_LIB_EXT1__

import errno_t strerror_s( char *buf, rsize_t bufsz, errno_t errnum );

//. The function `strerror_s` copies a description of the error number in argument `errnum` 
//. into `buf` limited to `bufsz` bytes. 

import size_t strerrorlen_s( errno_t errnum );

//. The function `strerrorlen_s` returns the length, in bytes, of a description of the error
//. number in argument `errnum`. 

#endif

//;
//: 







//;
//: 







//;
//: 







//;
//: 







//;
//: 







//;
//: Print description of current error (in `errno`)

import void perror( const char *s );




//;
//: 







//;
//: 







//;
//: 







//;
//: 







//;
//: 







//;
//: 







//;
//: 







//;

#endif

//;

/*********************************************************************************************/

