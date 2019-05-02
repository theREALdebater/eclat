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

#ifndef __ERRNO_H__
#define __ERRNO_H__

extern int * __ECLAT__errno(void);
extern int __ECLAT__EDOM(void);
extern int __ECLAT__EILSEQ(void);
extern int __ECLAT__ERANGE(void);

#define EDOM      (__ECLAT__EDOM())
#define EILSEQ    (__ECLAT__EILSEQ())
#define ERANGE    (__ECLAT__ERANGE())
//#define errno     (*__ECLAT__errno())
#define errno     (*__errno_location())

// On Linux:
// all syscalls return between -4095 and -1 on failure
// e.g. ENOENT==2, ENOENT from syscall returns -2
// VDSO intervenes
// vsyscallpage

#endif

//;
/*********************************************************************************************/

