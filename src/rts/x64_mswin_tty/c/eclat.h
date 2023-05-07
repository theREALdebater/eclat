









/*********************************************************************************************
 * 
 * Copyright (C) 2019 The AdaOS Project
 * 
 * This file is part of ECLAT. 
 * 
 *   ECLAT is free software: you can redistribute it and/or modify it under the terms of the 
 *   GNU General Public License as published by the Free Software Foundation, either version 3 
 *   of the License, or (at your option) any later version. 
 * 
 *   ECLAT is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 *   without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 *   See the GNU General Public License for more details. 
 * 
 *   You should have received a copy of the GNU General Public License along with ECLAT.  If 
 *   not, see <http://www.gnu.org/licenses/>.
 * 
 *********************************************************************************************/

//: ECLAT C header file
















//: Microsoft C compatibility

//. A declaration of the following form is recognised by ECLAT:

//       extern "C" __declspec(dllimport) 
//       errno_t __cdecl getenv_s( 
//          size_t *restrict len, 
//          char *restrict value,
//          rsize_t valuesz, 
//          const char *restrict name );

//. The `extern "C"` part is for the benefit of C++, but is harmlessly ignored by ECLAT C. The 
//. macros `__declspec()` and `__cdecl` are not macros in ECLAT but are built-in to the C 
//. compiler. The argument `dllimport` does not imply import from a DLL specifically, but any 
//. kind of module recognised by ECLAT. The Realizor configuration determines the kind of 
//. module. Currently `__cdecl` does nothing and is just ignored. 

//;








//;

/*********************************************************************************************/
