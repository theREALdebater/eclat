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
 
 //:

#undef assert
#undef static_assert

#ifdef NDEBUG
#define assert(test) ((void)0)
#else
#define assert(test) __builtin(ASSERT(test))
#endif

#define static_assert _Static_assert

//;
/*********************************************************************************************/

