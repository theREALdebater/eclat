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

//: Standard mathematical functions

#ifndef __MATH_H__
#define __MATH_H__





#define pow(x,y)        __builtin(POWER(x,y))
???#define powi(x,y)    __builtin(INTEGER_POWER(x,n)) // results in multiplication tree

//. Finding the optimal power tree might be hard, but since it is only interesting for small 
//. powers, the obvious answer is to precompute it once (Knuth provides a table up to 100) and 
//. use that hardcoded table (that's what gcc does internally for `powi`). 






#endif

//;

/*********************************************************************************************/
