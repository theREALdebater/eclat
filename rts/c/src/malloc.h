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









// _aligned_malloc

// __declspec(noalias)
// __declspec(restrict)
// void * _aligned_malloc(
    // size_t size,
    // size_t alignment
// );

#declare _aligned_offset_malloc(size,alignment,offset) __builtin(malloc(size,alignment,offset,???pool???))

#declare _aligned_malloc(size,alignment) _aligned_offset_malloc(size,alignment,0)

#declare _aligned_offset_realloc(pointer,size,alignment,offset) __builtin(realloc(pointer,size,alignment,offset,???pool???))

#declare _aligned_realloc(pointer,size,alignment) _aligned_offset_realloc(pointer,size,alignment,0)

#declare _aligned_free(pointer) __builtin(free(pointer))

#declare 

#declare malloc(size) _aligned_malloc(size,1)







//;
/*********************************************************************************************/

