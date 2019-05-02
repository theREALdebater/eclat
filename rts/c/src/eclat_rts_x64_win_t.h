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

//: ECLAT Run Time Support header file for the C language

//. Architecture:   x86-64
//. Platform:       Microsoft(R) Windows(R)
//. Type:           Text
//. 
//. The appropriate `<rts/eclat_rts_*_*_*.h>` header is included automatically by ECLAT.

#ifndef __ECLAT_RTS_H__
#define __ECLAT_RTS_H__

//: Assertion

//. Although the C standard distinguished between 'static' assertion (done at compile time) and 
//. normal assertion (assumed to be doen at run time), ECLAT cannot do so -- since there are 
//. two kinds of 'compile time' (actual compilation and realisation) -- and adopts the same 
//. strategy as everywhere there is this conundrum, that everything is technically dynamic, and 
//. will be optimised in release mode. Since ECLAT and the Realizor both perform compile-time 
//. execution in many places, it will often be the case that things which you might expect to 
//. fail only at run time are actually reported as failing by ECLAT or the Realizor. 
//. 
//. Thus, there is simply the builtin operation `ASSERT`, for which there is no distinction 
//. between static and dynamic. It can take just one parameter, an integer expression which 
//. evaluates to non-zero to indicate the assertion passes, or two. The second parameter is
//. a static text expression that is evaluate and reported back to the user if an assertion 
//. fails.

#define _Static_assert(test,msg)    __builtin(ASSERT(test,msg))
#define _Static_assert(test)        __builtin(ASSERT(test))

//;













#endif

//;
/*********************************************************************************************/

