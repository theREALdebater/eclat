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
 
//: Standard assertion

#ifndef __ASSERT_H__
#define __ASSERT_H__

//. Any prior definitions of `assert` and `static_assert` are overridden by this header.

#undef assert
#undef static_assert

//. The user indicates debugging is not required by defining `NDEBUG`. In this case, `assert` 
//. does nothing (but `static_assert` is unaffected).

#ifdef NDEBUG
#define assert(test) ((void)0)
#else
#define assert(test) __builtin(ASSERT(test,assert_failure(##test),assert_success(##test)))
#endif

//. According to the standard, `static_assert` is a declaration , whilst `assert` is a 
//. statement. However, the ECLAT builtin `ASSERT` is a declaration and is used for both. 

#define static_assert _Static_assert

//. Although the C standard distinguishes between 'static' assertion (done at compile time) and 
//. normal assertion (assumed to be done at run time), ECLAT cannot do so -- since there are 
//. two kinds of 'compile time' (actual compilation and realisation) -- and adopts the same 
//. strategy as everywhere there is this conundrum, that everything is technically dynamic, and 
//. will be optimised in release mode. Since ECLAT and the Realizor both perform compile-time 
//. execution in many places, it will often be the case that things which you might expect to 
//. fail only at run time are actually reported as failing by ECLAT or the Realizor. 
//. 
//. Thus, there is simply the builtin declaration `ASSERT`, for which there is no distinction 
//. between static and dynamic. It can take one or two parameters. The first parameter is an 
//. integer expression which evaluates to non-zero to indicate the assertion passes. The second 
//. parameter is a static text expression that is evaluated and reported back to the user if an 
//. assertion fails; if the second parameter is omitted, the message reported is the first 
//. parameter converted to text. 

#define _Static_assert( test, msg )    __builtin(ASSERT(test,assert_failure(msg),assert_success(msg)))
#define _Static_assert(test)        __builtin(ASSERT(test,assert_failure(##test),assert_success(##test)))

//. The message is reported to the user by calling `assert_failure(msg)`.

#define assert_failure( msg )     _Assert_failure( msg )
#define assert_success( msg )     _Assert_success( msg )

//. The default definitions of `assert_failure` and `assert_success` are to call the RTS  
//. functions `_Assert_failure` and `_Assert_success` respectively. The user can redefine 
//. `assert_failure` and `assert_success` in order to change the default behaviour. 

import void _Assert_failure(const char *msg);
import void _Assert_success(const char *msg);

//. The usual RTS implementation of `_Assert_failure` is to output "ASSERT FAILED: ", and then 
//. the message `msg`, and then end of line, to standard error. The usual RTS implementation of 
//. `_Assert_success` is to do nothing. 


#endif

//;

/*********************************************************************************************/

