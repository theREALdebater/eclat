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

import char* strerror( int errnum )
{
   if (errnum < 0 || errnum > errors_max)
      return "Unknown error";
   
   return errors[ errnum ];
}

//. The function `strerror` returns a string describing the error number in argument `errnum`.

import errno_t strerror_s( char *buf, rsize_t bufsz, errno_t errnum )
{
   
}

//. The function `strerror_s` copies a description of the error number in argument `errnum` 
//. into `buf` limited to `bufsz` bytes. 

import size_t strerrorlen_s( errno_t errnum )
{
   
}

//. The function `strerrorlen_s` returns the length, in bytes, of a description of the error
//. number in argument `errnum`. 

//;















//: Standard errors

static int errors_max = 78;

static char * errors[] = 
{
   "Argument list too long", // E2BIG
   "Permission denied", // EACCES
   "Address in use", // EADDRINUSE
   "Address not available", // EADDRNOTAVAIL
   "Address family not supported", // EAFNOSUPPORT
   "Resource unavailable, try again", // EAGAIN
   "Connection already in progress", // EALREADY
   "Bad file descriptor", // EBADF
   "Bad message", // EBADMSG
   "Device or resource busy", // EBUSY
   "Operation canceled", // ECANCELED
   "No child processes", // ECHILD
   "Connection aborted", // ECONNABORTED
   "Connection refused", // ECONNREFUSED
   "Connection reset", // ECONNRESET
   "Resource deadlock would occur", // EDEADLK
   "Destination address required", // EDESTADDRREQ
   "Mathematics argument out of domain of function", // EDOM
   "File exists", // EEXIST
   "Bad address", // EFAULT
   "File too large", // EFBIG
   "Host is unreachable", // EHOSTUNREACH
   "Identifier removed", // EIDRM
   "Illegal byte sequence", // EILSEQ
   "Operation in progress", // EINPROGRESS
   "Interrupted function", // EINTR
   "Invalid argument", // EINVAL
   "I/O error", // EIO
   "Socket is connected", // EISCONN
   "Is a directory", // EISDIR
   "Too many levels of symbolic links", // ELOOP
   "File descriptor value too large", // EMFILE
   "Too many links", // EMLINK
   "Message too large", // EMSGSIZE
   "Filename too long", // ENAMETOOLONG
   "Network is down", // ENETDOWN
   "Connection aborted by network", // ENETRESET
   "Network unreachable", // ENETUNREACH
   "Too many files open in system", // ENFILE
   "No buffer space available", // ENOBUFS
   "No message is available on the STREAM head read queue", // ENODATA
   "No such device", // ENODEV
   "No such file or directory", // ENOENT
   "Executable file format error", // ENOEXEC
   "No locks available", // ENOLCK
   "Link has been severed", // ENOLINK
   "Not enough space", // ENOMEM
   "No message of the desired type", // ENOMSG
   "Protocol not available", // ENOPROTOOPT
   "No space left on device", // ENOSPC
   "No STREAM resources", // ENOSR
   "Not a STREAM", // ENOSTR
   "Function not supported", // ENOSYS
   "The socket is not connected", // ENOTCONN
   "Not a directory", // ENOTDIR
   "Directory not empty", // ENOTEMPTY
   "State not recoverable", // ENOTRECOVERABLE
   "Not a socket", // ENOTSOCK
   "Not supported", // ENOTSUP
   "Inappropriate I/O control operation", // ENOTTY
   "No such device or address", // ENXIO
   "Operation not supported on socket", // EOPNOTSUPP
   "Value too large to be stored in data type", // EOVERFLOW
   "Previous owner died", // EOWNERDEAD
   "Operation not permitted", // EPERM
   "Broken pipe", // EPIPE
   "Protocol error", // EPROTO
   "Protocol not supported", // EPROTONOSUPPORT
   "Protocol wrong type for socket", // EPROTOTYPE
   "Result too large", // ERANGE
   "Read-only file system", // EROFS
   "Invalid seek", // ESPIPE
   "No such process", // ESRCH
   "Stream ioctl() timeout", // ETIME
   "Connection timed out", // ETIMEDOUT
   "Text file busy", // ETXTBSY
   "Operation would block", // EWOULDBLOCK
   "Cross-device link ", // EXDEV
}
   
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

