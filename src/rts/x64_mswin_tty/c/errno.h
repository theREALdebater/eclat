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

//: Standard error flag

#ifndef __ERRNO_H__
#define __ERRNO_H__

import int * __ECLAT__errno(void);
import int __ECLAT__EDOM(void);
import int __ECLAT__EILSEQ(void);
import int __ECLAT__ERANGE(void);

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




//: Standard errors

#define E2BIG              ( 0) // Argument list too long
#define EACCES             () // Permission denied
#define EADDRINUSE         () // Address in use
#define EADDRNOTAVAIL      () // Address not available
#define EAFNOSUPPORT       () // Address family not supported
#define EAGAIN             () // Resource unavailable, try again
#define EALREADY           () // Connection already in progress
#define EBADF              () // Bad file descriptor
#define EBADMSG            () // Bad message
#define EBUSY              () // Device or resource busy
#define ECANCELED          () // Operation canceled
#define ECHILD             () // No child processes
#define ECONNABORTED       () // Connection aborted
#define ECONNREFUSED       () // Connection refused
#define ECONNRESET         () // Connection reset
#define EDEADLK            () // Resource deadlock would occur
#define EDESTADDRREQ       () // Destination address required
#define EDOM               () // Mathematics argument out of domain of function
#define EEXIST             () // File exists
#define EFAULT             () // Bad address
#define EFBIG              () // File too large
#define EHOSTUNREACH       () // Host is unreachable
#define EIDRM              () // Identifier removed
#define EILSEQ             () // Illegal byte sequence
#define EINPROGRESS        () // Operation in progress
#define EINTR              () // Interrupted function
#define EINVAL             () // Invalid argument
#define EIO                () // I/O error
#define EISCONN            () // Socket is connected
#define EISDIR             () // Is a directory
#define ELOOP              () // Too many levels of symbolic links
#define EMFILE             () // File descriptor value too large
#define EMLINK             () // Too many links
#define EMSGSIZE           () // Message too large
#define ENAMETOOLONG       () // Filename too long
#define ENETDOWN           () // Network is down
#define ENETRESET          () // Connection aborted by network
#define ENETUNREACH        () // Network unreachable
#define ENFILE             () // Too many files open in system
#define ENOBUFS            () // No buffer space available
#define ENODATA            () // No message is available on the STREAM head read queue
#define ENODEV             () // No such device
#define ENOENT             () // No such file or directory
#define ENOEXEC            () // Executable file format error
#define ENOLCK             () // No locks available
#define ENOLINK            () // Link has been severed
#define ENOMEM             () // Not enough space
#define ENOMSG             () // No message of the desired type
#define ENOPROTOOPT        () // Protocol not available
#define ENOSPC             () // No space left on device
#define ENOSR              () // No STREAM resources
#define ENOSTR             () // Not a STREAM
#define ENOSYS             () // Function not supported
#define ENOTCONN           () // The socket is not connected
#define ENOTDIR            () // Not a directory
#define ENOTEMPTY          () // Directory not empty
#define ENOTRECOVERABLE    () // State not recoverable
#define ENOTSOCK           () // Not a socket
#define ENOTSUP            () // Not supported
#define ENOTTY             () // Inappropriate I/O control operation
#define ENXIO              () // No such device or address
#define EOPNOTSUPP         () // Operation not supported on socket
#define EOVERFLOW          () // Value too large to be stored in data type
#define EOWNERDEAD         () // Previous owner died
#define EPERM              () // Operation not permitted
#define EPIPE              () // Broken pipe
#define EPROTO             () // Protocol error
#define EPROTONOSUPPORT    () // Protocol not supported
#define EPROTOTYPE         () // Protocol wrong type for socket
#define ERANGE             () // Result too large
#define EROFS              () // Read-only file system
#define ESPIPE             () // Invalid seek
#define ESRCH              () // No such process
#define ETIME              () // Stream ioctl() timeout
#define ETIMEDOUT          () // Connection timed out
#define ETXTBSY            () // Text file busy
#define EWOULDBLOCK        () // Operation would block
#define EXDEV              () // Cross-device link 

//;

#endif

//;

/*********************************************************************************************/

