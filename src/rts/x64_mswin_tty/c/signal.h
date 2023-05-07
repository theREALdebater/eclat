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

//: Standard signals

#ifndef __SIGNAL_H__
#define __SIGNAL_H__

//: Send a signal to self

int raise (int sig);

//. Sends signal `sig` to the current executing process.

//;
//: Send signal to any process







//;
//: Set a signal handler

void (*signal(int sig, void (*func)(int)))(int);

//. Specifies a way to handle the signals with the signal number specified by `sig`.
//. 
//. Parameter func specifies one of the three ways in which a signal can be handled by a 
//. program:
//. 
//.  * Default handling (`SIG_DFL`): The signal is handled by the default action for that 
//.    particular signal.
//. 
//.  * Ignore signal (`SIG_IGN`): The signal is ignored and the code execution will continue 
//.    even if not meaningful.
//. 
//.  * Function handler: A specific function is defined to handle the signal.
//. 
//. Either `SIG_DFL` or `SIG_IGN` is set as the default signal handling behavior at program 
//. startup for each of the supported signals. 
//. 
//. Returns pointer to the previous handler for the signal, or `SIG_ERR` if something was 
//. wrong. 

//. There are many things a signal handler function is not permitted to do by ECLAT. See 
//. ????? for more details. 

//;
//: Predefined Signals

//. These are the signals mandated by the C standard:

#define SIGABRT      ( 6)
#define SIGFPE       ( 8)
#define SIGILL       ( 4)
#define SIGINT       ( 2)
#define SIGSEGV      (11)
#define SIGTERM      (15)

//. The following signals come from POSIX and other places:

#define SIGALRM      (14)  // Alarm clock
#define SIGBUS       (  )  // Access to an undefined portion of a memory object
#define SIGCHLD      (  )  // Child process terminated, stopped, or continued [def ignore]
#define SIGCONT      (  )  // Continue executing, if stopped [def continue]
#define SIGHUP       ( 1)  // Hangup
#define SIGKILL      ( 9)  // Kill (cannot be caught or ignored)
#define SIGPIPE      (13)  // Write on a pipe with no one to read it
#define SIGPOLL      (  )  // Pollable event
#define SIGPROF      (  )  // Profiling timer expired
#define SIGQUIT      ( 3)  // Terminal quit signal
#define SIGSTOP      (  )  // Stop executing (cannot be caught or ignored) [def stop]
#define SIGSYS       (  )  // Bad system call
#define SIGTRAP      ( 5)  // Trace/breakpoint trap
#define SIGTSTP      (  )  // Terminal stop signal [def stop]
#define SIGTTIN      (  )  // Background process attempting read [def stop]
#define SIGTTOU      (  )  // Background process attempting write [def stop]
#define SIGUSR1      (  )  // User-defined signal 1
#define SIGUSR2      (  )  // User-defined signal 2
#define SIGURG       (  )  // Out-of-band data is available at a socket [def ignore]
#define SIGVTALRM    (  )  // Virtual timer expired
#define SIGXCPU      (  )  // CPU time limit exceeded
#define SIGXFSZ      (  )  // File size limit exceeded
#define SIGWINCH     (  )  // Terminal window size changed [def ignore]
#define SIGEMT       (  )  // emulator trap
#define SIGINFO      (  )  // status (info) request is received from controlling terminal
#define SIGPWR       (  )  // power failure
#define SIGLOST      (  )  // file lock lost
#define SIGSTKFLT    (  )  // coprocessor stack fault [def SIGFPE instead]
#define SIGUNUSED    (  )  // system call with unused system call number [def SIGSYS]
#define SIGCLD       SIGCHLD // synonym

//. Standard values that can be passed as the `sig` parameter in the `signal` and `raise` 
//. function:
//. 
//. | ID        | Name                               |
//. | --------- | ---------------------------------- |
//. | `SIGABRT` | Signal Abort                       |
//. | `SIGFPE`  | Signal Floating-Point Exception    |
//. | `SIGILL`  | Signal Illegal Instruction         |
//. | `SIGINT`  | Signal Interrupt                   |
//. | `SIGSEGV` | Signal Segmentation Violation      |
//. | `SIGTERM` | Signal Terminate                   |
//. 
//. ### Signal Abort
//. Abnormal termination, such as is initiated by the abort function.
//. 
//. ### Signal Floating-Point Exception
//. Erroneous arithmetic operation, such as zero divide or an operation resulting in overflow 
//. (not necessarily with a floating-point operation).
//. 
//. ### Signal Illegal Instruction
//. Invalid function image, such as an illegal instruction. This is generally due to a 
//. corruption in the code or to an attempt to execute data.
//. 
//. ### Signal Interrupt
//. Interactive attention signal. Generally generated by the application user.
//. 
//. ### Signal Segmentation Violation
//. Invalid access to storage: When a program tries to read or write outside the memory it has 
//. allocated.
//. 
//. ### Signal Terminate
//. Termination request sent to program.
//. 
//. ### Defaults
//. The default signal handling for the above signals is `SIG_DFL`, which terminates the 'self' 
//. process. 

//;
//: Pre-defined signal handler values

//. The values `SIG_DFL` and `SIG_IGN` can be passed into the `func` parameter of the `signal` 
//. function, as an alternative to passing a pointer to a specific signal handler function. 

#define SIG_DFL      (-1)
#define SIG_IGN      (0)

//. The value `SIG_DFL` tells the `signal` function to set the handler for the given signal to 
//. its default. 

//. The value `SIG_IGN` tells the `signal` function to set the handler for the given signal to 
//. a handler that does nothing. 

#define SIG_ERR      (-2)

//. The value `SIG_ERR` is returned by the `signal` function if something failed. 

//;
//: Atomic Type

//. Integral type of an object that can be accessed as an atomic entity, even in the presence 
//. of asynchronous signals.

typedef (__builtin(sig_atomic_t))    sig_atomic_t;

#define SIG_ATOMIC_MIN     (__builtin(sig_atomic_min))
#define SIG_ATOMIC_MAX     (__builtin(sig_atomic_max))

//. A value from `SIG_ATOMIC_MIN` through to `SIG_ATOMIC_MAX` (inclusive) may be read and 
//. written from a variable of type `volatile sig_atomic_t` with no possibility of another 
//. thread interrupting the read or write, and no possibility of the read or write being 
//. interleaved with any read or write of the same variable by another thread. 

//;
//: 







//;

#endif

//;

/*********************************************************************************************/

