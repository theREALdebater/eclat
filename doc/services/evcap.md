-----------------------------------------------------------------------------------------------
# Event Capture Service

The __Event Capture Service__ receives [events](../events/events.md) from a configurable set of
event channels and brokers, and uses an instantiation of the standard package
`Ada.Sequential_IO` to write the events into a file. 





The [event capture service](../services/evcap.md) uses an instantiation of the standard package
`Ada.Sequential_IO` to write events into a file. 



-----------------------------------------------------------------------------------------------
##








.....



-----------------------------------------------------------------------------------------------
##





-----------------------------------------------------------------------------------------------
##





-----------------------------------------------------------------------------------------------
## Log File Names

The full name (including the path and extension) of the log file is 

configured .....

If the log file name is not configured, it is 

taken from the value of the 
environment variable `ADAOS_LOGFILE`. 



????? each program does it's own auditing

Likewise, the full name of the audit file is taken from 
the value of the environment variable `ADAOS_AUDITFILE`.  






### Defaults

If the log file name is not configured, and there is not 
environment variable set (or is empty or blank), default values are used.

The default value for the logging file is:

    @{BASEDIR}/logs/@{PROGNAME}/@{DATE}-@{TIME}-@{SEQ}.dat







????? each program does it's own auditing

The default value for the auditing file is:

    @{BASEDIR}/audit/@{PROGNAME}/@{DATE}-@{TIME}-@{SEQ}.dat










### Interpolations

Within the value of any of these configuration settings or environment variables, the following
_interpolations_ can be used: 

 * `@{BASEDIR}` will be replaced by the initial current directory, as obtained by calling 
   `Ada.Directories.Current_Directory` when the program is initialised. This value will not 
   have a trailing path separator (forward or backward slash). 

 * `@{PROGNAME}` will be replaced by the program name, as obtained by calling 
   `Ada.Environment.Program_Name` (when the program is initialised). If this value is a path, 
   the path is removed. 

 * `@{PROGNAME:U}` will be replaced by the same as `@{PROGNAME}`, but converted to upper case. 

 * `@{PROGNAME:L}` will be replaced by the same as `@{PROGNAME}`, but converted to lower case. 

 * `@{DATE}` will be replaced by the date the file is created, in the format `YYYYMMDD`. 

 * `@{DATE:D-}` will be replaced by the date the file is created in the format `YYYY-MM-DD`. 

 * `@{TIME}` will be replaced by the time (of day) the file is created, accurate to the nearest 
   second, in the format `HHMMSS`. 

 * `@{TIME:D-}` will be replaced by the time the file is created in the format `HH-MM-SS`. 

 * `@{SEQ}` will be replaced by a number which is sufficient, at the time when the file is
   created, to distinguish the file from any others with the same stem; the _stem_ of a file
   name is the file name without its distinguishing number; if there is no other file with the
   same stem, the number 1 is used, otherwise, whichever stem has the highest number `n`, `n`+1
   is used; this macro can only be used once, and only in the base name of the file (not its
   path nor its extension); `@{SEQ:Wn}` is replaced by `n` digits, where `n` is between 1 and
   9, or more digits if necessary (if the number is greater than can be expressed by that
   number of digits), with leading zeroes if necessary; `@{SEQ}` is the same as `@{SEQ:W1}`. 

The name should be absolute, but if it is relative, it will be relative to the current
directory at the time the file is created. Since the current directory can change during
execution of the program, using a relative path is likely to be unwise. 



-----------------------------------------------------------------------------------------------
##





-----------------------------------------------------------------------------------------------
##





-----------------------------------------------------------------------------------------------
##





-----------------------------------------------------------------------------------------------
##





-----------------------------------------------------------------------------------------------
##





-----------------------------------------------------------------------------------------------
##





-----------------------------------------------------------------------------------------------
##





