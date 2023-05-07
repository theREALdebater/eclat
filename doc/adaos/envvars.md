-----------------------------------------------------------------------------------------------
# Environment Variables

.....

It is recommended that the character 'NUL' (whose code point is value 0) is never used in any
environment variable value. The character 'NUL' is used as a string terminator in some
programming languages; an embedded 'NUL' is likely to cause premature string termination in
such languages. 

.....


The Open Group Base Specifications Issue 7, 2018 edition
IEEE Std 1003.1-2017 (Revision of IEEE Std 1003.1-2008)
Copyright (c) 2001-2018 IEEE and The Open Group

https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap08.html



-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## Names

.....

?????..... case sensitive (an upper case roman letter is considered different to its lower case counterpart) .....





-----------------------------------------------------------------------------------------------
## .....

All environment variables are kept, by AdaOS, in a 

????? [system object](../objects/objects.md) in
the class of the interface type `Environment_Variable_Map` 



```ada
package Environment_Variable_Maps is
   new Ada.Containers.Indefinite_Hashed_Maps (Unbounded_Wide_Wide_String, 
                                              Environment_Variable'Class);

function Environment_Variables (Compartment: access Program_Compartment) 
return 
   Environment_Variable_Maps.Map;

procedure Set_Environment_Variables (Compartment: access Program_Compartment;
                                     Map:         in     Environment_Variable_Maps.Map);
```





(declared in the package
`AdaOS.Compartments`). 

.....



-----------------------------------------------------------------------------------------------
## Types of Environment Variable {#typ}

All environment variables have or return a value of type `Wide_Wide_String`. Most are a simple store that
stores a string upon writing and retrieves that string upon reading. 



In the package `AdaOS.Environment_Variables`, 


```ada
type Environment_Variable is abstract tagged private;
```



However, some environment variables are _dynamic environment variables_, meaning that the value
returned is computed rather than just being retrieved, and that setting the variable may not be
as simple as storing the value. In some cases setting the variable is not allowed at all. 

To accommodate this, under AdaOS, every environment variable is represented by a 
value of the abstract tagged private type `Environment_Variable`. 

This type has a _getter function_ named `Value`. 

```ada
function Value (Variable: in Environment_Variable; 
                Name:     in Wide_Wide_String) return Wide_Wide_String is abstract;
```


```ada
type Updatable_Variable is abstract new Environment_Variable with private;
```





There is also an abstract tagged limited private type `Updatable_Variable`, which is derived
from `Environment_Variable` but adds a _setter procedure_, named `Set_Value`. 

```ada
procedure Set_Value (Variable: in Updatable_Variable; 
                     Name:     in Wide_Wide_String;
                     Value:    in Wide_Wide_String) is abstract;

procedure Clear (Variable: in Updatable_Variable; 
                 Name:     in Wide_Wide_String) is abstract;
```

.....



-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## Compartment Environment Variables

.....



Compartments have (primitive operations that are) functions which create new environment
variables that are suitable to the actual environment of the compartment. 

These are called _environment variable constructor_ functions. 

.....


### Shadow Environment Variables {#shad}

For compartments of [hosted platforms](?????), most of the environment variables need to be (in
effect) those of the host platform. 

This is done by having those environment variables represented by objects of types that are
derived from `Environment_Variables` but whose getter gets the value of the corresponding host
environment variable (of the same name), and, if appropriate, whose setter sets the value of
the corresponding host environment variable. 

This (type of) environment variable is called a _shadow_ environment variable (type). 

Typically, some of the environment variables that are provided by AdaOS but not by the host
platform continue to be implemented in the usual way. 

The AdaOS Native platform has to implement all the environment variables itself. 



-----------------------------------------------------------------------------------------------
## Monitoring {#mon}

All environment variables generate [events](../events/events.md) .....

.....







-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 








-----------------------------------------------------------------------------------------------
## Stored Environment Variables

.....



In the package `AdaOS.Environment_Variables`, 

```ada
type Stored_Variable is abstract new Updatable_Variable with private;
```

The `Set_Value` procedure stores the given `Value`. The `Value` function retrieves that value.
The `Clear` procedure (effectively) sets the value to the empty string. 


### Monitoring

The `Set_Value` and `Clear` procedures send a [monitoring event](#mon) of the type
`Stored_Variable_Update` to the event channel `AdaOS.Environment_Variables.Stored.name`, where
`name` is the name of the environment variable.

```ada
type Stored_Variable_Update is new System_Event
with
   record
      Old_Value, New_Value: Wide_Wide_Unbounded_String;
      ?????Compartment: Compartment_Access;
   end record;
```

For procedure `Clear`, the `New_Value` is the empty string. 

This event is sent as a result of calling the environment variable constructor
`New_Stored_Variable`. In practice, this will be because the constructor will itself call
`Set_Value` to initialise the variable. 


### Constructor

Compartments provide the following environment variable constructor:

```ada
function New_Stored_Variable (Compartment: access Program_Compartment;
                              Initial_Value: in Wide_Wide_String := "") 
return 
   Stored_Variable'Class;
```

The function `New_Stored_Variable` returns a new stored environment variable, which can be
initialised to a given value (but defaults to being initialised to the empty string). 
















-----------------------------------------------------------------------------------------------
## Error Level Environment Variables ????? Is this actually useful or sensible?

.....

In the package `AdaOS.Environment_Variables`, 

```ada
type Error_Level_Variable is new Updatable_Variable with private;
```
.....

By default, the environment variable named `ERRORLEVEL` is of this type. 










-----------------------------------------------------------------------------------------------
## Random Numbers and Strings

The environment variables named `RANDOM_MIN` and `RANDOM_MAX` hold, respectively, the lowest
and highest (in value) random number that can be returned by the `RANDOM_INT` environment
variable. The default values of these environment variables are, respectively, 0 and 32767. 

Whenever the environment variable named `RANDOM_INT` is read, its value is a (different)
randomly chosen decimal integer between the values of `RANDOM_MIN` and `RANDOM_MAX`
(inclusive). 

The value of all three of these environment variables is expressed as a decimal integer, with
no signs, punctuation, or spaces. 

The dynamic environment variable `RANDOM_INT` cannot be set. The variables `RANDOM_MIN` and
`RANDOM_MAX` can be set, but if the value given is not syntactically correct (a decimal
integer, with no signs, punctuation, or spaces), the exception ????? is propagated. 

If the value of `RANDOM_MIN` is greater than that of `RANDOM_MAX`, then reading `RANDOM_INT` will propagate the exception ?????

.....

`RANDOM_FORMAT`

.....

The following characters in the format string are replaced: 

| `0` | decimal digit from `0` to `9`                                     |
| `9` | decimal digit from `1` to `9`, deleted if the digit would be `0` and has no decimal digit to its left |
| `!` | upper case hexadecimal digit from `0` to `9` or `A` to `F`        |
| `?` | upper case Roman letter from `A` to `Z`                           |

Any other character is literal; it is not replaced.

The dynamic environment variable named `RANDOM` returns: 

 * If `RANDOM_FORMAT` is not set or is empty, the same result as `RANDOM_INT`.

 * Otherwise, the value of `RANDOM_FORMAT`, but with its replacement characters replaced as
   described below. 

The way replacement works is as follows: 

 1. An integer random value is obtained by calling the getter of `RANDOM_INT` and put into a
    variable `n` say; 

 2. For each replacement character, working right-to-left, apply the appropriate replacement
    algorithm (see just below). 

For a decimal replacement digit:

 1. Calculate the value of the digit as `n` modulo 10. For 0, the replacement is `0`, and so on
    up to 9, for which the replacement is `9`. For 0, if the replacement character is a `9`
    *and* there is no `9` replacement character to its left that is being replaced by a
    non-zero character, the replacement is to delete the character.

 2. Change `n` to be the (rounded-down) result of dividing it by 10. 

For a hexadecimal (`!`) replacement digit:

 1. Calculate the value of the digit as `n` modulo 16. For 0, the replacement is `0`, and so on
    up to 9, for which the replacement is `9`, for 10 the replacement is `A`, for 11 it is `B`,
    and so on up to 15, for which it is `F`. 

 2. Change `n` to be the (rounded-down) result of dividing it by 16. 

For a letter (`?`) replacement digit:

 1. Calculate the value of the digit as `n` modulo 26. For 0, the replacement is `A`, for 1 the
    replacement is `B`, and so on up to 25, for which the replacement is `Z`. 

 2. Change `n` to be the (rounded-down) result of dividing it by 26. 

It is recommended that no more than three `?` are used adjacent to each other, because of the
possibility of the random formation of unfortunate words. 

For example, if `RANDOM_FORMAT` is set to: 

    mx??000

the result of `RANDOM` (if `RANDOM_INT` returned `4172`) might be: 

    mxAE172

or (if `RANDOM_INT` returned `23`): 

    mxAA023

For another example, if `RANDOM_FORMAT` is set to

    99999.ABC

the result of `RANDOM` (if `RANDOM_INT` returned `4172`) might be

    4172.ABC

or (if `RANDOM_INT` returned `23`): 

    23.ABC



### Types

.....

In the package `AdaOS.Environment_Variables`, 

```ada
type Random_Number is range <a very big integer range>;

type Random_Number_Generator is abstract tagged private;

type Random_Minimum_Variable (Generator: access Random_Number_Generator'Class) 
is 
   new Updatable_Variable with private;

type Random_Maximum_Variable (Generator: access Random_Number_Generator'Class) 
is 
   new Updatable_Variable with private;

type Random_Format_Variable (Generator: access Random_Number_Generator'Class) 
is 
   new Updatable_Variable with private;

type Random_Integer_Variable (Generator: access Random_Number_Generator'Class) 
is 
   new Environment_Variable with private;

type Random_String_Variable (Generator: access Random_Number_Generator'Class) 
is 
   new Environment_Variable with private;
```

### Constructors

Compartments provide the following environment variable constructor:

```ada
function New_Random_Minimum_Variable (Compartment:   access Program_Compartment;
                                      Generator:     access Random_Number_Generator'Class;
                                      Initial_Value: in ????? := 0) 
return 
   Random_Minimum_Variable'Class;

```

The function `New_Random_Minimum_Variable` returns a new environment variable of a type in
`Random_Minimum_Variable'Class`, which can be .....



### Conventional Names

By default, there are environment variables with the following names and types: 

| Variable Name             | Type Name                     |
| ------------------------- | ----------------------------- |
| `RANDOM_INT`              | `Random_Integer_Variable`     |
| `RANDOM_MIN`              | `Random_Minimum_Variable`     |
| `RANDOM_MAX`              | `Random_Maximum_Variable`     |
| `RANDOM_FORMAT`           | `Random_Format_Variable`      |
| `RANDOM`                  | `Random_String_Variable`      |



-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## Current Working Directory {#cwd}



Reading the [dynamic environment variable](#dyn) `PWD` returns the full, absolute path of the
[current working directory](compart.md#cwd), or CWD for short. 

Setting `PWD` to a path (absolute or relative, in which case it is relative to the CWD) in
order to change the CWD. 

If this variable is set to a new path, that value must be a path of a directory that exists
and to which the compartment has 
[permission](../security/?????) to access fully ?????

The variables `CD`, `CWD` (also dynamic environment variables) are links to `PWD` and so are an 
an alias of it. Either of these variables can be retrieved and set. When either one is set to a 
particular value, all three will be automatically set to that value. 

The name `PWD` comes from the old Unix command `pwd`, which stood for 'print working
directory'. Similarly, `CD` comes from the command `cd`, short for change directory. 



.....

In the package `AdaOS.Environment_Variables`, 

```ada
type Current_Directory_Variable is abstract new Updatable_Variable with private;
```






By default, the environment variables named `PWD`, `CWD`, and `CD` are in this type's class. 

..... `Windows_Current_Directory_Variable` ..... 

..... `Linux_Current_Directory_Variable` ..... 

..... `Native_Current_Directory_Variable` ..... 




-----------------------------------------------------------------------------------------------
## Temporary Files Directory {#tmpdir}

The environment variable `TMPDIR` contains the [absolute full path](../objects/paths.md) of a
directory that can be used to create named system objects. 

On the [AdaOS Native platform](../pxcr/targets.md#plat), each different compartment has its own
temporary directory, which is initially empty, and when the compartment everything in its
temporary directory is automatically deleted (along with the directory itself and indeed the
entire compartment). 

On a hosted platform, what the 

When set, the temporary directory is changed to be the directory indicated by resolving the 
value to which the variable is set. If the value cannot be resolved to an object that is a 
directory, and to which the compartment has full permission, the exception 
`AdaOS.Usage_Error` is propagated (and the variable is not changed). 

The variables `TEMP`, `TEMPDIR`, and `TMP` are links to `TMPDIR` and so are an an alias of it. 
Any of these variables can be retrieved and set. When any one is set to a particular value, 
all will be automatically set to that value. 

The value of this variable is typically:

| Platform      | Directory                             |
| ------------- | ------------------------------------- |
| Windows       | `C:\Users\%USER%\AppData\Roaming`     |
| Linux/FHS     | `/tmp`                       |
| AdaOS Native  | `/temp`                               |

where `%USER%` is the value of the environment variable `USER`, and `${HOME}` is the value of
the environment variable `HOME`. These values may vary. 


[Compartment Temporary Directory](../objects/paths.md#cmpttemp)

[Module Instance Temporary Directory](../objects/paths.md#modtemp)










-----------------------------------------------------------------------------------------------
## Home Directory

The variable `HOME` .....

The variable `HOMEDRIVE` .....

The variable `HOMEPATH` .....

......


[Program Shared Directory](../objects/paths.md#progshar)

[Module Shared Directory](../objects/paths.md#modshar)








-----------------------------------------------------------------------------------------------
## Current User {#user}

The variable `USER` .....

??????The variable `SESSIONAME` ..... 

The variable `USERDOMAIN` .....

The variable `USERNAME` .....

The variable `USERPROFILE` .....

The variable `ROLE` .....





-----------------------------------------------------------------------------------------------
## Ambit {#amb}

On the [AdaOS Native platform](../pxcr/targets.md#plat), the [dynamic environment
variable](#dyn) named `AMBIT` needs to be set. 

It should specify the authorities of the compartment's [ambit](../security/security.md#amb)
expressed as a sequence of authority numbers separated from each other by the ????? path
separator character. 

An authority number is a non-negative decimal integer with no signs, spaces, or punctuation. 

The compartment's [base authority](../security/security.md#baseauth), is the first (or only)
authority in the ambit.

If this environment variable is not set, or its value is empty, then the ambit will comprise
only the authority `0` (the top authority) and the base authority will be the authority `0`.  

The order of the numbers is otherwise insignificant (and they might be re-ordered, except for
the first). Any duplicates are ignored (and might be removed). 

Since, on hosted platforms, the top authority (number 0) is the authority used throughout, it 
makes sense to simply not set this environment variable on hosted platforms. 



-----------------------------------------------------------------------------------------------
## Date and Time

The [dynamic environment variable](#dyn) named `DATETIME` returns the current system (clock)
date and time, as a string, adjusted to be 'wall time'. 

Wall time is .....

If both the `LANG` and `LANGSPEC` variables are not set

`LC_TIME` and `LC_ALL`

, so that the default locale is in 
force, the value is in the format:

?????    2021-06-12 14:15:16.789
    2021-06-12_14:15:16.789
    
where 2021 is the year, 06 is the month (June), and 12 is the day of the month, 14 is the hour 
of the day (2 pm), 15 the minute of the hour, and 16.789 the second of the minute (rounded to 
the nearest millisecond). 

?????One space (code 32) character separates the date and time-of-day 
parts. 

Otherwise, the format is in accordance with the locale dictated by the value of the `LANG` or 
the `LANGSPEC` variable. 

The dynamic environment variable `DATE` returns only the date part (year, month, and day) of
`DATETIME`, for example: 

    2021-06-12

The dynamic environment variable `TIME` returns only the time-of-day part (hours, minute, and
seconds), for example: 

    14:15:16.789

These variables (`DATE` and `TIME`) cannot be set.


?????`YEAR`, `MONTH`, `DAY`, `HOUR`, `MINUTE`, `SECONDS`, `MONTH_NAME`, `SECONDS_OF_DAY`, `DAY_OF_YEAR`, `WEEK_NUM`











The dynamic environment variable `CLOCK_MJD` returns the current value of the [system clock](?????), in seconds as a decimal
number, with the MJD epoch. The dynamic environment variable `CLOCK_JULIAN_DATE` and `CLOCK_JD` both return the
same but with the JD epoch. 

The variable `TIME_OFFSET` should contain a decimal number ..... This value is the number of
seconds that should be added (or subtracted if it is a negative value) from the system clock to
get 'wall time'. This value will be changed if the [locale](#locale) time zone (`variable `TZ`)
is changed, to correspond to the offset implied by the time zone (together with daylight saving
time or any other adjustment that will be in effect). 

?????The variables `TIME_ADJUST_FREQ`, `TIME_ADJUST_START`, `TIME_ADJUST_AMOUNT`, and
`TIME_ADJUST_COUNT` have values that show .....





-----------------------------------------------------------------------------------------------
## Locale

The [monitored environment variable](#mon) `` .....

LANG, LC_ALL, LC_COLLATE, LC_CTYPE, LC_MESSAGES, LC_MONETARY, LC_NUMERIC, LC_TIME,

$LANG is used to set to the default locale. For example, if the locale values are pt_BR, then the language is set to (Brazilian) Portuguese and Brazilian practice is used where relevant. 

Different aspects of localization are controlled by individual $LC_-variables ($LC_CTYPE, $LC_COLLATE, $LC_DATE etc.). $LC_ALL can be used to force the same locale for all aspects.


LANGSPEC



$TZ
Refers to time zone. It can be in several formats, either specifying the time zone itself or referencing a file (in /usr/share/zoneinfo).


LANG=en_US.UTF-8
LANGUAGE=en_US
LC_CTYPE="en_US.UTF-8"
LC_NUMERIC=ro_RO.UTF-8
LC_TIME=ro_RO.UTF-8
LC_COLLATE="en_US.UTF-8"
LC_MONETARY=ro_RO.UTF-8
LC_MESSAGES="en_US.UTF-8"
LC_PAPER=ro_RO.UTF-8
LC_NAME=ro_RO.UTF-8
LC_ADDRESS=ro_RO.UTF-8
LC_TELEPHONE=ro_RO.UTF-8
LC_MEASUREMENT=ro_RO.UTF-8
LC_IDENTIFICATION=ro_RO.UTF-8
LC_ALL=


Users may use the following environment variables to announce specific localization requirements to applications. Applications can retrieve this information using the setlocale() function to initialize the correct behavior of the internationalized interfaces. The descriptions of the internationalization environment variables describe the resulting behavior only when the application locale is initialized in this way. The use of the internationalization variables by utilities described in the Shell and Utilities volume of POSIX.1-2017 is described in the ENVIRONMENT VARIABLES section for those utilities in addition to the global effects described in this section.

LANG
This variable shall determine the locale category for native language, local customs, and coded character set in the absence of the LC_ALL and other LC_* (LC_COLLATE, LC_CTYPE, LC_MESSAGES, LC_MONETARY, LC_NUMERIC, LC_TIME) environment variables. This can be used by applications to determine the language to use for error messages and instructions, collating sequences, date formats, and so on.
LC_ALL
This variable shall determine the values for all locale categories. The value of the LC_ALL environment variable has precedence over any of the other environment variables starting with LC_ (LC_COLLATE, LC_CTYPE, LC_MESSAGES, LC_MONETARY, LC_NUMERIC, LC_TIME) and the LANG environment variable.
LC_COLLATE
This variable shall determine the locale category for character collation. It determines collation information for regular expressions and sorting, including equivalence classes and multi-character collating elements, in various utilities and the strcoll() and strxfrm() functions. Additional semantics of this variable, if any, are implementation-defined.
LC_CTYPE
This variable shall determine the locale category for character handling functions, such as tolower(), toupper(), and isalpha(). This environment variable determines the interpretation of sequences of bytes of text data as characters (for example, single as opposed to multi-byte characters), the classification of characters (for example, alpha, digit, graph), and the behavior of character classes. Additional semantics of this variable, if any, are implementation-defined.
LC_MESSAGES
This variable shall determine the locale category for processing affirmative and negative responses and the language and cultural conventions in which messages should be written. It also affects the behavior of the catopen() function in determining the message catalog. Additional semantics of this variable, if any, are implementation-defined. The language and cultural conventions of diagnostic and informative messages whose format is unspecified by POSIX.1-2017 should be affected by the setting of LC_MESSAGES.
LC_MONETARY
This variable shall determine the locale category for monetary-related numeric formatting information. Additional semantics of this variable, if any, are implementation-defined.
LC_NUMERIC
This variable shall determine the locale category for numeric formatting (for example, thousands separator and radix character) information in various utilities as well as the formatted I/O operations in printf() and scanf() and the string conversion functions in strtod(). Additional semantics of this variable, if any, are implementation-defined.
LC_TIME
This variable shall determine the locale category for date and time formatting information. It affects the behavior of the time functions in strftime(). Additional semantics of this variable, if any, are implementation-defined.
NLSPATH
This variable shall contain a sequence of templates that the catopen() function uses when attempting to locate message catalogs. Each template consists of an optional prefix, one or more conversion specifications, a pathname, and an optional suffix.
For example:

NLSPATH="/system/nlslib/%N.cat"

defines that catopen() should look for all message catalogs in the directory /system/nlslib, where the catalog name should be constructed from the name parameter passed to catopen() ( %N ), with the suffix .cat.

Conversion specifications consist of a '%' symbol, followed by a single-letter keyword. The following keywords are currently defined:

%N
The value of the name parameter passed to catopen().
%L
The value of the LC_MESSAGES category.
%l
The language element from the LC_MESSAGES category.
%t
The territory element from the LC_MESSAGES category.
%c
The codeset element from the LC_MESSAGES category.
%%
A single '%' character.
An empty string is substituted if the specified value is not currently defined. The separators <underscore> ( '_' ) and <period> ( '.' ) are not included in the %t and %c conversion specifications.

Templates defined in NLSPATH are separated by <colon> characters ( ':' ). A leading or two adjacent <colon> characters ( "::" ) is equivalent to specifying %N. For example:

NLSPATH=":%N.cat:/nlslib/%L/%N.cat"

indicates to catopen() that it should look for the requested message catalog in name, name.cat, and /nlslib/category/name.cat, where category is the value of the LC_MESSAGES category of the current locale.

Users should not set the NLSPATH variable unless they have a specific reason to override the default system path. Setting NLSPATH to override the default system path produces undefined results in the standard utilities and in applications with appropriate privileges.

The environment variables LANG, LC_ALL, LC_COLLATE, LC_CTYPE, LC_MESSAGES, LC_MONETARY, LC_NUMERIC, LC_TIME, and NLSPATH provide for the support of internationalized applications. The standard utilities shall make use of these environment variables as described in this section and the individual ENVIRONMENT VARIABLES sections for the utilities. If these variables specify locale categories that are not based upon the same underlying codeset, the results are unspecified.

The values of locale categories shall be determined by a precedence order; the first condition met below determines the value:

If the LC_ALL environment variable is defined and is not null, the value of LC_ALL shall be used.

If the LC_* environment variable (LC_COLLATE, LC_CTYPE, LC_MESSAGES, LC_MONETARY, LC_NUMERIC, LC_TIME) is defined and is not null, the value of the environment variable shall be used to initialize the category that corresponds to the environment variable.

If the LANG environment variable is defined and is not null, the value of the LANG environment variable shall be used.

If the LANG environment variable is not set or is set to the empty string, the implementation-defined default locale shall be used.

If the locale value is "C" or "POSIX", the POSIX locale shall be used and the standard utilities behave in accordance with the rules in POSIX Locale for the associated category.

If the locale value begins with a <slash>, it shall be interpreted as the pathname of a file that was created in the output format used by the localedef utility; see OUTPUT FILES under localedef. Referencing such a pathname shall result in that locale being used for the indicated category.

[XSI] [Option Start] If the locale value has the form:

language[_territory][.codeset]

it refers to an implementation-provided locale, where settings of language, territory, and codeset are implementation-defined.

LC_COLLATE, LC_CTYPE, LC_MESSAGES, LC_MONETARY, LC_NUMERIC, and LC_TIME are defined to accept an additional field @ modifier, which allows the user to select a specific instance of localization data within a single category (for example, for selecting the dictionary as opposed to the character ordering of data). The syntax for these environment variables is thus defined as:

[language[_territory][.codeset][@modifier]]

For example, if a user wanted to interact with the system in French, but required to sort German text files, LANG and LC_COLLATE could be defined as:

LANG=Fr_FR
LC_COLLATE=De_DE

This could be extended to select dictionary collation (say) by use of the @ modifier field; for example:

LC_COLLATE=De_DE@dict

[Option End]
An implementation may support other formats.

If the locale value is not recognized by the implementation, the behavior is unspecified.

These environment variables are used by the newlocale() and setlocale() functions, and by the standard utilities.

Additional criteria for determining a valid locale name are implementation-defined.


-----------------------------------------------------------------------------------------------
## Display Unit

The variable `DISPLAY` contains the identifier for the visual display unit that X11 programs should use by default.

The variable `TERM` .....

The variable `` .....



-----------------------------------------------------------------------------------------------
## Browser

$BROWSER
Contains a colon-separated list of a user's web browser preferences, for use by programs that need to allow the user to view content at a URL. The browsers in the list are intended to be attempted from first to last, stopping after the first one that succeeds. This arrangement allows for fallback behavior in different environments, e.g., in an X11 environment, a graphical browser (such as Firefox) can be used, but in a console environment a terminal-base browser (such a Lynx) can be used. A %s token may be present to specify where the URL should be placed; otherwise the browser should be launched with the URL as the first argument.[5][6][7][8][9]



-----------------------------------------------------------------------------------------------
## `PATH`

On hosted platforms, the `PATH` variable should include the ECLAT tools `bin` directory, so 
that those tools can be executed (without having to give the full path to them every time). 

The `PATH` variable does not have any special meaning or purpose on the AdaOS Native platform. 

In Ada, within the package `Ada.Environment.Execution`, the function `Path_List_Separator` 
returns the character which separates paths within the `PATH` environment variable. On the 
Windows platform it returns `;` semicolon, whilst on Linux (and all Unix-based) platforms it 
returns `:` colon. For convenience, the function `Program_Paths_Count` returns how many paths 
there are in the `PATH` environment variable, and the function `Program_Path` returns a 
numbered path. On the AdaOS Native platform, all three of these functions propagate the 
exception `AdaOS.Usage_Error`. 

On hosted platforms, the `PATHEXT` variable should include .....




 The paths should include:
 
     ${HOME}/.local/bin










-----------------------------------------------------------------------------------------------
## Default Shell

The variable `SHELL` and `COMSPEC` .....







-----------------------------------------------------------------------------------------------
## Computer Name

The variable `COMPUTERNAME` and `HOSTNAME` .....


-----------------------------------------------------------------------------------------------
## Operating System

The variable `OS` .....

..... `Windows_NT` ......

The variable `SYSTEMDRIVE` .....

e.g. `C:`

The variable `SYSTEMROOT` .....

e.g. `C:\WINDOWS`


-----------------------------------------------------------------------------------------------
## Machine Characteristics

The variable `NUMBER_OF_PROCESSORS` .....

The variable `PROCESSOR_ARCHITECTURE` .....

`AMD64`





The variable `PROCESSOR_IDENTIFIER` .....

`Intel64 Family 6 Model 158 Stepping 10, GenuineIntel`



The variable `PROCESSOR_LEVEL` .....

The variable `PROCESSOR_REVISION` .....





-----------------------------------------------------------------------------------------------
## Names to Avoid

There are some commonly used environment variable names it is wise to avoid using. 

The following environment variables are reserved by ECLAT (and AdaOS): 

?????these have changed I think

    ECLAT_SVC
    PXCR_SVC
    ALDUS_SVC
    ECLAT_CWL
    ALDUS_CWS
    BASEAUTH

.......

According to information taken from 

 * [Environment Variables][2] of The Open Group Base Specifications Issue 7, 2018 edition. 
 
 * [Complete List of Environment Variables in Windows 10][3] of Windows Ten Forums

it is unwise to conflict with certain variables that are frequently exported by widely used 
command interpreters and applications: 

```
ALLUSERSPROFILE, APPDATA, ARFLAGS, CC, CD, CDPATH, CFLAGS, CHARSET, CMDCMDLINE, 
CMDEXTVERSION, COLUMNS, COMMONPROGRAMFILES, COMMONPROGRAMFILES(X86), COMMONPROGRAMW6432, 
COMPUTERNAME, COMSPEC, DATE, DATEMSK, DEAD, DRIVERDATA, EDITOR, ENV, ERRORLEVEL, EXINIT, FC, 
FCEDIT, FFLAGS, GET, GFLAGS, HISTFILE, HISTORY, HISTSIZE, HOME, HOMEDRIVE, HOMEPATH, IFS, 
LANG, LC_ALL, LC_COLLATE, LC_CTYPE, LC_MESSAGES, LC_MONETARY, LC_NUMERIC, LC_TIME, LDFLAGS, 
LEX, LFLAGS, LINENO, LINES, LISTER, LOCALAPPDATA, LOGNAME, LOGONSERVER, LPDEST, MAIL, 
MAILCHECK, MAILER, MAILPATH, MAILRC, MAKEFLAGS, MAKESHELL, MANPATH, MBOX, MORE, MSGVERB, 
NLSPATH, NPROC, NUMBER_OF_PROCESSORS, OLDPWD, ONEDRIVE, OPTARG, OPTERR, OPTIND, OS, PAGER, 
PATH, PATHEXT, PPID, PRINTER, PROCESSOR_ARCHITECTURE, PROCESSOR_IDENTIFIER, PROCESSOR_LEVEL, 
PROCESSOR_REVISION, PROCLANG, PROGRAMDATA, PROGRAMFILES, PROGRAMFILES(X86), PROGRAMW6432, 
PROJECTDIR, PROMPT, PS1, PS2, PS3, PS4, PSMODULEPATH, PUBLIC, PWD, RANDOM, RANDOM, SECONDS, 
SESSIONNAME, SHELL, SYSTEMDRIVE, SYSTEMROOT, TEMP, TERM, TERMCAP, TERMINFO, TIME, TMP, TMPDIR, 
TZ, USER, USERDOMAIN, USERDOMAIN_ROAMINGPROFILE, USERNAME, USERPROFILE, VISUAL, WINDIR, 
XDG_CACHE_HOME, XDG_CONFIG_DIRS, XDG_CONFIG_HOME, XDG_DATA_DIRS, XDG_DATA_HOME, 
XDG_RUNTIME_DIR, XDG_STATE_HOME, YACC, YFLAGS
```



-----------------------------------------------------------------------------------------------
## XDG

..... [XDG Base Directory Specification][1] .....

| `$XDG_DATA_HOME`      | base directory relative to which user-specific data files should be written
| `$XDG_CONFIG_HOME`    | base directory relative to which user-specific configuration files should be written
| `$XDG_STATE_HOME`     | base directory relative to which user-specific state data should be written - non-temp, but non-original
| `$XDG_DATA_DIRS`      | preference-ordered set of base directories to search for data files in addition to the $XDG_DATA_HOME. Should be separated with `:`
| `$XDG_CONFIG_DIRS`    | preference-ordered set of base directories to search for configuration files in addition to the $XDG_CONFIG_HOME. Should be separated with `:`
| `$XDG_CACHE_HOME`     | base directory relative to which user-specific non-essential data files should be stored
| `$XDG_RUNTIME_DIR`    | base directory relative to which user-specific non-essential runtime files and other file objects (such as sockets, named pipes, ...) should be stored. The directory MUST be owned by the user, and he MUST be the only one having read and write access to it. Its Unix access mode MUST be 0700.

The lifetime of the directory MUST be bound to the user being logged in. It MUST be created when the user first logs in and if the user fully logs out the directory MUST be removed. If the user logs in more than once he should get pointed to the same directory, and it is mandatory that the directory continues to exist from his first login to his last logout on the system, and not removed in between. Files in the directory MUST not survive reboot or a full logout/login cycle.

The directory MUST be on a local file system and not shared with any other system. The directory MUST by fully-featured by the standards of the operating system. More specifically, on Unix-like operating systems AF_UNIX sockets, symbolic links, hard links, proper permissions, file locking, sparse files, memory mapping, file change notifications, a reliable hard link count must be supported, and no restrictions on the file name character set should be imposed. Files in this directory MAY be subjected to periodic clean-up. To ensure that your files are not removed, they should have their access time timestamp modified at least once every 6 hours of monotonic time or the 'sticky' bit should be set on the file.

If $XDG_RUNTIME_DIR is not set applications should fall back to a replacement directory with similar capabilities and print a warning message. Applications should use this directory for communication and synchronization purposes and should not place larger files in it, since it might reside in runtime memory and cannot necessarily be swapped out to disk.

| `$XDG_DATA_HOME`      | `${HOME}/.local/share`
| `$XDG_CONFIG_HOME`    | `${HOME}/.config`
| `$XDG_STATE_HOME`     | `${HOME}/.local/state`
| `$XDG_DATA_DIRS`      | `/usr/local/share/:/usr/share/`
| `$XDG_CONFIG_DIRS`    | `/etc/xdg`
| `$XDG_CACHE_HOME`     | `$HOME/.cache`
| `$XDG_RUNTIME_DIR`    | `/run/user/$uid`





-----------------------------------------------------------------------------------------------
## 

.....

https://github.com/actions/virtual-environments/blob/main/images/win/Windows2019-Readme.md

Microsoft Windows Server 2019 Datacenter

OS Version: 10.0.17763 Build 3046
Image Version: 20220619.1

| `VCPKG_INSTALLATION_ROOT` | 
| `CONDA` | 
| `CHROMEWEBDRIVER` | 
| `EDGEWEBDRIVER` | 
| `GECKOWEBDRIVER` | 
| `SELENIUM_JAR_PATH` | 
| `JAVA_HOME_8_X64` | 
| `JAVA_HOME_11_X64` | 
| `JAVA_HOME_13_X64` | 
| `JAVA_HOME_17_X64` | 
| `GOROOT_1_16_X64` | 
| `GOROOT_1_17_X64` | 
| `GOROOT_1_18_X64` | 
| `ANDROID_HOME` | 
| `ANDROID_NDK_HOME` | 
| `ANDROID_NDK_LATEST_HOME` | 
| `ANDROID_NDK_PATH` | 
| `ANDROID_NDK_ROOT` | 
| `ANDROID_SDK_ROOT` | 
| `` | 
| `` | 
| `` | 
| `` | 
| `` | 
| `` | 
| `` | 
| `` | 
| `` | 
| `` | 
| `` | 
| `` | 
| `` | 
| `` | 
| `` | 
| `` | 
| `` | 
| `` | 
| `` | 
| `` | 
| `` | 
| `` | 
| `` | 
| `` | 
| `` | 




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
## References

[1]: <https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html> 
     "XDG Base Directory Specification"


