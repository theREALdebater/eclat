-----------------------------------------------------------------------------------------------
# Command-Line Tool: `aldus`

The command-line tool `aldus` ..... [ALDUS](../aldus/aldus.md) .....

 * browse and choose libraries held in a sanctuary, showing vital information about each to 
   help the user make the choice; 

 * fetch a library from a sanctuary for local use, as well as identifying any other libraries 
   upon which a fetched library depends and fetching those also; 

 * check all such fetched libraries for newer versions, and automatically fetch any newer 
   versions; 
   
 * drop a fetched library; 

 * perform various other administrative functions. 

...





This tool depends on the [ALDUS Service](../services/aldus.md) and the [ECLAT 
Service](../services/eclat.md), which requires that the `ALDUS_SVC` and `ECLAT_SVC` environment 
variables are set to the full path name of the ALDUS Service and ECLAT Service saved state 
files. 

On a [hosted platform](?????), this command-line tool .....



-----------------------------------------------------------------------------------------------
## Current Working Sanctuary {#cws}

The subcommands of the `aldus` tool almost all make reference to a _current working sanctuary_, 
or just _CWS¬, .....

The current working sanctuary is: 

 1. if the environment variable `ALDUS_CWS` is set, the sanctuary whose name is in this 
    environment variable; 
    
 2. 



If the environment variable `ALDUS_CWS` is set (exists and is not blank), but its value does 
not match the name of a sanctuary known to the ALDUS Service, 




-----------------------------------------------------------------------------------------------
## Subcommand: `sanctuary` or `s`

Shows or sets the [CWS](#cws).

.....



-----------------------------------------------------------------------------------------------
## Subcommand: `list` or `L`

Shows the libraries in the [CWS](#cws).

.....



-----------------------------------------------------------------------------------------------
## Subcommand: `delete` 

Deletes a library from the [CWS](#cws).

Must be followed by the name of the library to be deleted, and that may optionally be followed 
by the compatibility version of the library to be deleted. 

If the compatibility version is omitted, then all versions of the library which exist in the 
sanctuary are deleted. 

If the compatibility version is supplied but that version of the library does not exist in the 
sanctuary, then a warning message is written to the standard output (unless the quiet option 
has been specified), but there is no error (the exit code is 0). 

Deletion of a library from the CWS does not afect any upstream sanctuary, and it does not 
affect any installations of the eleted library, except that .....

For example: 

    aldus delete acme.greenhouse.common 1.2
    
In this example, if a version compatible with `1.2` of the library `acme.greenhouse.common` 
exists in the current working sanctuary, it will be deleted; if it does not, the user will 
receive a warning. 

In Allegra, the equivalent is: 

    ALDUS.CWS.Libraries (L, V).Delete;
    
where `L` is the name of the library, and `V` is the compatibility version, both as string 
expressions. 

For example:

    ALDUS.CWS.Libraries ("acme.greenhouse.common", "1.2").Delete;



-----------------------------------------------------------------------------------------------
## Subcommand: `install` 

Installs a library from the [CWS](#cws) onto the local computer.

Must be followed by the name of the library to be installed, and that may optionally be 
followed by the compatibility version of the library to be installed. 

If the compatibility version is omitted, then the latest version of the library which exists in 
the sanctuary is installed. 

If the compatibility version is supplied but that version of the library does not exist in the 
sanctuary, then an error message is written to the standard error, and the exit code is 1. 

.....



-----------------------------------------------------------------------------------------------
## Subcommand: `restore` 

Ensure that all installed libraries are up-to-date good for use	



-----------------------------------------------------------------------------------------------
## Subcommand: `mirror` 

Set up mirroring of a sanctuary’s library in another sanctuary	

-----------------------------------------------------------------------------------------------
## Subcommand: `sanctify` (alias `push`)

Create a new library in a sanctuary	

-----------------------------------------------------------------------------------------------
## Subcommand: `update` 

Ensure an installed library is up-to-date	

......



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




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## Subcommand: `help` 

......	



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






