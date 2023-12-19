-----------------------------------------------------------------------------------------------
# ALDUS

The __Automated Library Download and Updating System__, or __ALDUS__ comprises a set of Ada
libraries, and plugins and tools based on them, that are intended to make it easy to: 

 * Emplace a [module](../pxcr/modules.md), together with its 
   [stub library](../eclat/building.md#stublibs), or a 
   [wrapped ECLAT library](../eclat/libraries.md#wrapped), or the 
   [source text](../eclat/libraries.md#source-files) that can be compiled to make a library, 
   available for use by yourself or other developers (on other computers), as a collection of 
   [members](#mem) of a [sanctuary](#sanc); 

 * Explicitly obtain (e.g. download) and keep a [local cache](#cache) of copies of members in
   one or more sanctuaries, so they can be re-used without having to be obtained again; 

 * When compiling a library, automatically obtain the (members that are) libraries which are
   needed (either included by a library you are compiling or included by some other library
   that it needs) but are not already in the local cache; 
 
 * Update the configuration of a library to the latest version of each library it includes
   which is a member of a sanctuary; 
   
 * Detect, report, and assist with the resolution of [version conflicts](#conf); 

 * Manage snactuaries and their members. 

.....

ALDUS comprises:

 * a [service](#serv); 

 * a [command-line tool](#tool), as a standalone program and as an Allegra plugin; 

 * an [ECLAT plug-in](#plugin). 




.....

ALDUS is open source, and published under the [GPL][gpl]. 

.....



-----------------------------------------------------------------------------------------------
## Members {#mem}

A [sanctuary](#sanc) _member_ can be: 

 * a PXCR [module](../pxcr/modules.md), with an accompanying [module class
   descriptor](../pxcr/mcd.md) file and wrapped ECLAT [stub
   library](../eclat/building.md#stublibs), and optionally with an equivalent full library, and
   optionally with an equivalent stub source tree, and optionally with an equivalent full
   source tree; or 
 
 * a wrapped ECLAT [library](../eclat/libraries.md#wrapped), optionally with an equivalent
   source tree; or 
 
 * an _Ada source tree_, a tree of Ada source text files, that can be compiled to make a
   library or included in another Ada project; or 

 * a _C source tree_, a tree of C source text files, that can be compiled to make a library or
   included in another C project; or 

 * a _data collection_, a tree of data files, that are all related or form a whole thing, for a
   specific purpose that is related to the development of software, and can optionally include
   one or more other collections. 


### Modules 

A _module_ 
a PXCR [module](../pxcr/modules.md), with an accompanying [module class
descriptor](../pxcr/mcd.md) file and wrapped ECLAT [stub
library](../eclat/building.md#stublibs), and optionally with an equivalent full library, and
optionally with an equivalent stub source tree, and optionally with an equivalent full
source tree
   
.....





A module member is normally accompanied by a [stub library](../eclat/building.md#stublibs) .....





A module member can optionally include an _equivalent full library_. If it does, it is implicit
that the full library can be included by any other library in such a way as to have the same
effect as if the stub library had been included (and the module incorporated into the
realisation). 

The purpose of an equivalent full library is to allow the functionality of the module to be
obtained without having to incorporate the module in realisations. 

A module member can optionally include an _equivalent stub source tree_, which is the source
text that can be compiled into the stub library. It can be an Ada source tree or a C source
tree. 

A module member can optionally include an _equivalent full source tree_, which is the source
text that can be compiled into the equivalent full library. It can be an Ada source tree or a C
source tree. 

When a 


### Libraries

A library member comprises a wrapped ECLAT [library](../eclat/libraries.md#wrapped), which is a
file that normally has the `.elw` file extension, .....

A library member can optionally include an _equivalent source tree_, which is the source text
that can be compiled to make the library. It can be an Ada source tree or a C source tree. 







### Ada Source Tree






### C Source Tree





### Data Collection

A data collection should be a set of files that a software developer is likely to want to
obtain (e.g. download) all together. 

A data collection can explicitly include one or more other collections. In that case, all the
files of the other collections are automatically included (logically, rather than as physical
copies). 

If a developer is actually likely to want, very often, half the files of a collection, the
collection should probably be split into two. A collection that simply includes the two halves
could then be created, for the convenience of those who want both halves. 

The data files can be of any type. 

Since ALDUS is intended to be used by software developers, a data collection that might be
useful to other kinds of users should probably be made available by some means other than ALDUS
(instead of, or in addition to, as an ALDUS data collection). 



  
.....



-----------------------------------------------------------------------------------------------
## Sanctuaries {#sanc}

An ALDUS _sanctuary_ contains a set of [members](#mem), 




?????wrapped ECLAT [libraries](../eclat/libraries.md) under 
the control of the [ALDUS service](../services/aldus.md). 

ALDUS provides a [command-line tool](../tools/aldus.md) which can be used to set up and 
maintain a sanctuary. Libraries in the sanctuary can be fetched by developers who are given 
access to it. 

A sanctuary may have an _upstream sanctuary_, from which it downloads libraries, and to which 
it uploads updates to its libraries and new libraries. 



Currently, the data in a sanctuary is mainly kept as a tree of files rooted in an (ordinary)
directory. 

.....
















-----------------------------------------------------------------------------------------------
## Local Caches {#cache}

.....










-----------------------------------------------------------------------------------------------
## ALDUS Service {#serv}

The __ALDUS Service__ is a service, provided as a [service program](../services/servprog.md),
which maintains its own sanctuary. 





The ALDUS Service is a [file management service](?????) .....





Typically, a computer or (local) network on which software development is done will have an
instance of the ALDUS service running, maintaining a _local sanctuary_ for the direct use of
the computer or all the computers in the network. For a network, it is assumed that the (ALDUS
Service giving access to the) local sanctuary will be reliable (always available) and fast. 






-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## Upstream Sanctuaries {#upstream}

...........




-----------------------------------------------------------------------------------------------
## Licences






-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## Downloading

When it fetches a library from an [upstream sanctuary](#upstream), .....

ALDUS uses the Kronos [File Operations Helper](../services/kronos.md#fileops) to transfer the 
files .....





...



-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## Command-Line Tool {#tool}

.....

standalone program and as an AdaShell plugin

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
or just _CWS_, .....

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

Set up mirroring of a sanctuary�s library in another sanctuary	

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












-----------------------------------------------------------------------------------------------
## ECLAT Plug-In {#plugin}

.....

If the compilation of an ECLAT library requires another library (because the library being
compiled depends on it), and the required library is not already in the ?????, then the ALDUS
plugin for ECLAT attempts to fetch the library from a trusted catalogue .....







-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## Versions and Dependencies {#conf}

Every ECLAT [library](../eclat/libraries.md) contains, as part of its metadata, a 
[version](../intro/versions.md) and a set of specific versions of other libraries that it 
includes .....

Similarly, every [member](#mems) in an ALDUS catalogue is associated with a version 

These versions are fully compatible with [Semantic Versioning][semver]. The 'API', or 'public 
API',  that the SemVer documentation speaks of corresponds to the 'surface' of an ECLAT 
library, or the equivalent in other kinds of member. 

In essence, the surface is whatever the member promises to those who use it. 

.....



### Version Numbers





### Precedence 

To recap what the SemVer specification (version 2.0.0) says:

_Precedence_ refers to how versions are compared to each other when ordered.

 1. Precedence MUST be calculated by separating the version into major, minor, patch and
    pre-release identifiers in that order (Build metadata does not figure into precedence).

 2. Precedence is determined by the first difference when comparing each of these identifiers 
    from left to right as follows: Major, minor, and patch versions are always compared 
    numerically.

    Example: `1.0.0 < 2.0.0 < 2.1.0 < 2.1.1`.

 3. When major, minor, and patch are equal, a pre-release version has lower precedence than a 
    normal version: 

    Example: `1.0.0-alpha < 1.0.0`.

 4. Precedence for two pre-release versions with the same major, minor, and patch version MUST 
    be determined by comparing each dot separated identifier from left to right until a 
    difference is found as follows: 

    1. Identifiers consisting of only digits are compared numerically.

    2. Identifiers with letters or hyphens are compared lexically in ASCII sort order.

    3. Numeric identifiers always have lower precedence than non-numeric identifiers.

    4. A larger set of pre-release fields has a higher precedence than a smaller set, if all of 
       the preceding identifiers are equal. 

    Example: `1.0.0-alpha < 1.0.0-alpha.1 < 1.0.0-alpha.beta < 1.0.0-beta < 1.0.0-beta.2 < 
              1.0.0-beta.11 < 1.0.0-rc.1 < 1.0.0`.


### Acceptability

To make an ECLAT library `A` include another library `B`, the version of `B` must be
_acceptable_ to `A`. 

To specify an acceptable version, the catalogue entry for `A`---or the library configuration 
of `A`, if it is not in the catalogue---will specify one or several _version targets_.

A version target is one of: 

 * a version number; 
 
 * a _truncated version number_; 
 
 * a _version range_. 

A truncated version number is one of: 

 * a major version number only; 
 
 * a major and a minor version number; 
 
 * major, minor, and patch; 
 
 * major, minor, patch, and pre-version. 

If there is more than one number, they are separated by a `.` period (dot, full stop)
character, as usual. 
 
If comparing precedence between a version number `V` and a truncated version number `T`, ..... 

A version range is two truncated version numbers, `L` and `U` say, where `L <= U`. 

.....

The version `V` of `B` will be unacceptable with a version target if: 

 * the target is a version number `T` and ``

 * the target is a version number `T` and ``




### Circularity 

A library `A` _needs_ library `B` if either: 

 * `A` includes `B`; or
 
 * `A` includes some library `C` and `C` needs `B`. 
 
This gives rise to the notion of a _dependency tree_. The tree must be acyclic, which means 
that there must never be a situation where `A` needs `B` and `B` needs `A` within the tree. 

This situation is termed a _circularity_, and is one of the things that ALDUS detects and 
reports to the user. If a circularity is detected as being caused by a new catalogue entry or 
an attempted update to an existing entry, (ALDIS gives the user an explanation to the user and) 
the circularity must be _resolved_ (got rid of) before ALDUS allows the entry to be registered 
or updated. 

.....



### Conflict 

There is a _resolution failure by omission_ if a library needs another library in the catalogue
and there is, in the catalogue, no version of that other library that is acceptable to the
library that needs it. 

Otherwise, there is a _resolution failure by conflict_ if a library needs another library in
the catalogue and there is, in the catalogue, no version of that other library that is
acceptable to all of the libraries which would include it. 

For example: 

 * The primary library `A` depends on library `B`; and 

 * `A` also depends on library `C`; and 

 * `B` depends on library `D`; and 
 
 * `C` also depends on `D`; and 
 
 * there is, in the catalogue, no version of `D` that is acceptable to both `B` and `C`. 

This kind of conflict cannot be allowed, because only one version of the other library (e.g. 
`D`) can exist in the effective library (formed from the entire dependency tree rooted at the 
primary library). If two versions of one library were allowed, there would (almost certainly) 
be library units with the same full name in the effective library. This is not allowed. 

If a conflict is detected as being caused by a new catalogue entry or an attempted update to 
an existing entry, the conflict must be _resolved_ (got rid of) before ALDUS allows the entry 
to be registered or updated. 

But that isn't the only way a conflict can occur. A conflict can occur if `A`, instead of being
a member in a catalogue, is one of the user's own (local) libraries, the user has already set
`B` as being included in `A`, and now the user is trying to get ALDUS to make `C` included in
`A` as well. ALDUS will detect the conflict in this situation and refuse to add `C` (giving an
explanation to the user). 

.....






-----------------------------------------------------------------------------------------------
## Resolving Circularities and Conflicts






-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## The Undercroft

There is a global sanctuary, called the __Undercroft__, which is free for use by anyone, 
subject to terms and conditions forbidding abuse of this facility. 

Libraries which are intended to be made available to the world for use under the terms of the 
[GPL][gpl] can be uploaded into the Undercroft. 

Information about the Undercroft can be obtained by visiting the website:

 * [?????](?????)

...








-----------------------------------------------------------------------------------------------
## References

[semver]: <https://semver.org> "Semantic Versioning 2.0.0"

[gpl]: <https://www.gnu.org/licenses/> "General Public License"


