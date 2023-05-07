-----------------------------------------------------------------------------------------------
# ALDUS

The __Automated Library Download and Updating System__, or __ALDUS__ comprises a set of Ada
libraries, and tools based on them, that are intended to make it easy to: 

 * Make a [wrapped ECLAT library](../eclat/libraries.md) available for use on other computers,
   as a collection of libraries called a [sanctuary](#sanc); 

 * Make use of the libraries in one or more sanctuaries; 
 
 * Keep up-to-date with the latest version of each library used, and update to the latest
   versions easily; 
   
 * Cache downloaded [artefacts](#art), so they can be re-used without having to be downloaded again; 
 
 * Detect, report, and assist with the resolution of [version conflicts](#conflicts); 

 * Manage the libraries in a sanctuary. 

.....

ALDUS comprises:

 * a [service](#serv); 

 * a [command-line tool](#tool), as a standalone program and as an Allegra plugin; 

 * an [ECLAT plug-in](#plugin). 




.....



-----------------------------------------------------------------------------------------------
## Artefacts {#art}

An _artefact_ is .....

An artefact can be one of: 

 * a PXCR [module](../pxcr/modules.md), with an accompanying [module class descriptor](../pxcr/mcd.md) file and
   wrapped ECLAT [stub library](?????); 
 
 * a wrapped ECLAT [library](../eclat/libraries.md), with an accompanying ..... ; 
 
 * a tree of Ada or C source text files, that can be compiled into a library or included in
   another Ada or C project; 

 * a tree of data files, that are all related or form a whole thing for a specific purpose. 
  
.....



-----------------------------------------------------------------------------------------------
## Sanctuaries {#sanc}

An ALDUS _sanctuary_ contains a set of [artefacts](#art), 




?????wrapped ECLAT [libraries](../eclat/libraries.md) under 
the control of the [ALDUS service](../services/aldus.md). 

ALDUS provides a [command-line tool](../tools/aldus.md) which can be used to set up and 
maintain a sanctuary. Libraries in the sanctuary can be fetched by developers who are given 
access to it. 

A sanctuary may have an _upstream sanctuary_, from which it downloads libraries, and to which 
it uploads updates to its libraries and new libraries. 



Currently, the data in a sanctuary is mainly kept as files in a (ordinary) directory. 

.....





-----------------------------------------------------------------------------------------------
## ALDUS Service {#serv}

The __ALDUS Service__ is a service, provided as a [service program](../services/servprog.md),
which maintains its own sanctuary. 





The ALDUS Service is a [file management service](?????) .....





Typically, a computer or (local) network on which software
development is done will have an instance of the ALDUS service running, maintaining a _local
sanctuary_ for the direct use of the computer or all the computers in the network. For a
network, it is assumed that the (ALDUS Service giving access to the) local sanctuary will be
reliable (always available) and fast. 






-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 








-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## Upstream Sanctuaries {#upstream}

...........




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## Licences






-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## Stub Libraries






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





-----------------------------------------------------------------------------------------------
## ECLAT Plug-In {#plugin}

.....

If the compilation of an ECLAT library requires another library (because the library being
compiled depends on it), and the required library is not already in the ?????, then the ALDUS
plugin for ECLAT attempts to fetch the library from a trusted catalogue .....







-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## Versions and Dependencies {#conflicts}

Every ECLAT [library](../eclat/libraries.md) contains, as part of its metadata, a 
[version](../intro/versions.md) and a set of specific versions of other libraries that it 
depends on .....

Similarly, every [artefact](#artefacts) in an ALDUS catalogue is associated with a version 

These versions are fully compatible with [Semantic Versioning][semver]. The 'API', or 'public 
API',  that the SemVer documentation speaks of corresponds to the 'surface' of an ECLAT 
library, or the equivalent in other kinds of artefact. In essence, the surface is whatever the 
artefact promises to those who use it. 

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

To make an ECLAT library `A` depend on another library `B`, the version of `B` must be 
_acceptable_ to `A`. 

To specify an acceptable version, the catalogue entry for `A`---or the library configuration 
of `A`, if it is not in the catalogue---will specify one or several _version targets_.

A version target is one of: 

 * a version number; 
 
 * _truncated version number_
 
 * a _version range_; 

A truncated version number is one of: 

 * a major version number only; 
 
 * a major and a minor version number; 
 
 * major, minor, and patch; 
 
 * major, minor, patch, and pre-version. 
 
If comparing precedence between a version number `V` and a truncated version number `T`, ..... 

A version range is two truncated version numbers, `L` and `U` say, where `L <= U`. 

.....

The version `V` of `B` will be unacceptable with a version target if: 

 * the target is a version number `T` and ``

 * the target is a version number `T` and ``




### Circularity 

A library `A` _needs_ library `B` if either: 

 * `A` depends on `B`; or
 
 * `A` depends on some library `C` and `C` needs `B`. 
 
This gives rise to the notion of a _dependency tree_. The tree must be acyclic, which means 
that there must never be a situation where `A` needs `B` and `B` needs `A` within the tree. 

This situation is termed a _circularity_, and is one of the things that ALDUS detects and 
reports to the user. If a circularity is detected as being caused by a new catalogue entry or 
an attempted update to an existing entry, (ALDIS gives the user an explanation to the user and) 
the circularity must be _resolved_ (got rid of) before ALDUS allows the entry to be registered 
or updated. 

.....



### Conflict 

There is a _conflict_ if a library needs another library in the catalogue and there is, in the 
catalogue, no version of that other library that is acceptable to all of the libraries which 
would depend on it.

For example: 

 * The primary library `A` depends on library `B`; and 

 * `A` also depends on library `C`; and 

 * `B` depends on library `D`; and 
 
 * `C` also depends on `D`; and 
 
 * there is, in the catalogue, no version of `D` that is acceptable to both `B` and `C`. 

This kind of conflict cannot be allowed, because only one version of the other library (e.g. 
`D`) can exist in the effective library (formed from the entire dependency tree rooted at the 
primary library). If two versions of one library were allowed, there would (almost certainly) 
be library units with the same full name in the effective library. This is not possible (for 
hopefully obvious reasons). 

If a conflict is detected as being caused by a new catalogue entry or an attempted update to 
an existing entry, the conflict must be _resolved_ (got rid of) before ALDUS allows the entry 
to be registered or updated. 

But that isn't the only way a conflict can occur. A conflict can occur if `A`, instead of 
being an artefact in the catalogue, is one the user's own libraries, the user has already set 
`B` as a dependency of `A`, and now the user is trying to get ALDUS to make `C` a dependency 
of `A` as well. ALDUS will detect the conflict in this situation and refuse to add `C` (giving 
an explanation to the user). 

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
[General Public Library](https://www.gnu.org/licenses/) can be uploaded into the Undercroft. 

Information about the Undercroft can be obtained by visiting the website:

 * [?????](?????)

...








-----------------------------------------------------------------------------------------------
## References

[semver]: <https://semver.org> "Semantic Versioning 2.0.0"




