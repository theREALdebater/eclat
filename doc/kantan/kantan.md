-----------------------------------------------------------------------------------------------
# Kantan

__Kantan__ is a package management system, and is currently the default package management
system for the AdaOS Native [platform](../pxcr/targets.md#plat). 

Kantan only makes packages that can be installed on an AdaOS Native system. For
making packages to be installed on any other platform, including AdaOS on a hosted platform, 
then the package management systems of the host platform must
be used.

However, for making packages to be installed on AdaOS Native, the Kantan tools can be run on
any platform. Kantan packages can only be installed on an AdaOS Native system. 



-----------------------------------------------------------------------------------------------
## Packages {#}

A _Kantan package_---not to be confused with Ada packages---is an archive file, in the ZIP
format, that contains all the files needed to either introduce a specific piece of
functionality to, or to improve upon an existing piece of funtionality of, a
[security principal](../security/security.md#princ) in a controlled manner. 

An _as-new package_ introduces a specific piece of functionality. 

An _upgrade package_ improves upon an existing piece of funtionality. 

When applied to a specific principal, an as-new package is said to be _installed_, an upgrade
package is said to _upgrade_ an already installed package. A package can be easily removed; the
package is then said to have been _uninstalled_. 

The installation, upgrading, and uninstallation of packages for a principal does not (in
itself) affect any other principal, except possibly its inferior principals. 

A Kantan package might contain only data, but it will usually contain software to be executed
by the principal, as well as data accompanying the software. 

Associated with every package is meta-data, in the form of an XML file called a [Package
Information File](#pif), that gives details of the package (its name, version, description,
licence, etc). 

In order to construct (or reconstruct) packages, the [`kantan` command-line tool](#clt) needs
one or more [Package Make Files](#pmf) giving it all the information it needs. 

.....




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## Package Versions {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}








-----------------------------------------------------------------------------------------------
## {#}





-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## Package Information {#info}

A _package information file_ is an XML file of a specific schema which contains all the
information about a package that someone potentially wanting to use the package might need to
know. 


(its name, version, description,
licence, etc). 


When a package is built, the package information file is copied into the package itself, but is
also copied into a catalogue database .....








```xml
<package-info
      xmlns=""
      xmlns:db=""
      name="acme.peakprod.frobdel"
      uuid="820178BB-AF90-4DDA-8BF6-589631315418">
   <title>Frobnicator Deluxe</title>
   <version>1.7</version>
   <differences>
      <db:para>
         Frobnicator Deluxe version 1.7 features a new algorithm that improves frobnication speed 
         over previous versions by up to 20% in some cases.
      </db:para>
      <db:para>
         It also includes a number of bug fixes.
      </db:para>
   </differences>
   <description>
      <db:para>
         Frobnicate your whordles with ease, using this simple program. It displays a list of the 
         whordles in your whordolary, in alphabetic order, with tick boxes next to them. Simply
         tick the whordles you wish to frobnicate, and click on the 'Frob Now!' button. 
      </db:para>
   </description>
   <files>
      <file type="pxc" name="plugins/adaos.desktop.1.1/frobdel.pxc" size="17893246">
         <description>
            <db:para>
               The main Frobnicator Deluxe PXC module, which is a plugin for the AdaOS Desktop 
               version 1.1.
            </db:para>
         </description>
      </file>
      ...
   </files>
   ...
   <history>
      <previous-version>
         <title>Frobnicator Deluxe</title>
         <version>1.6</version>
         ...
         <description>
            <db:para>
               Frobnicate your whordles with ease, using this simple program. It displays a list of the 
               whordles in your whordolary, in alphabetic order, with tick boxes next to them. Simply
               tick the whordles you wish to frobnicate, and click on the 'Frob' button. 
            </db:para>
         </description>
         <files>
            <file type="pxc" name="plugins/adaos.desktop.1.1/frobdel.pxc" size="14568390"/>
            ...
         </files>
      </previous-version>
      ...
   </history>
</package-info>
```


-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## Making a Package {#make}

A _package make file_ is an XML file of a specific schema which contains all the information
needed by Kantan to construct (or reconstruct) a package. 





A package make file's name usually has the `,kantan-pmf.xml` extension on the end, but this is
not required. 








```xml
<package-make
      xmlns=""
      xmlns:db=""
      name="acme.peakprod.frobdel"
      moniker="frobdel"
      uuid="820178BB-AF90-4DDA-8BF6-589631315418">

   <rule-file path="${KANTAN_RULES}/eclat,kantan-rules.xml"/>

   <file-sources>
      <root-directory type="git" moniker="git-frobdel">
         <repository>https://github.com/acme/repos/frobdel</repository>
         <include>
            <original-path>/package/*</original-path>
            <package-path>/*</package-path>
         </include>
         ...
         <apply-rule name="git-source"/>
      </root-directory>
   </file-sources>

   <eclat>
      <libraries>
         <library moniker="frobdel" name="acme.peakprod.frobdel"/>
         ...
      </libraries>

      ...



   </eclat>
   <info-file source="git-frobdel">
      <path>/package/acme.peakprod.frobdel,kantan-pif.xml</path>
   </info-file>
</package-make>
```



### Rule Files

The element `rule-file` is used to specify a [rule file](#rules) that is to be used to ......

has two attributes, `path` and .....

The `path` attribute's value is the path of the rule file





to wrap up the output from a build into a 
Kantan package and generate the meta-data (file). 

?????A plugin will enable a package, after it has passed testing, to be added 
to a catalogue of packages. 



------------------------------------------------------
......

The `kantan-make` command-line tool provides: 

 * .....



    kantan-make M

where `M` is the path of the package make file that is to be used. 

For convenience, if `M` is not found, then an attempt is made to open `M,kantan-pmf.xml` instead. 

If just the command: 

    kantan-make

is run, and if the environment variable `KANTAN_CURRENT_PACKAGE` is set, then its value is used
as the package make file name, otherwise the user is prompted to enter the name, and the environment
variable `KANTAN_CURRENT_PACKAGE` is set from what the user enters.

.....



It is typical for a package to have just one make file to make it, and for that make file to
have the same base name as the package. For example, a package `acme.peakprod.frobdel` may have
a single package make file named `acme.peakprod.frobdel,kantan-make.xml` 



-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## Installing Packages {#int}

A Kantan package can only be installed on an AdaOS Native [platform](../pxcr/targets.md#plat). 



The Allegra procedure: 

    kantan install P into C

Installs the package named `P` into a [system configuration file](../pxcr/sysconf.md) named C. 

The Allegra procedure: 

    kantan install P

assumes ......










(specific XML schema) and catalogues (also XML files) to work out what actions to take

system configuration file is C if specified, but defaults to `current,sys-cfg.xml`

Unpacks packages into unpacked package root directories, as necessary, and also removes these directories

Installs packages, from unpacked package root directories, into installation (instance) root directories, also removing installations etc.

Performs system reconfiguration

If necessary, runs the Realizor

generates report of everything done (text file? XML file? other?)

reboots



The procedure:

    kantan config

.....




### Package Installation Name

.....

When a program is run, it is either actually installed or treated as if it had been installed
on the [effective system](../intro/intro.md#effsys). Either way, it will have an _installation
name_. 






On Windows and Linux, no distinction is made between the installation name of a program and the
name of the program. Kantan allows a program to be installed more than once (for a particular
principal) under different names, called _installation names_. Thus, for Windows and Linux, the
installation name will be the same as the program name, but this may not be the case under
AdaOS Native.

On Windows and Linux, a program is installed onto the computer system, and is not specific to a
user at all. On these platforms, each program needs to have separate configuration data for
each different user. Under AdaOS Native, a program is only ever installed for a specific user
(indeed, usually, for a specific role of a specific user), and so the configuration (and
working data) files are already specific to the user (or role).


-----------------------------------------------------------------------------------------------
## {#}


Command-line tool

kantan <subcommand> 

Options

All command line options may be set using the configuration file, the descriptions indicate the configuration option to set.
For boolean options you can override the config file by using something like -f-,--no-f, -f=no or several other variations.

   -c, --config-file=<path>

Configuration File; Specify a configuration file to use. The program will read the default
configuration file and then this configuration file. If configuration settings need to be set
before the default configuration files are parsed specify a file with the KANTAN_CONFIG
environment variable. 

   -h, --help

Show a short usage summary.

   -v, --version

Show the program version.

-t=<target release>

-a=<architecture>





Subcommands

new <package>

The `new` subcommand creates a new package .....

list <package>

list is used to display a list of packages. It supports shell pattern for matching package names and the following options: --installed, --upgradable, --all-versions are supported.	 

search <package>

Search for the given term(s) and display matching packages.	

show <package>

Show the package information for the given package(s). 

install <package>

install is followed by one or more package names desired for installation or upgrading.

A specific version of a package can be selected for installation by following the package name
with an equals and the version of the package to select. This will cause that version to be
located and selected for install. Alternatively a specific distribution can be selected by
following the package name with a slash and the version of the distribution or the Archive name
(stable, testing, unstable).

remove <package>...

remove is identical to install except that packages are removed instead of installed. Note that
removing a package leaves its configuration files on the system.

If a plus sign is appended to the package name (with no intervening space), the identified
package will be installed instead of removed.

edit‑sources

Edit the sources.list file and provides basic sanity checks.	 

update <package>

Resynchronize the package index files from their sources. 

upgrade

Install the newest versions of all packages currently installed on the system from the sources
enumerated in /etc/apt/sources.list. New packages will be installed, but existing packages will
never be removed. 

full‑upgrade

Perform the function of upgrade but may also remove installed packages if that is required in
order to resolve a package conflict.


apikey - retrieves, saves or deletes an API key for a particular source
cache - Manage the local HTTP caches used to store queries (v2.1.0+)
config - Retrieve and configure config file settings
convert - converts packages from one type to another type
download - downloads packages - optionally internalizing all remote resources
export - exports list of currently installed packages
feature - view and configure choco features
features - view and configure choco features (alias for feature)
find - searches remote packages (alias for search)
help - displays top level help information for choco
info - retrieves package information. Shorthand for choco search pkgname --exact --verbose
optimize - optimizes installation, reducing space usage
install - installs packages using configured sources
list - lists local packages
new - creates template files for creating a new Chocolatey package
outdated - retrieves information about packages that are outdated. Similar to upgrade all --noop
pack - packages nuspec, scripts, and other Chocolatey package resources into a nupkg file
pin - suppress upgrades for a package
push - pushes a compiled nupkg to a source
search - searches remote packages
setapikey - retrieves, saves or deletes an API key for a particular source (alias for apikey)
source - view and configure default sources
sources - view and configure default sources (alias for source)
support - provides support information
sync - synchronizes against system installed software - generates missing packages
synchronize - synchronizes against system installed software - generates missing packages
template - get information about installed templates
templates - get information about installed templates (alias for template)
uninstall - uninstalls a package
unpackself - re-installs Chocolatey base files
upgrade - upgrades packages from various sources




-----------------------------------------------------------------------------------------------
## Directory for Package Installation Specific Data {#work}

The [environment variable](../adaos/envvars.md#instdir) `APPDATA` contains the full, absolute
path of the directory for a package installation to store files. 

A package installation named `P` should store these files in the following directory:

    %APPDATA%\P
    ${APPDATA}/P

The value of the environment variable `APPDATA` is typically as follows:

| Platform      | Directory                             |
| ------------- | ------------------------------------- |
| Windows       | `C:\Users\%USER%\AppData\Roaming`     |
| Linux/FHS     | `${HOME}`                             |
| AdaOS Native  | `/work`                               |

where `%USER%` is the value of the environment variable `USER`, and `${HOME}` is the value of
the environment variable `HOME`. 

These values may vary.

The expectation is that a user who has installed the same package (with the same package
installation name `P`) on multiple [effective systems](../intro/intro.md#effsys) will have the
same files in `%APPDATA%\P` or `${APPDATA}/P` on each of those systems. Whether and how updates
to these files are propagated to other systems is not currently defined. 

A package installation named `P` should store its user-specific configuration data files in the
following directory: 

    %APPDATA%\P\Config
    ${APPDATA}/P/config



-----------------------------------------------------------------------------------------------
## Directory for Package Installation Data Specific to System {#local}

The [environment variable](../adaos/envvars.md#instdir) `LOCALAPPDATA` contains the full,
absolute path of the directory for a package installation to store files specific to the
installation and an [effective system](../intro/intro.md#effsys). 

A package installation named `P` should store these files in the following directory:

    %LOCALAPPDATA%\P
    ${LOCALAPPDATA}/P

The value of the environment variable `LOCALAPPDATA` is typically as follows:

| Platform      | Directory                             |
| ------------- | ------------------------------------- |
| Windows       | `C:\Users\%USER%\AppData\Local`       |
| Linux/FHS     | `${HOME}/hosts/${HOSTNAME}`           |
| AdaOS Native  | `/local`                              |

where `%USER%` is the value of the environment variable `USER`, `${HOME}` is the value of the
environment variable `HOME`, and `${HOSTNAME}` is the value of the environment variable
`HOSTNAME`. 

These values may vary.

The files stored in this directory are expected to be separate from the files stored on other
[effective systems](../intro/intro.md#effsys), even if the same package installation name has
been installed by the same user on different systems. 



-----------------------------------------------------------------------------------------------
## Directory for Package Installation Data {#data}

The [environment variable](../adaos/envvars.md#instdir) `PROGRAMDATA` contains the full,
absolute path of the directory for files to be stored which are specific to a package, but not
specific to any installation of the package. 

A package named `P` should store these files in the following directory:

    %PROGRAMDATA%\P
    ${PROGRAMDATA}/P

The value of the environment variable `PROGRAMDATA` is typically as follows:

| Platform      | Directory                             |
| ------------- | ------------------------------------- |
| Windows       | `C:\ProgramData`                      |
| Linux/FHS     | `/etc`                                |
| AdaOS Native  | `/data`                               |

These values may vary.

A package whose name is `P` should store its non-installation specific configuration data
files in the following directory:

    %PROGRAMDATA%\P\Config
    ${PROGRAMDATA}/P/config

The files in this location would be private to the package `P`. 







-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## Rule Files {#rules}

A Kantan _rule file_ contains a set of rules that Kantan is to use in [making](#make) a
package. 







```xml
<kantan-rules>
   <rule target="eclat-build">

      <description></description>

      <parameter name="P">Product name</parameter>

      <needs></needs>
      <needs></needs>
      <needs></needs>
      <needs></needs>

      <action using="allegra" good-exit="0 1 2"><![CDATA[
         eclat --product=${P}
      ]]></action>



   </rule>

   <rule target="git-source">

      <description></description>

      <parameter name="P">Path to root directory of repository</parameter>

      <needs></needs>
      <needs></needs>
      <needs></needs>
      <needs></needs>

      <action using="allegra" good-exit="0 1"><![CDATA[
         git pull ${P}
      ]]></action>

   </rule>
</kantan-rules>
```









-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## AdaOS

.....

 * The packages `AdaOS.Config.PXCR` and `AdaOS.Config.ECLAT` contain packages generated by 
   the [Realizor Configuration Helper](../config/Helpers.md) and the .....

 * The [Logging and Auditing](../debug/Logging.md) packages `AdaOS.Logging` and 
   `AdaOS.Auditing`. 

 * The [Events](../events/Events.md) package `AdaOS.Events` .....

 * The [Localisation](../locales/Locales.md) package `AdaOS.Localization` and its children
 
 * The [Memory Management](../memory/Defrag.md) packages `AdaOS.Storage_Pools` and `AdaOS.Memory` 

 * The [System Objects](../objects/Objects.md) package `AdaOS.Objects`

 * The [Realizor](pxcr/Plugins.md) package `AdaOS.Modules`, and its child `AdaOS.Modules.Plugins`

 * The [Self and External Programs](../rts/Instances.md) package `AdaOS.Execution`

 * The [Inter-Process Communication](../rts/IPC.md) package `AdaOS.Communication`

 * The [Services](../services/Services.md) package `AdaOS.Services`

 * The []() package `AdaOS.Security`

 * The [Kronos](../services/Kronos.md) package `AdaOS.Jobs`

 * The []() package ``

 * The []() package ``

 * The []() package ``

 * The []() package ``

 * The []() package ``

 * The []() package ``

 * The []() package ``

 * The []() package ``

 * The []() package ``




-----------------------------------------------------------------------------------------------
## Standard Ada Library

.....

[ECLAT Ada Standard Library](../adalib/adalib.md)




-----------------------------------------------------------------------------------------------
## {#}






-----------------------------------------------------------------------------------------------
## {#}

A GUI will allow the user to: 

 * browse a catalog and select packages to be installed; 
 
 * browse a list of already installed packages, view available updates and upgrades for each, 
   select packages to be uninstalled, updated, or upgraded; carry out the selected operations. 




-----------------------------------------------------------------------------------------------
## {#}








