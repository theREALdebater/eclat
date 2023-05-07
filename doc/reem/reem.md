-----------------------------------------------------------------------------------------------
# Package Execution Viability Assurance

__Reproducible Execution Environment Management__, or __REEM__, is a concept intended to reduce
the likelihood that a program fails, at the time when an attempt is made to run it, because
something in its execution environment is missing or wrong. 

Typically, a program is installed (in some sense) and used in many environments (computers,
operating systems, platforms, architectures), and it is generally impossible to ensure that
there are no potentially problematic differences in those environments. 

Environments necessarily differ. For example, there will be differences in available peripheral
hardware. There will be many other reasons why differences are, for practical purposes,
unavoidable.  

There needs to be a way to ensure that there is nothing in (or missing from) the environment 
that will---or is likely to---prevent the program running successfully. 

It may not be practicable for such a test to be perfect, but it is likely to be useful. Not
only can it identify that there is a problem, but it can probably give some diagnostic
information helping users to pinpoint the cause of the problem and fix it. 

In fact, it may also be possible to automatically fix some problems. 

At the same time, programs could be made to run self-tests, to provide additional assurance
that the programs themselves are free of bugs as they run within a particular environment. 



-----------------------------------------------------------------------------------------------
## Infrastructure

The ECLAT Reproducible Execution Environment Management infrastructure is based on:

 * a [REEM Executive Service](../services/reem.md) that uses a set of [specifications](#specs)
   to check the execution environment on behalf of a set of programs; 

 * [plugins](#plugins) for the REEM Executive Program that extend the different kinds of 
   [requirement](#rqts) that can be dealt with; 
   
 * a [General REEM Library](#grl) that is used by the REEM Executive Program but can also be 
   used by plugins to assist them. 



-----------------------------------------------------------------------------------------------
## Strategy

..........

and that package contains one or more programs that could 
benefit from REEM, any



### Three Images

The usual REEM strategy is termed the _three images_ strategy. 

A _refresh_ is .........

Whenever a package is installed or removed, the _refresh needed_ flag of the computer (system) 
is set (to 'true'). 



When a refresh is needed, the [REEM Executive Service](../services/reem.md) will initiate the
[realisation](../pxcr/realizor.md) of two new [executable images](../pxcr/images.md): 

 * the _trial image_, which contains all the REEM and testing functionality, and omits the
   functionality it doesn't need; 

 * the _commit image_, which is the same as the trial image but without the REEM
   and testing functions and with any of the functionality the trial image omitted. 

The third image is the _rollback image_, which is (normally) the current image. 

The following steps are carried out: 

 1. First, the trial image is realised (in the background, so the users of the computer can
    continue working). When the trial image is realised, it is ready to be set up as the new
    current boot image. 

 2. If the user or system administrator initiates it, or reboot has been set to automatic, the
    computer reboots into the trial image. 

 3. The trial image then runs the REEM [resolution cycle](#rslvrs), and then all of the package
    tests. It reports progress to the user on the screen, and logs the results. 

 4. If the user or system administrator aborts the trial, or if the REEM resolution cycle
    fails, or any of the tests fail, then the computer is immediately rebooted back into the
    rollback image. The rollback image remains as the computer's current boot image. 

 5. Otherwise, the computer is rebooted back into the rollback image, but the commit image is
    realised (again, in the background) and the commit image is set as the computer's current
    boot image. 

 6. When the commit image has been realised, if the user or system administrator initiates it,
    or reboot has been set to automatic, the computer reboots into the commit image. According
    to settings, the rollback image may be deleted at this stage. By default, the rollback
    image three steps back is deleted (if it exists), so that three steps of rollback remain
    available. 






### Pre-Install

Beside performing the REEM resolution cycle as a part of the installation of a package, the
REEM strategy includes a pre-installation cycle per package, called the package's _pre-check_. 

The purpose of the pre-check is to ensure that the target environment is capable of being
suitable for the package, both in its current state but also that anything which needs to be
changed can be changed. 

There will be some (or many) aspects of the environment that the pre-check cannot verify, or
cannot be certain about. That is why the REEM resolution cycle is needed. But it is valuable if
there is anything that it can check in advance. 





### Post-Uninstall

In theory, any changes made to the execution environment a package is installed into should be
undone when the package is uninstalled. 

However, this approach would be too naïve.

If another package had been installed 





.....


### Scripting

The REEM strategy is all written in the form of [AdaShell](?????) scripts. This makes it
relatively easy for the strategy to be adapted by user to their needs and preferences. 


.....

```sh



```














.....





### Command-Line Tool

The [`reem` command-line tool](../tools/reem) .....

The `status K` command can be used to find out if the refresh needed flag of the package `K` 
is true or false.  Regardless of the flag's value, the `refresh K` command can be given to 
initiate a refresh of package `K`; after the refresh is completed, the refresh needed flag of 
`K` is reset back to 'false'. 



When a refresh is initiated, usually by a command of some sort from a user, the static 
directories of all installed packages are searched for [specification files](#specs), and those
files are read and merged into a specification ........




..........



-----------------------------------------------------------------------------------------------
## Specifications {#specs}

A _specification_ is a set of [requirements](#rqts). 

........



A _specification file_ is a plain text file containing 



-----------------------------------------------------------------------------------------------
## Requirements {#rqts}

A _requirement_ is an object that represents some aspect of the environment that needs to be 
checked in a specific way. 

.......

The [REEM Executive Service](../services/reem.md) supports many different kinds of 
requirements: 

 * environment variables
 
 * files

 * services

 * peripheral hardware, devices

 * connections to other computers and services on other computers

 * other programs available to be executed

 * one or more locales

 * ODBC databases or drivers

 * etc.

However, [Plugins](#plugins) can add further kinds of requirement. 

......



-----------------------------------------------------------------------------------------------
## Conflict {#conflict}

The environment of a particular program could be checked in a simplistic manner, but 
realistically the checking needs to be done for a whole set of programs---ideally all the 
programs that can be executed on the computer---especially if it is necessary to make changes 
to the environment. It would be pointless to change the environment to suit one program if 
doing so invalidated the environment for another program. 

REEM makes the assumption that any one [requirement](#rqts) may be capable of being in 
_conflict_ with a certain set of other requirements, but will be totally independent of all 
the others. 

For example, a requirement pertaining to an environment variable named `XYZ` will (with 
perhaps certain esoteric exceptions) be independent of a requirement pertaining to an 
environment variable named `ABC`. Changing `XYZ` will not affect `ABC` and vice versa. 

It is therefore necessary to detect which requirements are, in a practical sense, in conflict 
with one another, and report such conflicts. Better still would be to automatically resolve 
such conflicts, having detected them. 

This is the job of [conflict resolvers](#rslvrs)



-----------------------------------------------------------------------------------------------
## Conflict Resolvers {#rslvrs}

A _conflict resolver_ is an object that is capable of detecting conflicts between a certain set 
of [requirements](#rqts), reporting on any conflicts detected, and possibly resolving them. 

Every kind of requirement can create one or more new conflict resolvers and/or attach itself 
to existing ones (while adjusting their configuration at the same time). 

The [REEM Executive Service](../services/reem.md) ..... 

.....

A conflict resolver is actually a kind of [Kronos](../services/kronos.md) job, ......






A _resolution cycle_ comprises:

 1. iterate through all the requirements in a [specification](#specs) to produce a set of 
    conflict resolvers; 
 
 2. execute these resolvers; 
 
 3. return their amended sets of requirements, [merge](#merge) them together again into a single 
    set of requirements (a new specification); 
    
 4. .....

The resolvers will therefore, in general, be executed in parallel, so they must not interfere 
with each other. 

When a resolver is executed, it:

 1. is given a *copy* of the set of requirements it is to deal with; 

 2. detects any conflicts among those requirements; 
 
 3. reports on any conflicts it has found; 
 
 4. optionally attempts to resolve the conflicts, generating a new set of requirements. 



The resolution cycle is repeated until it produces no changes: the merged specification is 
exactly the same as the specification produced by the previous cycle. 

The term _instability_ describes the situation where a resolution cycle repeats too many times.
It is is configurable as to how many times constitutes too many, but the default is n + 1 where
n is the number of requirements in the (original) specification. However, the cycle is also
considered unstable if the set of new requirements generated by a cycle is the same as any of
the sets of requirements generated by any of the previous cycles (or the original set of
requirements). The REEM Executive Service will stop with a failure if it detects instability. 

The REEM Executive Service reports on the changes between the original specification (before
the first resolution cycle) and the final specification (after the last cycle). 










-----------------------------------------------------------------------------------------------
## Requirement Type: Environment Variables

.......


### General Environment Variables

Checks that a specific 

[environment variable](?????)

is set, has a specific value, or is not set, .....


#### `exists`

Checks that the environment variable has been set to something (that is not blank).

If the `environment-variable` element has content, it is a default value. 

Failure of the `exists` check will have a resolution of: 

 * setting the environment variable to the default value, if a (non-blank) default value is 
   specified; 
   
 * if a (non-blank) default value is not specified, reporting an error .....

......


#### `has-value`

Checks that the environment variable has the value of the `environment-variable` element's 
content. 

..........


#### `is-empty`

.........

The `environment-variable` element must not have any content.



```xml
   <reem:environment-variable
      var-name="ECLAT_ROOT"
      check="exists">
      c:\eclat
   </reem:environment-variable>
   
   
   
   
   
   
<requirement name="Valid_Platform_With_DB" operations="Create">
   <alternative name="WindowsDB2">
      <property checkId="Windows_XP_Check">
         <propertyName>OsType</propertyName>
         <value>Windows XP</value>
      </property>
      <software checkVarName="db2_for_Windows_check">
         <UUID>22345678901234567890123456789012</UUID>
         <name pattern="true">(DB2|Universal Database)</name>
         <minVersion>7.2</minVersion>
         <maxVersion>8.1</maxVersion>
      </software>
   </alternative>
</requirement>


<custom artifactIdRef="winRegCustomCheck" checkId="GSK4_Win_Registry_Check">
   <parameter variableNameRef="Win_Reg_Hive">HKEY_LOCAL_MACHINE</parameter>
   <parameter variableNameRef="Win_Reg_Key">SOFTWARE\IBM\GSK4\CurrentVersion\Version</parameter>
   <parameter variableNameRef="Win_Reg_Type">REG_SZ</parameter>
   <parameter variableNameRef="Win_Reg_Value">4.0.2.49</parameter>
</custom>


<customCheckArtifact artifactId="winRegCustomCheck">
   <fileIdRef>WinRegistryCheckArtifact</fileIdRef>
   <parameterMaps>
      <map>
         <internalName>Win_Reg_Hive</internalName>
         <externalName>Reg_Hive</externalName>
      </map>
      <map>
         <internalName>Win_Reg_Key</internalName>
         <externalName>Reg_Key</externalName>
      </map>
      <map>
         <internalName>Win_Reg_Type</internalName>
         <externalName>Reg_Type</externalName>
      </map>
      <map>
         <internalName>Win_Reg_Value</internalName>
         <externalName>Reg_Value</externalName>
      </map>
   </parameterMaps>
</customCheckArtifact>


<SDD-dd:DeploymentDescriptor
xmlns:sp="http://docs.oasis-open.org/SDD/ns/starterProfile"
... >
...
<SDD-dd:Topology>
<SDD-dd:Resource id="os" type="sp:CIM_OperatingSystem">
<SDD-dd:HostedResource id="Filesys" type="sp:CIM_FileSystem"/>
</SDD-dd:Resource>
</SDD-dd:Topology>
...
<SDD-dd:Requirement id="DiskSpace" operation="install use">
<SDD-dd:ResourceConstraint
 id="DiskSpaceRequirement" resourceRef="Filesys">
<SDD-dd:ConsumptionConstraint>
<SDD-dd:PropertyName>
 sp:CIM_FileAdaOS.AvailableSpace
 </SDD-dd:PropertyName>
<SDD-dd:Value unit="megabytes">15</SDD-dd:Value>
</SDD-dd:ConsumptionConstraint>
<SDD-dd:PropertyConstraint>
<SDD-dd:PropertyName>
 sp:CIM_FileAdaOS.Type
 </SDD-dd:PropertyName>
<SDD-dd:Value>NTFS</SDD-dd:Value>
</SDD-dd:PropertyConstraint>
</SDD-dd:ResourceConstraint>
</SDD-dd:Requirement>
...
</SDD-dd:DeploymentDescriptor>

```



### `PATH`

The `PATH` environment variable ........

......

```xml
   <reem:program-paths>
      <reem:reaches program="zipple"/>
      <reem:reaches program="dingbar">
         <reem:suggested-directory dir-name="/mnt/c/foobar/bin"/>
         <reem:suggested-directory dir-name="/usr/opt/foobar/bin"/>
      </reem:reaches>
   </reem:program-paths>
```

In the above example, the first `reaches` element requires that program `zipple` is executable, 
meaning that one of the paths in the `PATH` allows the 

Note that each 'reaches' is a separate requirement. 

.........


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
## General REEM Library {#grl}

The __General REEM Library__, or __GRL__, is an ECLAT library that aims to provide 
functionality likely to be useful to REEM [plugins](#plugins).

.....


????? Reliable Execution Environment Database {#reed}

????? The _Reliable Execution Environment Database_, or _REED_, holds information needed by the 
[GRL](#grl) .....

.......














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
## References

[1]: <https://github.com/anthony-arnold/AdaID> "AdaID on GitHub"






