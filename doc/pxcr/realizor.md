-----------------------------------------------------------------------------------------------
# The Realizor

The __Realizor__ fulfils a role akin to traditional linkers, but it actually does much of what 
a traditional compiler does. 

The Realizor takes a set of files called _modules_, and, using [configuration
data](../intro/config.md), generates an _executable image_. The image is said to be _realised_
by this process. 

On a [hosted platform](targets.md#plat), an executable image is the same as an executable file
for the platform. On the AdaOS Native platform, it is a boot image or segment image. 

......

The [Realizor Service](../services/pxcr.md) performs realisation. This function of the service 
can be invoked using the `pxcr` [command-line tool](../tools/pxcr.md). 

On hosted platforms, `pxcr` is a separate executable (program) file that has its services
(including the 

?????Syscon Service

) built-in. On the AdaOS platform, services are part of the boot
image, or a segment image it references. 

.....



-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## Modules {#mod}

.....

[Modules](modules.md)

.....




Not only do the appropriate modules for the [run time system](../rts/rts.md) need to be
configured to be included in an [executable image](#img), but also all the modules that provide
all of the exports---subroutines, static data, and dynamic data areas---that will be needed by
other modules in the image. 

.....







-----------------------------------------------------------------------------------------------
## Targets {#targ}

.....

[Targets](targets.md)

.....



-----------------------------------------------------------------------------------------------
## Module Versions {#ver}

Every module has a [version](../intro/versions.md). Different versions of a module identify new
iterations of the module, as it is changed, maintained, and improved. 

Every time a module is changed, its version is also changed (unless the module is renamed, 
which may be done when the module is branched, forked, or otherwise extensively changed). 



.....

For every module configured to be included in an executable image, the [compatibility
version](../intro/versions.md#comp) of the module must also be configured. 



.....





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## System Start (#start)

.....

Every [executable image](#img) or [boot image](#boot) must have exactly one module in it which is a 


?????[base module](../eclat/building.md#modes). 


?????A base module exports a parameterless subroutine named `system.execute.call`. This subroutine .....



When ??????a library is built in base module mode, the `system.execute` subroutine should be 
implemented as a library-level procedure with the following Ada declaration: 

```ada
procedure Start 
   with Export, External_Name => "system.execute.call";
```

The name of the procedure (`Start` in this example) is unimportant providing its external name 
is `system.execute.call`. 

The Realizor will generate code that calls this procedure. 



..... [run time system](#startup) .....






-----------------------------------------------------------------------------------------------
## Exports (#exports)

The image is expected to export the following subroutines:

| Name                   | Purpose
| ---------------------- | --------------------------------------------------------------------
| pxcr.                  | 
| pxcr.                  | 
| pxcr.                  | 
| pxcr.                  | 
| pxcr.                  | 
| pxcr.                  | 
| pxcr.                  | 
| pxcr.                  | 
| pxcr.                  | 
| pxcr.                  | 
| pxcr.                  | 
| pxcr.                  | 
| pxcr.                  | 
| pxcr.                  | 
| pxcr.                  | 

The realisation is expected to export the following data:

| Name                   | Purpose
| ---------------------- | --------------------------------------------------------------------
| pxcr.                  | 
| pxcr.                  | 
| pxcr.                  | 
| pxcr.                  | 
| pxcr.                  | 
| pxcr.                  | 
| pxcr.                  | 
| pxcr.                  | 
| pxcr.                  | 
| pxcr.                  | 
| pxcr.                  | 
| pxcr.                  | 
| pxcr.                  | 
| pxcr.                  | 
| pxcr.                  | 
| pxcr.                  | 
| pxcr.                  | 










-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## ECLAT Realizor Library

The Realizor is written in Ada source text, as part of the ECLAT Realizor Library (ERL). This 
library is normally built into a module named `system.pxcr.mod`. This module is included into 
the realisation of the `pxcr` command-line tool. 

.....



-----------------------------------------------------------------------------------------------
## Profiling {#profiling}

.....





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## System Configuration Helper {#syscon}

The _System Configuration Helper_ is a [Realizor Helper plugin](helpers.md) which
[generates](../pxcr/modules.md#genmod) a [system configuration module](../config/sysconfig.md) 
.....











-----------------------------------------------------------------------------------------------
## Pseudo Modules {#pseudo}

.....

A _pseudo-module_ only exists as loaded (in memory) into the Realizor, but not as a file on
disk anywhere. 


Typically, pseudo-modules are created by [helpers](helpers.md) .....





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





