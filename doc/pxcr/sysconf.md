-----------------------------------------------------------------------------------------------
# System Configuration

A _system configuration_ is a set of [saved state](../intro/config.md) files which contain all
the information that the [Realizor](realizor.md) needs to create an 
[executable image](images.md). 


-----------------------------------------------------------------------------------------------
## 

All these files are kept in a single directory, the _system configuration directory_. 

For compatibility version `V` of an
image named `X`, the [path](../objects/paths.md) of the directory is either the value of the
[environment variable](../adaos/envvars.md) named `PXCR_CURRENT_IMAGE`, or, if that environment
variable is not set, it is: 

| Platform     | Directory                                           |
| ------------ | --------------------------------------------------- |
| Windows      | `C:\Users\%USER%\AppData\Roaming\pxcr\img\X\V\cfg`  |
| Linux/FHS    | `${HOME}/pxcr/img/X/V/cfg`                          |
| AdaOS Native | `/pxcr/img/X/V/cfg`                                 |

where `%USER%` is the value of the environment variable `USER`, and `${HOME}` is the value of
the environment variable `HOME`. 

The compatibility version `V` must be in the format `Maj.Min`, where `Maj` is the major version
number and `Min` is the minor version number. Both `Maj` and `Min` must comprise decimal digits
with no leading zeroes (if the value is zero, then a single digit `0`). 


There is one file for the Realizor itself, named:

    pxcr,cfg.dat

In addition, there will generally be one file for each contributory library depended on by the
Realizor, one for each Realizor plugin, and one for each (different) contributory library
depended on by any of these plugins. Currently: 

    ,cfg.dat
    ,cfg.dat
    ,cfg.dat
    ,cfg.dat
    ,cfg.dat
    ,cfg.dat
    ,cfg.dat

.....



-----------------------------------------------------------------------------------------------
## State Update Files

Each of the system configuration files will have at least one corresponding 
[state update file](../intro/config.md#stuf). 

These are normally stored in the directory 







```xml






```


-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
# Security Foundation

The _security foundation_ of an [effective system](../intro/intro.md#effsys) comprises a
specification of the effective system's:

 * [authorities](security.md#auth); 

 * hierarchy of authorities; 

 * [ambit](security.md#amb) of each authority ?????

 * [principals](security.md#princ) (users and roles) and their
   [identities](security.md#ident); 

 * hierarchy of principals (and therefore identities); 

 * 










-----------------------------------------------------------------------------------------------
## {#}

 which contains the following exports: 


### `System.Security.Security_Authority_Last`

This is a 

.......


### `System.Security.Super`

.......



### `System.Security.Is_Superior`

.......



### `System.Security.Is_Inferior`

.......


### `System.Security.Security_Mode_Last`






-----------------------------------------------------------------------------------------------
## {#}





-----------------------------------------------------------------------------------------------
## {#}





-----------------------------------------------------------------------------------------------
## {#}





-----------------------------------------------------------------------------------------------
## System Configuration Helper {#helper}

The _system configuration helper_ is a [Realizor helper plugin](../pxcr/helpers.md) generates a
pseudo-module called the _system configuration module_ in a file named `pxcr.system.pxc`. 

.......



-----------------------------------------------------------------------------------------------
## {#}





-----------------------------------------------------------------------------------------------
## {#}





-----------------------------------------------------------------------------------------------
## {#}






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




