-----------------------------------------------------------------------------------------------
# Configuration Summary

The configuration items used by the ECLAT and Realizor (and other associated) tools are listed 
here, with a brief description. Detailed information about these items is to be found 
throughout the ECLAT documentation. 



-----------------------------------------------------------------------------------------------
## Installation

...........

| Name              | Contains                                                 |
| ----------------- | -------------------------------------------------------- |
| `bin`             | tools executable files                                   |
|                   |                                                          |
|                   |                                                          |
|                   |                                                          |
|                   |                                                          |




-----------------------------------------------------------------------------------------------
## Environment Variables

The following environment variables need to be set: 

| Name               | Contains                                                  |
| ------------------ | --------------------------------------------------------- |
| `PATH`             | Should include ECLAT tools `bin` directory                |
| `ECLAT_SVC`        | Full path of ECLAT Service saved state file               |
| `PXCR_SVC`         | Full path of Realizor Service saved state file            |
?????| `ECLAT_RTS`        | Full path of Run Time System saved state file            |
?????| `TETHYS_SVC`       | Full path of Tethys Service saved state file             |
| `ALDUS_SVC`        | Full path of ALDUS Service saved state file               |
| `ECLAT_CWL`        | Name of the Current Working Library                       |
| `ALDUS_CWS`        | Name of the Current Working Sanctuary                     |
| `?????_SVC`        | Full path of ????? Service saved state file               |
| `?????_SVC`        | Full path of ????? Service saved state file               |
| `DEFAUTH`         | Default authority, as a decimal integer                      |

............

On hosted platforms, the `PATH` variable should include the ECLAT tools `bin` directory, so 
that those tools can be executed (without having to give the full path to them every time). On 
the AdaOS platform it is irrelevant. 

On the AdaOS platform, the environment variable `DEFAUTH` needs to be set. It should be the 
number of the default authority to be used, expressed as a decimal integer with no spaces or 
punctuation. If this environment variable is not set, the value `0` is assumed by default, 
which is the number of the top authority. Since, on hosted platforms, this is the authority 
used throughout, it makes sense to simply not set this environment variable on hosted 
platforms. 

...........



-----------------------------------------------------------------------------------------------
## Saved States

The following saved state files are used by ECLAT and its attendant software:

 * ``, for the [Realizor service](../services/pxcr.md)

 * ``, for the [ECLAT service](../services/eclat.md)

 * ``, for the [run time system](../rts/rts.md)

 * ``, the [Tethys service](../services/tethys.md)
 
 * .........................

In general, every update item's path begins with the name of the saved state it belongs to: 

| Saved State For | Item name begins with |
| --------------- | --------------------- |
| Realizor        | `pxcr`                |
| ECLAT           | `eclat`               |
?????| RTS             | `rts`                 |
?????| Tethys          | `tethys`              |

However, there is no harm in putting a mixture of all kinds of item in one state update file 
`F,stuf.txt`, and running `eclat-stuf S F` to update the saved state file `S` (which will 
harmlessly ignore items that do not belong in that saved state file). 



-----------------------------------------------------------------------------------------------
## ......

The following letters are used, in the update item paths, to mean the following:

| Symbol | Meaning                                       |
| ------ | --------------------------------------------- |
| L      | Library name                                  |
| U      | Library unit name                             |
| S      | Programming language name                     |
| B      | Build name                                    |
| P      | Partition name                                |
| C      | Module class name OR plugin class name        |
| M      | Module name                                   |
| N      | Module element (import or export) name        |
| X      | Executable image name                         |
| A      | Program partition name                         |
| Z      | Program name                                  |
| E      | Environment name                              |
| V      | Environment variable name OR library version  |
|        |                                               |
|        |                                               |

The names of libraries, builds, modules, module elements, module classes, plugin classes, executable images, 
and programs are [ECLAT entity names](names.md). 



-----------------------------------------------------------------------------------------------
## Programming Languages

A programming language name can be any of: 

 * `ada` 
 * `c`
 * `cpp`



-----------------------------------------------------------------------------------------------
## Summary of State Update Items

Abbreviations:

 * FP = file path

 * DP = directory path

 * RDP = root directory path
 
 * FPPL = file path pattern list
 
 * MIP = module initialisation procedure
 
 * FQN = fully qualified name 
 
Build mode value must be one of: `partition`; `plugin`; `module`. 

Any 'list' is a string list separated by `|`

| Path                                          | Purpose
| --------------------------------------------- | ------------------------------------------------------ |
?????| `eclat/directories/root`                      | RDP for all data related to ECLAT                      |
| `eclat/directories/source`                    | RDP for source root directories                        |
| `eclat/directories/libraries`                 | RDP for library directories                            |
| `eclat/directories/builds`                    | RDP for module files                                   |

| `eclat/libraries(L)/source/S`                 | RDP for lang `S` source files for library `L`          |
| `eclat/libraries(L)/wrapped_files(V)`         | FP for wrapped library `L` version `V`                 |
| `eclat/libraries(L)/unwrapped_directory`      | DP for unwrapped files of library `L`                  |
| `eclat/library(L1)/dependencies(L2)/versions` | Acceptable versions for library dependency             |

| `eclat/builds(B)/mode`                        | Build mode for build `B`                               |
| `eclat/builds(B)/modules(M)/file`             | FP for (output of) module `M` of build `B`             |
| `eclat/builds(B)/modules(M)/fulfils`          | Fulfilments list                                       |
| `eclat/builds(B)/modules(M)/needs`            | Requirements list                                      |
| `eclat/builds(B)/modules(M)/init`             | Export name of MIP                                     |

?????| `eclat/builds(B)/modules(M)/imports`          | Imports list for module `M` of build `B`               |

| `eclat/builds(B)/modules(M)/exports`          | Exports list for module `M` of build `B`               |
| `eclat/builds(B)/modules(M)/plugin_classes`   | Plugin class list for plugin `M` of build `B`          |

| `eclat/builds(B)/partitions(P)/units`         | Libary units that must be included in partition `P`    |
| `eclat/builds(B)/main_unit`                   | Main subprogram                                        |
| `eclat/builds(B)/programs(Z)/`     |                                                        |
| `eclat/`                                      |                                                        |
| `eclat/`                                      |                                                        |
| `eclat/`                                      |                                                        |
| `eclat/`                                      |                                                        |

| `pxcr/directories/root`                       | RDP for all data related to the Realizor               |
| `pxcr/directories/images`                     | RDP for image files                                    |
| `pxcr/default/target`                         | Self-target of Realizor                                |

| `pxcr/images(X)/file_type`                    | `bin` (no file ext), `so`, `exe`, or `dll`             |
| `pxcr/images(X)/target`                       | Target for image `X`                                   |
| `pxcr/images(X)/file`                         | FP for (output of) image `X`                           |
| `pxcr/images(X)/plugins(C)/search_paths`      | FPPL of plugin module files to include for class `C`   |
| `pxcr/images(X)/plugins(C)/init`              | Export name of MIP for plugin class `C`                |

| `pxcr/images(X)/`                             |                                                        |
| `pxcr/images(X)/`                             |                                                        |
| `pxcr/images(X)/`                             |                                                        |
| `pxcr/images(X)/`                             |                                                        |

| `rts/images(X)/sit/length`                    | Length of the System Instance Table of image `X`       |
| `rts/images(X)/partitions(A)/init_procs`      | List of export names of MIPs of partition `A`           |
| `rts/images(X)/partitions(A)/main_proc`       | Export name of main procedure of partition `A`          |
| `rts/images(X)/initial_partition/name`         | Name of the initial partition of image `X`              |
| `rts/images(X)/initial_partition/args`         | Program arguments for the initial partition             |
| `rts/images(X)/initial_partition/vars(V)`      | Environment variables for the initial partition         |
|                                               | 
| `tethys/environments(E)/vars(V)`              | Value for environment variable named `V`               |
| `tethys/environments(E)/vars_inherit`         | List of environment variables to be inherited          |
| `tethys/environments(E)/limits`               | 
| `tethys/environments(E)/services`             | List of services in service directory                  |
| `tethys/environments(E)/programs`             | List of programs in program directory                  |
| `tethys/environments(E)/`                     | 
| `tethys/environments(E)/`                     | 
|                                               | 
| `tethys/partitions(A)/init`               | List of partitions (partitions) of program `P`         |
| `tethys/programs(P)/partitions`               | List of partitions (partitions) of program `P`         |
| `tethys/programs(P)/main_partition`            | Main partition of program `P`                           |
| `tethys/programs(P)/environment`              | Environment to use as basis for executing progam `P`   |

| `partition_helper/builds(B)/partitions(A)/`     |                                                        |
| `partition_helper/builds(B)/partitions(A)`     |                                                        |
| `partition_helper/builds(B)/partitions(A)`     |                                                        |
| ``                                            |                                                        |
| ``                                            |                                                        |
| ``                                            |                                                        |
| ``                                            |                                                        |
| ``                                            |                                                        |
| ``                                            |                                                        |
| ``                                            |                                                        |
| ``                                            |                                                        |



-----------------------------------------------------------------------------------------------
## Defaults

Almost every configuration item used by the ECLAT and Realizor (and other associated) tools has 
a default value. The current defaults are presented here. 

.....

| Path                           | Default
| ------------------------------ | ------------------------------------------------------------
| ECLAT/Dir/Root                 | ${ENV/ECLAT}
| ECLAT/Dir/Source               | ${ECLAT/Dir/Root}/Source
| ECLAT/Dir/Lib                  | ${ECLAT/Dir/Root}/Lib
| ECLAT/Dir/Build                | ${ECLAT/Dir/Root}/Build
| ECLAT/Dir/Bin                  | ${ECLAT/Dir/Root}/Bin
| PXCR/Dir/Bin                   | ${ECLAT/Dir/Bin}

| ECLAT/Lib/L/Source/S           | ${ECLAT/Dir/Source}/L/S
| ECLAT/Lib/L/Unwrapped          | ${ECLAT/Dir/Lib}/L
| ECLAT/Build/M/Output           | ${ECLAT/Dir/Build}/M.pxc
| PXCR/Bin/X/Output              | ${PXCR/Dir/Bin}/X${PXCR/Bin/X/Exec_File_Type}
| PXCR/Bin/X/Target              | ${PXCR/Default/Target}

| ECLAT/Build/M/Import/N/Type    | Dead
| ECLAT/Build/M/Import/N/Name    | N
| ECLAT/Build/M/Export/N/Type    | Dead
| ECLAT/Build/M/Export/N/Name    | N
| ECLAT/                         | 
| ECLAT/                         | 
| ECLAT/                         | 
| ECLAT/                         | 
| ECLAT/                         | 
| ECLAT/                         | 
| ECLAT/                         | 
| PXCR/Bin/X                     | 
| PXCR/Bin/X                     | 
| PXCR/Bin/X                     | 
| PXCR/Bin/X                     | 
| PXCR/Bin/X                     | 
| PXCR/Bin/X                     | 
| PXCR/Bin/X                     | 
| PXCR/Bin/X                     | 
| PXCR/Bin/X                     | 
| PXCR/Bin/X                     | 
|                                | 
|                                | 
|                                | 
|                                | 
|                                | 
|                                | 
|                                | 
|                                | 
|                                | 




-----------------------------------------------------------------------------------------------
## The ECLAT Root and Base Directories

.....

There is one directory, the _ECLAT root directory_, that has no default and must be either 
configured or whose full path must be in the environment variable named `ECLAT`. 

By default, all files associated with ECLAT are stored in the directory subtree rooted at the 
ECLAT root directory. Different locations can be configured explicitly if required. 

The ECLAT root directory is defined by an update item with the path:

    eclat/directories/root
    
The value of this configuration item is a directory path. The default is `${ENV/ECLAT:Find}`.

Directly within the ECLAT root directory is, by default, a set of directories called the 
_ECLAT base directories_: 

 * the _source base directory_, for the source text files that are to be compiled into libraries
 * the _libraries base directory_, for the files that are generated when creating or updating libraries
 * the _build base directory_, for the files that are generated when building builds
 * the _executable base directory_, for the files that are generated by the Realizor
 * .....

.....

A base directory is defined by an update item with the path:

    eclat/directories/D
    
where `D` is one of: 

 * `sources` for the _source base directory_; 
 * `libraries` for the _libraries base directory_; 
 * `builds` for the _builds base directory_; 
 
In addition there is:
 
 * `pxcr/directories/images` for the _executable images base directory_. 
    
The value of these update items is a directory path. The defaults are:

| Base Directory           | Default                           |
| ------------------------ | --------------------------------- |
| sources                  | `${eclat/directories/root}/src`   |
| libraries                | `${eclat/directories/root}/lib`   |
| builds                   | `${eclat/directories/root}/bld`   |
| executable images        | `${eclat/directories/root}/bin`   |

.....



-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## Module Classes

........

| Module Class    | Description                                                     |
| --------------- | --------------------------------------------------------------- |
| `rts`           | Run Time System
| `svc-ctlr`      | Service Controller
| ``
| ``
| ``
| ``
| ``
| ``

........

..... fulfilments and requirements .....

........

| Module                  | Fulfils                  | Requires                 |
| ----------------------- | ------------------------ | ------------------------ |
| `rts-*-*-*`             | `rts`                    | (depends on the RTS)     |
| ``                      | ``                       | ``                       |
| ``                      | ``                       | ``                       |
| ``                      | ``                       | ``                       |
| `tethys`                | `svc-ctlr`               | `rts`                |
| ``                      | ``                       | ``                       |
| ``                      | ``                       | ``                       |
| ``                      | ``                       | ``                       |
| ``                      | ``                       | ``                       |
| ``                      | ``                       | ``                       |
| ``                      | ``                       | ``                       |
| ``                      | ``                       | ``                       |
| ``                      | ``                       | ``                       |
| ``                      | ``                       | ``                       |

........





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





