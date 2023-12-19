-----------------------------------------------------------------------------------------------
# Executable Images

An _executable image_ is the result of realising a set of modules into either an executable
(program) file (on hosted platforms) or a _boot image_ or a _segment image_ (on the AdaOS
Native platform). 

A [system configuration](sysconf.md) file guides the [Realizor](realizor.md) how an executable
image is to be realised. 

.....




For a [hosted platform](targets.md#plat), an executable image will be an executable program of
the host platform. 

For the AdaOS Native platform, an executable image is either a [boot image](#boot) or a set of
[segment images](?????). A boot image or segment image is stored in a file. 









-----------------------------------------------------------------------------------------------
## Executable Program Files








-----------------------------------------------------------------------------------------------
## Realisation Segments {#seg}




The configuration for a realisation can have one or more _realisation segments_, each uniquely
identified within the realisation by a name, which is a [scoped name](../intro/names.md#scop)
scoped within the realisation. 

Each realisation segment is configured to be associated with the file name of an executable
image file. One (and only one) of the segment must be designated as the _boot segment_, and its
associated file will be the _boot image_. The files of the other segments are _segment images_. 

The purpose of a segment image (that is not the boot image) is that it does not necessarily
need to be resident in memory all the time the computer is up (in between being booted and
being shut down). It will need to be resident in memory for any of its elements to be used, but
not otherwise. 

Each of the modules of the realisation is assigned to a segment. 

Each segment will contain the realised machine code or data corresponding to the elements
within the modules assigned to it. 

Every segment except the boot segment is initially non-resident in memory. To make it resident,
it must be _loaded_. It becomes non-resident again if it is _purged_. 

The simplest structure a realisation can have is one segment, which will (necessarily) be the
boot segment. The realisation will therefore generate only a boot image (and no segment
images). 

Otherwise, within the realisation, a set of _combos_ are configured. Each combo is associated
with a set of segments, called its _members_. The boot segment is implicitly a member of every
combo, so it doesn't need to be explicitly configured (and isn't allowed to be). 

At any one time whilst the system is up, one of the combos is active. The members (segments) of
the active combo ......



Every procedure in (all of the modules comprising) the realisation is automatically associated
with a set of combos that .......

Whenever a call is made to a procedure, the Realizor inserts code into the call preamble code
to check if the active combo is in the set of combos that the procedure is compatible with. If
it is not, then a switch is made to one of the combos it is compatible with. 

The choice of combo, in this situation, .......







-----------------------------------------------------------------------------------------------
## Bootable Images








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








