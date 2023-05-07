
 -----------------------------------------------------------------------------------------------
# Booting

This document applies only to the AdaOS Native [platform](targets.md#plat). 

.....

Bootstrapping a computer is the process .....

.....




-----------------------------------------------------------------------------------------------
## Boot Volumes and Boot Devices {#vol}

A computer has one storage device assigned as the _boot device_ of the computer. The [boot
sector](#bsec) of this device is used as the boot sector of the computer, and may contain a
[master boot record](#mbr) or a [volume boot record](#vbr), and the boot device may also
contain a [GUID partition table](#gpt). 

.....

In addition to the boot device, a computer has a volume (or partition) assigned as the _boot
volume_ of the computer. This can be any storage volume that has a filesystem supported by
AdaOS and is readable at boot time. 

All the boot images and the initial segment image must be stored as files on the boot volume,
so that they can be loaded and executed by prior stages in the bootstrap process. 



-----------------------------------------------------------------------------------------------
## Boot Images {#img}

For the AdaOS Native platform, an executable image is either a _boot image_ or a set of _segment images_. 



Each segment image file is stored in a separate file, .....



......


(permanently) take over control of the system until the system is
next restarted (or fully powered down and then powered up again). 

......



A _boot image_ is a file that contains the image (binary data), in a file, of: 

 * A [primary bootloader](#btldr1); 

 * A [secondary bootloader](#btldr2); 

 * A [tertiary bootloader](#btldr3); 

 * 






-----------------------------------------------------------------------------------------------
## Initial Segment {#iseg}

On the AdaOS Native [platform](targets.md#plat), an [executable image](images.md) will be made
up of a set of segment images. 

One of these segments will be configured as the _initial segment_. 

It is the initial segment 





-----------------------------------------------------------------------------------------------
## Initial Module {#imod}

An [executable image](images.md), or a set of segment images (AdaOS Native), will almost always
be [realised](realizor.md) from multiple [modules](modules.md). 

On a hosted [platform](targets.md#plat), of these modules, there must be exactly one that is
configured as the _initial module_ of the image (or set of images). 

On the AdaOS Native platform, one of the segments will be configured as the _initial segment_,
and within that segment, one of the contributing modules will be configured as the initial
module. 





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
## Tertiary Bootloader {#btldr3}

The _tertiary bootloader_ is something only AdaOS Native has, and is a relatively small and
simple piece of software whose job is to: 

 1. Load the [initial segment](#iseg) image file into memory, from a filesystem supported by
    AdaOS; 

 2. Hand over control (permanently) to the initial segment, by jumping to its [module
    initialisation procedure](modules.md#init). 

### PXCB3

The Realizor is accompanied by a set of tertiary bootloaders named `AdaOS.PXCB3.x` where `x`
indicates the _tertiary boot architecture_: 

.....




-----------------------------------------------------------------------------------------------
## Secondary Bootloaders {#btldr2}

A _secondary bootloader_, or _second-stage bootloader_, is a relatively small and simple piece
of software whose job is to bring the computer up to the point where the user can choose which
operating system (or which system image) they wish the computer to boot into. 

If the user chooses AdaOS Native, the secondary bootloader then: 

 1. Loads the tertiary bootloader into memory, from a file in a filesystem supported by AdaOS; 

 2. Hands over control to the tertiary bootloader. 

If the user chooses another operating system, the secondary bootloader then boots into the
other operating system. 

The secondary bootloader may be located in a fixed location in the [boot volume](#vol), or it
may be in a file in a filesystem supported by AdaOS. 

.....


### PXCB2

The Realizor is accompanied by a set of secondary bootloaders named `AdaOS.PXCB2.x` where `x`
indicates the _secondary boot architecture_: 

?????

PXCB2 provides the following functionality:

 * Supports two or more different boot options, with a separate (static) configuration for
   each. Supports two or more different operating systems. 

 * Sophisticated set-up functionality for the tertiary bootloader or operating system. 

 * An attractive, robust, reliable, and polished menu allowing the user to select from multiple
   boot options. Suitable for non-technical users. 

 * Escape to a shell, suitable for technical users, which can be used to obtain information and
   perform a variety of system maintenance activities. 

PXCB2 is a [boot image](modules.md) file, and must be stored in a filesystem supported by
AdaOS. 





### Alternatives

However, it is intended to support as many alternative secondary bootloaders as possible. 

Examples of alternative secondary bootloaders include:

 * [GNU GRUB][5]
 * [rEFInd][6], a boot manager (rather than a bootloader)
 * [BOOTMGR][7]
 * [Syslinux][8]
 * [NTLDR][9]
 * [iBoot][10]
 * [Das U-Boot][4]
 * SeaBIOS, which provides PCBIOS services
 * edk2, which provides UEFI services
 * GRUB2, the bootloader used by many Linux distributions
 * depthcharge, a custom boot loader used on Chromebooks

......


-----------------------------------------------------------------------------------------------
## Primary Bootloaders {#btldr1}

A _primary bootloader_, or _first-stage bootloader_, is a very small and simple piece of
software. It is to loaded into (main) memory, unless it is already there because it is
stored in non-volatile memory (such as ROM or Flash storage, for example), and then it: 

 1. Loads the [secondary bootloader](#btldr2) into (main) memory; 

 2. Passes control to the secondary bootloader. 

Or, alternatively:

 1. Loads the [tertiary bootloader](#btldr3) into (main) memory; 

 2. Passes control to the tertiary bootloader. 

The next bootloader in the chain (secondary or tertiary), may be loaded from a fixed location
in the [boot volume](#vol), or from a file in a filesystem supported by AdaOS.


......




### PXCB1

The Realizor is accompanied by a set of primary bootloaders named `AdaOS.PXCB1.x` where `x` is
the boot architecture: 

| `PC64`    | 64-bit IBM PC based (ia-64, for example ?????)
| `PC32`    | 32-bit IBM PC based (ia-32, for example i386 CPU based motherboards)
| `PC16`    | 16-bit IBM PC based (for example 8086)
| `ARM6`    | ARM Thumb based
.....

PXCB1 provides the following functionality:

 * Init and clear Cache-As-RAM for heap and stack; 
 * Set up task, registers, stack pointer, etc.; 
 * Do microcode updates; 
 * Init timers; 
 * Switching from 16-bit real-mode to 32-bit protected mode; 
 * Init TPM and Root of Trust; 
 * Init DRAM and clear; 
 * Switch to regular DRAM and tear down Cache-As-RAM; 
 * Init PCI device; 
 * Init On-chip devices; 
 * Init graphics; 
 * Init CPU (e.g. set up SMM); 
 * Write-protect boot media; 
 * Lock security-related registers; 
 * Lock SMM mode; 
 * Initialise GPIO pins; 
 * Load the secondary bootloader; 
 * Decompress the secondary bootloader?????

Many of these areas of functionality are specific to a processor model, processor architecture,
mainboard model or family, and peripheral options. Areas of functionality are divided into
different modules, to allow the desired and appropriate functionality to be selected in a
configuration. 


### Alternatives

However, it is intended to support as many alternative primary bootloaders as possible. 

Examples of alternative primary bootloaders include:

 * [BIOS][1]
 * [coreboot][2]
 * [Libreboot][3], a coewboot distribution
 * [DasU-Boot][4]

The secondary bootloader is the 'payload' in coreboot nomenclature. 



.....







-----------------------------------------------------------------------------------------------
## Master Boot Record (MBR) {mbr#}

On a storage device which is formatted using the IBM PC partitioning system (the hard disk
partitioning system of the original IBM XT computer product), .....





On a computer which has an MBR (on its boot volume), the MBR is the first thing to be loaded
into memory (possibly Cache-as-RAM), and its only job is to load and pass control to the
primary bootloader of one of the partitions it defines. 

See [Wikipedia](https://en.wikipedia.org/wiki/Master_boot_record) for more details of the structure 
of the MBR.

The [Boot Installer](#inst) is able to set the various parameters within the MBR to correct
values, and then write an MBR image (file) into the boot sector of a disk. 

The Realizor is accompanied by an _MBR image_ named, which is merely a tiny piece of 8086 instruction
set machine code in a file (named `AdaOS.MBR.bin`).  

There are other MBR images available, and any of these can be used instead. Be wary, however,
of security considerations. 

.....



-----------------------------------------------------------------------------------------------
## Volume Boot Record (VBR) {#vbr}







-----------------------------------------------------------------------------------------------
## GUID Partition Tables (GPT) {#gpt}

A _GUID Partition Table_ (or _GPT_) .....

See [Wikipedia](https://en.wikipedia.org/wiki/GUID_Partition_Table) for more details
about GPTs.

The [Boot Installer](#inst) is able to generate a correct GPT and write it into the correct
location (just after the boot sector) of the computer's [boot device](#vol), in conjunction
with writing the [MBR](#mbr). 

.....




-----------------------------------------------------------------------------------------------
## Boot Sector {#bsec}

The _boot sector_ of the [boot volume](#vol) ......



-----------------------------------------------------------------------------------------------
## {#}







-----------------------------------------------------------------------------------------------
## Boot Installer {#inst}

The Realizor is accompanied by a library procedure called the __Boot Installer__.

The Boot Installer is used to put a set of [boot images](#img), as emitted by the
[Realizor](realizor.md), into or onto a storage volume---normally the [boot volume](#vol) of
the computer on which the Boot Installer is run---in such a way as that it can be booted from
at the next restart (or power up) of the computer. 

Various options allow the behaviour of the Boot Installer to be configured .....




 * Copy a .....

 * Copy a [tertiary bootloader](#btldr3) .....

 * .....

 * Write an [MBR](#mbr) or [VBR](#vbr) image (in a file) onto the volume's [boot
   sector](#bsec), having updated the parameter areas within to the correct values. 
   An alternative is to re-use an existing MBR (already in the boot sector of the boot device)
   The use of
   a [GPT](#gpt) is supported. 





The Boot Installer is wrapped as a [plugin](plugins.md) for a variety of programs, such as the
[Kantan](?????) package manager for example, and also as a standalone [command-line
tool](../tools/tools.md). 




-----------------------------------------------------------------------------------------------
## {#}







-----------------------------------------------------------------------------------------------
## {#}







-----------------------------------------------------------------------------------------------
## {#}









-----------------------------------------------------------------------------------------------
## References

[1]: <> "BIOS"

[2]: <https://www.coreboot.org> "coreboot"

[3]: <https://libreboot.org> "Libreboot Project"

[4]: <> "Das U-Boot"

[5]: <> "GNU GRUB"

[6]: <http://www.rodsbooks.com/refind/> "The rEFInd Boot Manager"

[7]: <> "BOOTMGR"

[8]: <> "Syslinux"

[9]: <> "NTLDR"

[10]: <> "iBoot"





