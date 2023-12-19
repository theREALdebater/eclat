-----------------------------------------------------------------------------------------------
# Targets

A _target_ is the combination of: 

 * a _architecture_

 * a _platform_
 
 * a run time system _variant_

Native machine code is generated on the assumption of a specific target; an attempt to run it
on a different target is likely to fail. 



-----------------------------------------------------------------------------------------------
## Architecture {#arch}

An _architecture_ defines a machine code instruction set, plus certain other things that are 
closely associated with it, such as registers, memory addressing, and certain fundamental 
peripheral circuitry. 

Examples of architectures are:

 * AMD64 (x86-64, x64) 
 
 * ARMv8 (AArch64)

 * RISC-V (RISCV)


### Sub-architectures

An architecture can have two or more _sub-architectures_. 

For example the ????? has:

 * Two cores that execute the ????? (Thumb) ARM instruction set (with
   its accompanying register model, etc.); 

 * Four cores that execute the ????? (?????) ARM instruction set (with
   its accompanying register model, etc.).

In this case, the architecture is ????? and it has two sub-architectures: ?????; ?????.

The machine code being executed by a Thumb core can call subroutines that execute in ?????, and
vice versa. 

To accommodate this kind of target, different [image segments](images.md#seg) can be configured
to be associated with different sub-architectures ......


......





### 

See also: https://en.wikipedia.org/wiki/Uname

    alpha

    arc

    arm
    aarch64_be (arm64)
    aarch64 (arm64)
    armv8b (arm64 compat)

    armv8l (arm64 compat)

    blackfin
    c6x
    cris
    frv
    h8300
    hexagon
    ia64
    m32r
    m68k
    metag
    microblaze
    mips (native or compat)
    mips64 (mips)
    mn10300
    nios2
    openrisc
    parisc (native or compat)
    parisc64 (parisc)
    ppc (powerpc native or compat)
    ppc64 (powerpc)
    ppcle (powerpc native or compat)
    ppc64le (powerpc)
    s390 (s390x compat)
    s390x
    score
    sh
    sh64 (sh)
    sparc (native or compat)
    sparc64 (sparc)
    tile
    unicore32
    i386 (x86)
    i686 (x86 compat)
    x86_64 (x64)
    xtensa














-----------------------------------------------------------------------------------------------
## Platforms {#plat}

A _platform_ defines a set of interfaces that are provided by an operating system, system 
monitor, kernel, or whichever system software has the purpose of enabling general-purpose 
('application') software to run successfully on computers that have minor hardware differences. 

Examples of platforms are: 

 * _Windows_: Microsoft Windows 10 and Windows Server 2016/2019

 * _Linux_: the Linux ABI (LSB v5.0)
 
 * _AdaOS Native_: the Ada Object System Native Kernel

When running on top of a _hosted platform_, such as Windows or Linux, .....



entirely within the run time system. 

Note that, for all platforms, the [run time system](../rts/rts.md) 
.....

.....



-----------------------------------------------------------------------------------------------
## RTS Variants {#variant}

Even on the same architecture and platform, the [run time system](../rts/rts.md) may need to do 
certain things differently, or have different levels of functionality. There may, therefore, be 
two or more different _variant_ run time systems for the same platform. 

In general, each different architecture and platform combination will also have the following 
three variants: 







-----------------------------------------------------------------------------------------------
## Names

Each target is identified by a name that succinctly encodes the:

 * processor architecture (its precise variation)
 
 * platform (operating system)
 
 * run time system variant
 
 * any other target distinction

The name therefore generally takes one of the following forms:

 * `A_P`
 
 * `A_P_V`
 
 * `A_P_V_X`
 
where `A` represents the architecture, `P` the platform, `V` the RTS variant (if any), and `X` 
any further distinction (if any). The names are case-insensitive but conventionally in lower 
case. 

.....

??????The following targets are currently (April 2021) supported:

 * `x64_mswin_tty`
 
 * `x64_linux_tty`

 * `x64_mswin_svc`
 
 * `x64_linux_svc`


### AMD64

The `x64` part of the target names denotes the AMD64 architecture (also known as x86-64, 
x86_64, x86-64, IA-32e, EM64T, and Intel 64), executing in long mode only. 

Instruction set extensions and any differences between the many different implementations 
(actual processor models) of AMD64 are avoided. 

.....


### Microsoft Windows

The `mswin` part of target names denotes that the executable will be in the [Portable 
Executable][1] format (PE) and use the [Microsoft Win32 API][2]. 

The Realizor only generates calls to the RTS; it is the RTS itself that makes calls into the 
Win32 API. 
   
.....

Linking with Windows PE [Dynamic Link Libraries](DLL.md) is supported. 


### Linux RTS 

The `linux` part of target names denotes that the executable will be in the [Executable and 
Linkable Format][3] (ELF) and use the [Linux Kernel API][4] "kernel�user space" API. 

The Realizor only generates calls to the RTS; it is the RTS itself that makes calls into the 
Linux Kernel API. 
   
.....

Linking with ELF `.so` [Shared Object](DLL.md) modules is supported. 


### Teletype

The `tty` part of target names denotes the command-line console ('Teletype', or 'TTY') variant 
of the RTS. This is of no significance to the Realizor, .....


### Service

The 'svc' part of target names denotes the service variant of the RTS. This is of no 
significance to the Realizor, .....



-----------------------------------------------------------------------------------------------
## Configuration of the Target

The [executable image](images.md) generated by the Realizor will always be for a single, 
specific target, the _selected target_ of that image. 

The Realizor is written principally in Ada, and must itself be compiled, built, and realised to 
make it into an executable. The target that is selected when realising a version of the 
Realizor is termed the _self-target_ of that version of the Realizor. 

The selected target can be configured, but if no target is explicitly configured then the 
Realizor will select its self-target. 

The target of an image can be explicitly set by configuration of the form:

    pxcr:
       images:
          -
             id: X 
             target: T
    
where `X` is the name of the image and `T` is the target name. If the target name is not 
supported, the realisation will fail (with an exception being propagated). 

The default file name for the output executable is `Xt` where X is the name of the realisation 
and `t` is the file type (extension) appropriate for the executable as follows:

| Platform       | Kind of Executable | File Type
| -------------- | ------------------ | -------------
| Linux          | Main               | (empty)
| Linux          | DLL                | `.so`
| Windows        | Main               | `.exe`
| Windows        | DLL                | `.dll`

If the location or file name of the executable generated by realisation `X` needs to be 
changed, it can be done by the command:

    PXCR/Bin/X/Output
    
    pxcr image X output F
    
where `X` is the name of the image and `F` is a path. 

If `F` is a path, it can be absolute or relative to the current working directory. 

`F` can be the path of the output file (including its name and file type/extension). If the 
extension is omitted, it defaults to `t` as defined above. If the file name is omitted (the 
trailing path delimiter, e.g. `/` or `\`, must not be omitted), then the default file name is 
used, but it is placed in the directory named by the path. 

If no path is specified, the default is the default file name in the current working directory. 



-----------------------------------------------------------------------------------------------
## Target Identifiers

In Ada source text, the values of the enumerated type `System.Name` are all the targets 
supported by the Realizor. The value of the constant `System.System_Name` is the selected 
target. These are both set by the Realizor at realisation time. The value of the constant is, 
nevertheless, static. 

Each different target is, therefore, identified by a non-negative integer which is the value of 

    System.Name'Pos (T)
    
where `T` is a value of `System.Name`. 

.....



-----------------------------------------------------------------------------------------------
## Trademarks

"Microsoft" and "Windows" are registered trademarks. 



-----------------------------------------------------------------------------------------------
## References

[1]: <https://docs.microsoft.com/en-us/windows/win32/debug/pe-format> ""
[2]: <https://docs.microsoft.com/en-us/windows/win32/apiindex/windows-api-list> ""
[3]: <https://www.kernel.org/doc/html/v4.12/core-api/kernel-api.html> ""
[4]: <https://refspecs.linuxfoundation.org/elf/elf.pdf> ""




