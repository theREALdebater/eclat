with Workbench.Program_Options;
with Interfaces, System;

procedure AdaOS.Program_Execution.ELF_32 is

   subtype Elf32_Addr  is Interfaces.Unsigned_32; -- Unsigned program address
   subtype Elf32_Half  is Interfaces.Unsigned_16; -- Unsigned medium integer
   subtype Elf32_Off   is Interfaces.Unsigned_32; -- Unsigned file offset
   subtype Elf32_Sword is Interfaces.Signed_32;   -- Signed large integer
   subtype Elf32_Word  is Interfaces.Unsigned_32; -- Unsigned large integer
   subtype Elf32_Byte  is Interfaces.Unsigned_8   -- Unsigned small integer

   for Elf32_Addr'Size  use 4*8; for Elf32_Addr'Alignment  use 4*8;
   for Elf32_Half'Size  use 2*8; for Elf32_Half'Alignment  use 2*8;
   for Elf32_Off'Size   use 4*8; for Elf32_Off'Alignment   use 4*8;
   for Elf32_Sword'Size use 4*8; for Elf32_Sword'Alignment use 4*8;
   for Elf32_Word'Size  use 4*8; for Elf32_Word'Alignment  use 4*8;
   for Elf32_Byte'Size  use 1*8; for Elf32_Byte'Alignment  use 1*8;

   EI_NIDENT: constant := 16;

   type Ehdr_type_Type is (
      ET_NONE,     -- 0 No file type
      ET_REL,      -- 1 Relocatable file
      ET_EXEC,     -- 2 Executable file
      ET_DYN,      -- 3 Shared object file
      ET_CORE,     -- 4 Core file
      ET_LOPROC,   -- 0xff00 Processor-specific
      ET_HIPROC ); -- 0xffff Processor-specific

   for Ehdr_type_Type'Size      use Elf32_Half'Size;
   for Ehdr_type_Type'Alignment use Elf32_Half'Alignment;

   for Ehdr_type_Type use (
      ET_NONE   => 0,
      ET_REL    => 1,
      ET_EXEC   => 2,
      ET_DYN    => 3,
      ET_CORE   => 4,
      ET_LOPROC => 16#FF00#,
      ET_HIPROC => 16#FFFF# );

   type Ehdr_machine_Type is (
      ET_NONE,          -- 0 No machine
      EM_M32,           -- 1 AT&T WE 32100
      EM_SPARC,         -- 2 SPARC
      EM_386,           -- 3 Intel Architecture
      EM_68K,           -- 4 Motorola 68000
      EM_88K,           -- 5 Motorola 88000
      EM_860,           -- 7 Intel 80860
      EM_MIPS,          -- 8 MIPS RS3000 Big-Endian
      EM_S370,          -- 9 IBM System 370
      EM_MIPS_RS4_BE ); -- 10 MIPS RS4000 Big-Endian
      -- RESERVED 11-16 Reserved for future use [What about 17-65535?]

   for Ehdr_machine_Type'Size      use Elf32_Half'Size;
   for Ehdr_machine_Type'Alignment use Elf32_Half'Alignment;

   for Ehdr_machine_Type use (
      ET_NONE        => 0,
      EM_M32         => 1,
      EM_SPARC       => 2,
      EM_386         => 3,
      EM_68K         => 4,
      EM_88K         => 5,
      EM_860         => 7,
      EM_MIPS        => 8,
      EM_S370        => 9,
      EM_MIPS_RS4_BE => 10 );



   type Elf32_Ehdr is
      record
         e_ident:     array (1..EI_NIDENT) of Elf32_Byte;
         e_type:      Ehdr_type_Type;
         e_machine:   Elf32_Half;
         e_version:   Elf32_Word;
         e_entry:     Elf32_Addr;
         e_phoff:     Elf32_Off;
         e_shoff:     Elf32_Off;
         e_flags:     Elf32_Word;
         e_ehsize:    Elf32_Half;
         e_phentsize: Elf32_Half;
         e_phnum:     Elf32_Half;
         e_shentsize: Elf32_Half;
         e_shnum:     Elf32_Half;
         e_shstrndx:  Elf32_Half;
      end record;



