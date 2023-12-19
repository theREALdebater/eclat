-----------------------------------------------------------------------------------------------
# RISC-V







-----------------------------------------------------------------------------------------------
## {#}






-----------------------------------------------------------------------------------------------
## {#}






-----------------------------------------------------------------------------------------------
## {#}






-----------------------------------------------------------------------------------------------
## {#}

The package `System.Machine_Code.RISCV` contains the following visible declarations: 

```ada
-- Op-codes (this is an assembly op-code, rather than bits 0-6 of an instruction):
type Op_Code is 
(
   -- I-format:
   ADDI,    -- add immediate
   MV,      -- move (copy) register-to-register
   SLTI,    -- set (1 into dest) if less than immediate (signed)
   SLTIU,   -- set (1 into dest) if less than immediate, unsigned
   SEQZ,    -- set (1 into dest) if equal to zero
   ANDI,    -- logical 'and' immediate
   ORI,     -- logical 'or' immediate
   XORI,    -- logical 'exclusive or' immediate
   SLLI,    -- shift left logical immediate
   SRLI,    -- shift right logical immediate
   SRAI,    -- shift right arithmetic immediate
   NOP,     -- no operation
   JALR,    -- jump and link register
   LW,      -- load word (32-bit) (Src is base address)
   LH,      -- load half (16-bit) (sign-extended) (Src is base address)
   LHU,     -- load half (16-bit), zero-extended (Src is base address)
   LB,      -- load byte (8-bit) (sign-extended) (Src is base address)
   LBU,     -- load byte (8-bit), zero-extended (Src is base address)
   ECALL,   -- environment call (src and dest must be 0, value identifies function)
   EBREAK,  -- environment break (src, dest, and value must all be 0)

   -- U-format:
   LUI,     -- load upper immediate
   AUIPC,   -- add upper immediate to PC
   ????? do we need to allow 32-bit immediate value?

   -- R-format:
   ADD,     -- add (registers)
   SUB,     -- subtract (registers)
   SLT,     -- set (1 into dest) if less than (signed)
   SLTU,    -- set (1 into dest) if less than, unsigned
   SNEZ,    -- set (1 into dest) if not equal to zero
   AND,     -- logical 'and' (registers)
   OR,      -- logical 'or' (registers)
   XOR,     -- logical 'exclusive or' (registers)
   SLL,     -- shift left logical (registers)
   SRL,     -- shift right logical (registers)
   SRA,     -- shift right arithmetic (registers)

   -- J-format:
   JAL,     -- jump and link
   J,       -- jump

   -- B-format:
   BEQ,     -- branch if equal
   BNE,     -- branch if not equal
   BLT,     -- branch if less than (signed)
   BLTU,    -- branch if less than, unsigned
   BGT,     -- branch if greater than (signed)
   BGTU,    -- branch if greater than, unsigned

   -- S-format:
   SW,      -- store word (32-bit) 
   SH,      -- store half (bottom 16-bits) 
   SB       -- store byte (bottom 8-bits)
);

-- Register number (standard ISAs):
type Reg32 is mod 32;

-- 12-bit sign-extended immediate value: 
subtype Signed_12 is Interfaces.Unsigned_64 range -(2**11) .. (2**11 - 1); 

-- 20-bit sign-extended immediate value: 
subtype Signed_20 is Interfaces.Unsigned_64 range -(2**19) .. (2**19 - 1); 

-- 
type Access_Type
is
   (Memory_Write, Memory_Read, Device_Output, Device_Input);

type Mem_Ordering
is
   array (Access_Type) of Boolean;

type Fence_Mode is (Normal, TSO);
```


-----------------------------------------------------------------------------------------------
## {#}

The package `System.Machine_Code.RISCV.RV32I` contains the following visible declarations: 

```ada
-- I ISA, R-format instruction format:
type fIR 
is 
   record
      Op:   Op_Code; -- assembly op-code
      Src1: Reg32; -- first source register
      Src2: Reg32; -- second source register
      Dest: Reg32; -- destination register
   end record;

-- I ISA, I-format instruction format:
type fII
is 
   record
      Op:   Op_Code; -- assembly op-code
      Src:  Reg32; -- source (or base address) register
      Dest: Reg32; -- destination register
      Val:  Signed_12; -- immediate value (or offset, for load)
   end record;

-- I ISA, S-format instruction format:
type fIS
is 
   record
      Op:   Op_Code; -- assembly op-code
      Base: Reg32; -- base address register
      Src:  Reg32; -- source value register
      Val:  Signed_12; -- immediate value (or offset, for store) 
   end record;

-- I ISA, U-format instruction format:
type fIU
is 
   record
      Op:   Op_Code; -- assembly op-code
      Dest: Reg32; -- destination register
      Val:  Signed_20; -- immediate value 
   end record;

-- I ISA, B-format instruction format:
type fIB
is 
   record
      Op:   Op_Code; -- assembly op-code
      Src1: Reg32; -- first source register
      Src2: Reg32; -- second source register
      Targ: Address; -- target address, must be within +/- 4 KiB
   end record;

-- I ISA, J-format instruction format:
type fIJ
is 
   record
      Op:   Op_Code; -- assembly op-code
      Dest: Reg32; -- destination register
      Targ: Address; -- target address, must be within +/- 1 MiB
   end record;

-- Special format for the FENCE instruction:
type fII_FENCE
is
   record
      Successor, Predecessor: Mem_Ordering;
      Mode:  Fence_Mode := Normal;
   end record;
```











-----------------------------------------------------------------------------------------------
## {#}






-----------------------------------------------------------------------------------------------
## {#}






-----------------------------------------------------------------------------------------------
## {#}






-----------------------------------------------------------------------------------------------
## {#}










