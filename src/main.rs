//CPU Registers
struct Registers {
  a: u8,
  b: u8,
  c: u8,
  d: u8,
  e: u8,
  f: FlagsRegister,
  h: u8,
  l: u8,
}

impl Registers {
  fn get_bc(&self) -> u16 {
    (self.b as u16) << 8
    | self.c as u16    
  }
  fn set_bc(&mut self, value: u16) {
    self.b = ((value & 0xFF00) >> 8) as u8;
    self.c = (value & 0xFF) as u8;
  }

  fn get_hl(&self) -> u16 {
    (self.h as u16) << 8
    | self.l as u16    
  }
  fn set_hl(&mut self, value: u16) {
    self.h = ((value & 0xFF00) >> 8) as u8;
    self.l = (value & 0xFF) as u8;
  }
  //also need 'af', 'de', and 'hl'
}

/* Flags register is structured as followed:
   ┌-> Carry
 ┌-+> Subtraction
 | |
1111 0000
| |
└-+> Zero
  └-> Half Carry
(the zeros are completely unused by the cpu)*/
struct FlagsRegister {
  zero: bool,
  subtract: bool, 
  half_carry: bool,
  carry: bool,
}

enum JumpTest {
  NotZero,
  Zero,
  NotCarry,
  Carry,
  Always
}

const ZERO_FLAG_BYTE_POSITION: u8 = 7;
const SUBTRACT_FLAG_BYTE_POSITION: u8 = 6;
const HALF_CARRY_FLAG_BYTE_POSITION: u8 = 5;
const CARRY_FLAG_BYTE_POSITION: u8 = 4;

impl std::convert::From<FlagsRegister> for u8 {
  fn from(flag: FlagsRegister) -> u8 {
    (if flag.zero       { 1 } else { 0 }) << ZERO_FLAG_BYTE_POSITION |
    (if flag.subtract   { 1 } else { 0 }) << SUBTRACT_FLAG_BYTE_POSITION |
    (if flag.half_carry { 1 } else { 0 }) << HALF_CARRY_FLAG_BYTE_POSITION |
    (if flag.carry      { 1 } else { 0 }) << CARRY_FLAG_BYTE_POSITION
  }
}

impl std::convert::From<u8> for FlagsRegister {
  fn from(byte: u8) -> Self {
    let zero = ((byte >> ZERO_FLAG_BYTE_POSITION) & 0b1) != 0;
    let subtract = ((byte >> SUBTRACT_FLAG_BYTE_POSITION) & 0b1) != 0;
    let half_carry = ((byte >> HALF_CARRY_FLAG_BYTE_POSITION) & 0b1) != 0;
    let carry = ((byte >> CARRY_FLAG_BYTE_POSITION) & 0b1) != 0;

    FlagsRegister {
      zero,
      subtract,
      half_carry,
      carry
    }
  }
}

//CPU Register Instructions
enum Instruction {
  ADD(ArithmeticTarget),
  
  JP(JumpTest),

  LD(LoadType),

  PUSH(StackTarget),
  POP(StackTarget),

  INC(IncDecTarget),

  RLC(PrefixTarget),

  CALL(JumpTest),
  RET(JumpTest),

  NOP(),
  HALT(),
}

impl Instruction {
  fn from_byte(byte: u8, prefixed:bool) -> Option<Instruction> {
    if prefixed {
      Instruction::from_byte_prefixed(byte)
    } else {
      Instruction::from_byte_not_prefixed(byte)
    }
  }

  fn from_byte_prefixed(byte: u8) -> Option<Instruction> {
    match byte {
      0x00 => Some(Instruction::RLC(PrefixTarget::B)),
      _ => /*todo: add mapping for rest of instructions*/ None
    }
  }

  fn from_byte_not_prefixed(byte: u8) -> Option<Instruction> {
    match byte {
      0x02 => Some(Instruction::INC(IncDecTarget::BC)),
      _ => /*todo: add mapping for rest of instructions*/ None
    }
  }
}

enum ArithmeticTarget {
  A, B, C, D, E, H, L,
}

enum PrefixTarget {
  B,
}

enum StackTarget {
  BC, DE, HL
}

enum IncDecTarget {
  BC, DE, HL
}

enum LoadByteTarget {
  A, B, C, D, E, H, L, HLI
}

enum LoadByteSource {
  A, B, C, D, E, H, L, D8, HLI
}

enum LoadType {
  Byte(LoadByteTarget, LoadByteSource),
}

impl CPU {
  fn step(&mut self) {
    let mut instruction_byte = self.bus.read_byte(self.pc);
    let prefixed = instruction_byte == 0xCB;
    if prefixed {
      instruction_byte = self.bus.read_byte(self.pc + 1);
    }
    let next_pc = if let Some(instruction) = Instruction::from_byte(instruction_byte, prefixed) {
      self.execute(instruction)
    } else {
      let description = format!("0x{}{:x}", if prefixed {"cb"} else {""}, instruction_byte);
      panic!("Unknown instruction found for: {}", description);
    };

    self.pc = next_pc;
  }

  fn read_next_byte() -> u8 {
    0
  }

  fn read_next_word() -> u16 {
    0
  }

  fn execute(&mut self, instruction: Instruction) -> u16{
    if self.is_halted {
      return 0x0
    }
    match instruction {
      Instruction::ADD(target) => {
        match target {
          ArithmeticTarget::C => {
            let value = self.registers.c;
            let new_value = self.add(value);
            self.registers.a = new_value;
            self.pc.wrapping_add(1)
          }
          _ => {/*todo: support more targets*/ self.pc}
        }      
      }
      Instruction::JP(test) => {
        let jump_condition = match test {
          JumpTest::NotZero => !self.registers.f.zero,
          JumpTest::NotCarry => !self.registers.f.carry,
          JumpTest::Zero => self.registers.f.zero,
          JumpTest::Carry => self.registers.f.carry,
          JumpTest::Always => true
        };
        self.jump(jump_condition)
      }
      Instruction::LD(load_type) => {
        match load_type {
          LoadType::Byte(target, source) => {
            let source_value = match source {
              LoadByteSource::A => self.registers.a,
              LoadByteSource::D8 => CPU::read_next_byte(),
              LoadByteSource::HLI => self.bus.read_byte(self.registers.get_hl()),
              _ => { panic!("todo: implement other sources") }
            };
            match target {
              LoadByteTarget::A => self.registers.a = source_value,
              LoadByteTarget::HLI => self.bus.write_byte(self.registers.get_hl(), source_value),
              _ => { panic!("todo: implement other targets") }
            };
            match source {
              LoadByteSource::D8 => self.pc.wrapping_add(2),
              _                  => self.pc.wrapping_add(1),
            }
          }
          //some other load types: Word, AFromIndirect, IndirectFromA, AFromByteAddress, ByteAddressFromA
          _ => { panic!("todo: implement other load types") }
        }
      }
      Instruction::PUSH(target) => {
        let value = match target {
          StackTarget::BC => self.registers.get_bc(),
          _ => { panic!("todo: support more targets") }
        };
        self.push(value);
        self.pc.wrapping_add(1)
      }
      Instruction::POP(target) => {
        let result = self.pop();
        match target {
          StackTarget::BC => self.registers.set_bc(result),
          _ => { panic!("todo: support more targets") }
        };
        self.pc.wrapping_add(1)
      }
      Instruction::CALL(test) => {
        let jump_condition = match test {
          JumpTest::NotZero => !self.registers.f.zero,
          _ => { panic!("todo: support more conditions") }
        };
        self.call(jump_condition)
      }
      Instruction::RET(test) => {
        let jump_condition = match test {
          JumpTest::NotZero => !self.registers.f.zero,
          _ => { panic!("todo: support more conditions") }
        };
        self.return_(jump_condition)
      }
      Instruction::NOP() => {
        self.pc.wrapping_add(1)
      }
      Instruction::HALT() => {
        self.is_halted = true;
        self.pc.wrapping_add(1)
      }
      _ => { panic!("todo: support more instructions") }
    }
  }

  fn add(&mut self, value: u8) -> u8 {
    let (new_value, did_overflow) = self.registers.a.overflowing_add(value);
    self.registers.f.zero = new_value == 0;
    self.registers.f.subtract = false;
    self.registers.f.carry = did_overflow;
    //Half Carry is set if adding the lower nibbles of the value and register A
    //together result in a value bigger than 0xF. If result is larger than 0xF
    //then the addition caused a carry from the lower nibble to the upper nibble.
    self.registers.f.half_carry = (self.registers.a & 0xF) + (value & 0xF) > 0xF;
    new_value
  }

  fn jump(&self, should_jump: bool) -> u16 {
    if should_jump {
      //Gameboy is little endian so read pc + 2 as most significant bit
      //and pc + 1 as least significant bit
      let least_significant_byte = self.bus.read_byte(self.pc + 1) as u16;
      let most_significant_byte = self.bus.read_byte(self.pc + 2) as u16;
      (most_significant_byte << 8) | least_significant_byte
    } else {
      //if we dont jump we need to still move the program
      //counter forward by 3 since the jump instruction is
      //3 bytes wide (1 byte for tag and 2 bytes for jump address)
      self.pc.wrapping_add(3)
    }
  }

  fn push(&mut self, value: u16) {
    self.sp = self.sp.wrapping_sub(1);
    self.bus.write_byte(self.sp, ((value & 0xFF00) >> 8) as u8);

    self.sp = self.sp.wrapping_sub(1);
    self.bus.write_byte(self.sp, (value & 0xFF) as u8);
  }

  fn pop(&mut self) -> u16 {
    let lsb = self.bus.read_byte(self.sp) as u16;
    self.sp = self.sp.wrapping_add(1);

    let msb = self.bus.read_byte(self.sp) as u16;
    self.sp = self.sp.wrapping_add(1);

    (msb << 8) | lsb
  }

  fn call(&mut self, should_jump: bool) -> u16 {
    let next_pc = self.pc.wrapping_add(3);
    if should_jump {
      self.push(next_pc);
      CPU::read_next_word()
    } else {
      next_pc
    }
  }

  fn return_(&mut self, should_jump: bool) -> u16 {
    if should_jump {
      self.pop()
    } else {
      self.pc.wrapping_add(1)
    }
  }

  /*additional instructions to implement:
    ADDHL, ADC, SUB, SBC, AND, OR, XOR, CP, INC, DEC, CCF, SCF, RRA, RLA, RRCA, 
    RRLA, CPL, BIT, RESET, SET, SRL, RR, RL, RRC, RLC, SRA, SLA, SWAP
  */
}

struct CPU {
  registers: Registers,
  pc: u16,
  sp: u16,
  bus: MemoryBus,
  is_halted: bool,
}

struct MemoryBus {
  memory: [u8; 0xFFFF],
  gpu: GPU,
  oam_blocked: bool,
  eram_blocked: bool,
}

impl MemoryBus {
  fn read_byte(&self, address: u16) -> u8 {
    let address = address as usize;
    match address {
      ROM_BANK_00_BEGIN ..= ROM_BANK_00_END => {self.memory[address]}
      ROM_BANK_01_BEGIN ..= ROM_BANK_01_END => {self.memory[address]}
      VRAM_BEGIN ..= VRAM_END => {
        self.gpu.read_vram(address - VRAM_BEGIN)
      }
      ERAM_BEGIN ..= ERAM_END => {if self.eram_blocked {self.memory[address]} else {self.memory[address]}} //a read during eram block being true is undefined, so implemented as eram being marked read only
      WRAM_BEGIN ..= WRAM_END => {self.memory[address]}
      WRAM_01_BEGIN ..= WRAM_01_END => {self.memory[address]}
      ECHORAM_BEGIN ..= ECHORAM_END => {self.memory[address - 0x2000]} //mirror of wram (address - 0x2000)
      OAM_BEGIN ..= OAM_END => {self.memory[address]}
      BLOCKED_RAM_BEGIN ..= BLOCKED_RAM_END => {if self.oam_blocked {0xFF} else {0x00}} //behavior depends on hardware revision (DMG behavior implemented here, minus corruption)
      IO_REGISTERS_BEGIN ..= IO_REGISTERS_END => {self.memory[address]} //replace later like with vram, not all i/o is both readable and writeable
      HRAM_BEGIN ..= HRAM_END => {self.memory[address]}
      INTERRUPT_REGISTER => {self.memory[address]}
      _ => panic!("Attempt to read outside defined memory bus range")
    }
  }

  fn write_byte(&mut self, address: u16, value: u8) {
    let address = address as usize;
    match address {
      ROM_BANK_00_BEGIN ..= ROM_BANK_00_END => {} //replace later like with vram, writes trigger many different behaviors
      ROM_BANK_01_BEGIN ..= ROM_BANK_01_END => {} //depending on what register is attempted to be written to and what MBC is present in the game cartrige
      VRAM_BEGIN ..= VRAM_END => {
        self.gpu.write_vram(address - VRAM_BEGIN, value)
      }
      ERAM_BEGIN ..= ERAM_END => {if self.eram_blocked {/*blocked*/} else {self.memory[address] = value}}
      WRAM_BEGIN ..= WRAM_END => {self.memory[address] = value}
      WRAM_01_BEGIN ..= WRAM_01_END => {self.memory[address] = value}
      ECHORAM_BEGIN ..= ECHORAM_END => {self.memory[address - 0x2000] = value} //mirror of wram (address - 0x2000)
      OAM_BEGIN ..= OAM_END => {self.memory[address] = value} //replace later like with vram, direct writes only work during HBlank and VBlank periods (PPU stuff)
      BLOCKED_RAM_BEGIN ..= BLOCKED_RAM_END => {/*blocked*/}
      IO_REGISTERS_BEGIN ..= IO_REGISTERS_END => {self.memory[address] = value} //replace later like with vram, not all i/o is both readable and writeable
      HRAM_BEGIN ..= HRAM_END => {self.memory[address] = value}
      INTERRUPT_REGISTER => {self.memory[address] = value}
      _ => panic!("Attempt to write outside defined memory bus range")
    }
  }
}

const ROM_BANK_00_BEGIN: usize = 0x0000;
const ROM_BANK_00_END: usize = 0x3FFF;
const ROM_BANK_01_BEGIN: usize = 0x4000;
const ROM_BANK_01_END: usize = 0x7FFF;
const VRAM_BEGIN: usize = 0x8000;
const VRAM_END: usize = 0x9FFF;
const VRAM_SIZE: usize = VRAM_END - VRAM_BEGIN + 1;
const ERAM_BEGIN: usize = 0xA000; //external ram, battery buffered (for save files and the like)
const ERAM_END: usize = 0xBFFF;
const WRAM_BEGIN: usize = 0xC000;
const WRAM_END: usize = 0xCFFF;
const WRAM_01_BEGIN: usize = 0xD000;
const WRAM_01_END: usize = 0xDFFF;
const ECHORAM_BEGIN: usize = 0xE000;
const ECHORAM_END: usize = 0xFDFF;
const OAM_BEGIN: usize = 0xFE00;
const OAM_END: usize = 0xFE9F;
const BLOCKED_RAM_BEGIN: usize = 0xFEA0;
const BLOCKED_RAM_END: usize = 0xFEFF;
const IO_REGISTERS_BEGIN: usize = 0xFF00;
const IO_REGISTERS_END: usize = 0xFF7F;
const HRAM_BEGIN: usize = 0xFF80;
const HRAM_END: usize = 0xFFFE;
const INTERRUPT_REGISTER: usize = 0xFFFF;

#[derive(Copy,Clone)]
enum TilePixelValue {
    Zero,
    One,
    Two,
    Three,
}

type Tile = [[TilePixelValue; 8]; 8];
fn default_tile() -> Tile {
  [[TilePixelValue::Zero; 8]; 8]
}
fn empty_tile() -> Tile {
    [[TilePixelValue::Zero; 8]; 8]
}

struct GPU{
    vram: [u8; VRAM_SIZE],
    tile_set: [Tile; 384],
}

impl GPU {
  fn read_vram(&self, address: usize) -> u8 {
    self.vram[address]
  }

  fn write_vram(&mut self, index: usize, value: u8) {
    self.vram[index] = value;
    // If our index is greater than 0x1800, we're not writing to the tile set storage
    // so we can just return.
    if index >= 0x1800 { return }

    // Tiles rows are encoded in two bytes with the first byte always
    // on an even address. Bitwise ANDing the address with 0xffe
    // gives us the address of the first byte.
    // For example: `12 & 0xFFFE == 12` and `13 & 0xFFFE == 12`
    let normalized_index = index & 0xFFFE;

    // First we need to get the two bytes that encode the tile row.
    let byte1 = self.vram[normalized_index];
    let byte2 = self.vram[normalized_index + 1];

    // A tiles is 8 rows tall. Since each row is encoded with two bytes a tile
    // is therefore 16 bytes in total.
    let tile_index = index / 16;
    // Every two bytes is a new row
    let row_index = (index % 16) / 2;

    // Now we're going to loop 8 times to get the 8 pixels that make up a given row.
    for pixel_index in 0..8 {
      // To determine a pixel's value we must first find the corresponding bit that encodes
      // that pixels value:
      // 1111_1111
      // 0123 4567
      //
      // As you can see the bit that corresponds to the nth pixel is the bit in the nth
      // position *from the left*. Bits are normally indexed from the right.
      //
      // To find the first pixel (a.k.a pixel 0) we find the left most bit (a.k.a bit 7). For
      // the second pixel (a.k.a pixel 1) we first the second most left bit (a.k.a bit 6) and
      // so on.
      //
      // We then create a mask with a 1 at that position and 0s everywhere else.
      //
      // Bitwise ANDing this mask with our bytes will leave that particular bit with its
      // original value and every other bit with a 0.
      let mask = 1 << (7 - pixel_index);
      let lsb = byte1 & mask;
      let msb = byte2 & mask;

      // If the masked values are not 0 the masked bit must be 1. If they are 0, the masked
      // bit must be 0.
      //
      // Finally we can tell which of the four tile values the pixel is. For example, if the least
      // significant byte's bit is 1 and the most significant byte's bit is also 1, then we
      // have tile value `Three`.
      let value = match (lsb != 0, msb != 0) {
          (true, true) => TilePixelValue::Three,
          (false, true) => TilePixelValue::Two,
          (true, false) => TilePixelValue::One,
          (false, false) => TilePixelValue::Zero,
      };

      self.tile_set[tile_index][row_index][pixel_index] = value;
    }

}
}

use std::env;
use std::fs;

fn main() {
  let args: Vec<String> = env::args().collect();
  let rom_file = &args[1];

  println!("Hello, world!");
  println!("loading rom: {}", rom_file);
  let cartrige = fs::read(rom_file).expect("should have been able to read file");
  //println!("{:?}", cartrige);

  let skipBoot: bool = true;
  let mut cpu =  if skipBoot { 
    CPU {
      registers: Registers {
        a: 0x01,
        b: 0x00,
        c: 0x13,
        d: 0x00,
        e: 0xD8,
        f: FlagsRegister {
          zero: true,
          subtract: false, 
          half_carry: true,
          carry: true,
        },
        h: 0x01,
        l: 0x4D,},
      pc: 0x0100,
      sp: 0xFFFE,
      bus: MemoryBus {
        memory: [0; 0xFFFF],
        gpu: GPU {
          vram: [0; VRAM_SIZE],
          tile_set: [default_tile(); 384],
        },
        eram_blocked: true,
        oam_blocked: false,
      },
      is_halted: false,
    } } else {
    CPU {
      registers: Registers {
        a: 0,
        b: 0,
        c: 0,
        d: 0,
        e: 0,
        f: FlagsRegister {
          zero: false,
          subtract: false, 
          half_carry: false,
          carry: false,
        },
        h: 0,
        l: 0,},
      pc: 0,
      sp: 0,
      bus: MemoryBus {
        memory: [0; 0xFFFF],
        gpu: GPU {
          vram: [0; VRAM_SIZE],
          tile_set: [default_tile(); 384],
        },
        eram_blocked: true,
        oam_blocked: false,
      },
      is_halted: false,
    } };

  let mut x = 0;
  while x < 0x8000 {
    cpu.bus.memory[x] = cartrige[x];
    x += 1;
  }
  //println!("{:?}", cpu.bus.memory);
  let mut i = 0;
  while i < 500 {
    cpu.step();
    i = i + 1;
  }
}
