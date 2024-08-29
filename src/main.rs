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
              LoadByteSource::D8 => self.read_next_byte(),
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
      self.read_next_word()
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
  memory: [u8; 0xFFFF]
}

impl MemoryBus {
  fn read_byte(&self, address: u16) -> u8 {
    self.memory[address as usize]
  }

  fn write_byte(&self, addr: u16, byte: u8) {

  }
}

fn main() {
  println!("Hello, world!");
}
