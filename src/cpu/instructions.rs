
pub enum ArithmeticTarget {
  A, B, C, D, E, H, L,
}

pub enum PrefixTarget {
  B,
}

pub enum StackTarget {
  BC, DE, HL
}

pub enum IncDecTarget {
  BC, DE, HL
}

pub enum LoadByteTarget {
  A, B, C, D, E, H, L, HLI
}

pub enum LoadByteSource {
  A, B, C, D, E, H, L, D8, HLI
}

pub enum LoadType {
  Byte(LoadByteTarget, LoadByteSource),
}

pub enum JumpTest {
  NotZero,
  Zero,
  NotCarry,
  Carry,
  Always
}

//CPU Register Instructions
pub enum Instruction {
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
  pub fn from_byte(byte: u8, prefixed:bool) -> Option<Instruction> {
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