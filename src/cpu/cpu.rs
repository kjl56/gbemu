
use std::fs;
use std::io::Write;

use crate::cpu::registers;
use crate::cpu::instructions::{Instruction, ArithmeticTarget, ArithmeticSource, RotateTarget, StackTarget, IncDecTarget, LoadType, LoadTarget, LoadSource, JumpTest, JumpTarget};
use crate::memory::membus;

pub struct CPU {
  registers: registers::Registers,
  pc: u16,
  sp: u16,
  ime: bool,
  pub bus: membus::MemoryBus,
  is_halted: bool,
  logfile: fs::File,
}

impl CPU {
  pub fn new() -> CPU {
    CPU {
      registers: registers::Registers {
        a: 0x01,
        b: 0x00,
        c: 0x13,
        d: 0x00,
        e: 0xD8,
        f: registers::FlagsRegister {
          zero: true,
          subtract: false, 
          half_carry: true,
          carry: true,
        },
        h: 0x01,
        l: 0x4D,},
      pc: 0x0100,
      sp: 0xFFFE,
      ime: false,
      bus: membus::MemoryBus::new(),
      is_halted: false,
      logfile: fs::File::options().write(true).create(true).open("doctorlog.txt").unwrap(),
    }
  }

  pub fn step(&mut self) {
    if self.pc != 0x0 {
      let cpustate = format!(
          "A:{:02X?} F:{:02X?} B:{:02X?} C:{:02X?} D:{:02X?} E:{:02X?} H:{:02X?} L:{:02X?} SP:{:04X?} PC:{:04X?} PCMEM:{:02X?},{:02X?},{:02X?},{:02X?}",
          self.registers.a,
          u8::from(&self.registers.f),
          self.registers.b,
          self.registers.c,
          self.registers.d,
          self.registers.e,
          self.registers.h,
          self.registers.l,
          self.sp,
          self.pc,
          self.bus.read_byte(self.pc),
          self.bus.read_byte(self.pc + 1),
          self.bus.read_byte(self.pc + 2),
          self.bus.read_byte(self.pc + 3)
      );
      writeln!(self.logfile, "{}", cpustate);
    }
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

  fn read_next_byte(&self) -> u8 {
    self.bus.read_byte(self.pc + 1) as u8
  }

  fn read_next_word(&self) -> u16 {
    //Gameboy is little endian so read pc + 2 as most significant bit
    //and pc + 1 as least significant bit
    let least_significant_byte = self.bus.read_byte(self.pc + 1) as u16;
    let most_significant_byte = self.bus.read_byte(self.pc + 2) as u16;
    (most_significant_byte << 8) | least_significant_byte
  }

  fn execute(&mut self, instruction: Instruction) -> u16{
    if self.is_halted {
      return 0x0
    }
    println!("{:?}", instruction);
    match instruction {
      Instruction::ADD(target, source) => {
        match target {
          ArithmeticTarget::A => {
            match source {
              ArithmeticSource::A => {self.registers.a = self.add(self.registers.a); self.pc.wrapping_add(1)},
              ArithmeticSource::B => {self.registers.a = self.add(self.registers.b); self.pc.wrapping_add(1)},
              ArithmeticSource::C => {self.registers.a = self.add(self.registers.c); self.pc.wrapping_add(1)},
              ArithmeticSource::D => {self.registers.a = self.add(self.registers.d); self.pc.wrapping_add(1)},
              ArithmeticSource::E => {self.registers.a = self.add(self.registers.e); self.pc.wrapping_add(1)},
              ArithmeticSource::H => {self.registers.a = self.add(self.registers.h); self.pc.wrapping_add(1)},
              ArithmeticSource::L => {self.registers.a = self.add(self.registers.l); self.pc.wrapping_add(1)},
              ArithmeticSource::HL => {self.registers.a = self.add(self.bus.read_byte(self.registers.get_hl())); self.pc.wrapping_add(1)},
              ArithmeticSource::D8 => {self.registers.a = self.add(self.read_next_byte()); self.pc.wrapping_add(2)},
              _ => panic!("not a valid ADD instruction")
            }
          }
          ArithmeticTarget::HL => {
            let value = match source {
              ArithmeticSource::BC => self.add16(self.registers.get_bc()),
              ArithmeticSource::DE => self.add16(self.registers.get_de()),
              ArithmeticSource::HL => self.add16(self.registers.get_hl()),
              ArithmeticSource::SP => self.add16(self.sp),
              _ => panic!("not a valid ADD instruction")
            };
            self.registers.set_hl(value);
            self.pc.wrapping_add(1)
          }
          ArithmeticTarget::SP => {
            let value = self.read_next_byte() as i8 as i16;
            let (new_value, did_overflow) = self.sp.overflowing_add_signed(value);
            self.registers.f.zero = false;
            self.registers.f.subtract = false;
            self.registers.f.carry = did_overflow;
            self.registers.f.half_carry = (self.sp & 0xF) + ((value as u16) & 0xF) > 0xF;
            self.sp = new_value;
            self.pc.wrapping_add(2)
          }
        }      
      }
      Instruction::ADC(target, source) => {
        match source {
          ArithmeticSource::A => {self.registers.a = self.add(self.registers.a + (if self.registers.f.carry  {1} else {0})); self.pc.wrapping_add(1)},
          ArithmeticSource::B => {self.registers.a = self.add(self.registers.b + (if self.registers.f.carry  {1} else {0})); self.pc.wrapping_add(1)},
          ArithmeticSource::C => {self.registers.a = self.add(self.registers.c + (if self.registers.f.carry  {1} else {0})); self.pc.wrapping_add(1)},
          ArithmeticSource::D => {self.registers.a = self.add(self.registers.d + (if self.registers.f.carry  {1} else {0})); self.pc.wrapping_add(1)},
          ArithmeticSource::E => {self.registers.a = self.add(self.registers.e + (if self.registers.f.carry  {1} else {0})); self.pc.wrapping_add(1)},
          ArithmeticSource::H => {self.registers.a = self.add(self.registers.h + (if self.registers.f.carry  {1} else {0})); self.pc.wrapping_add(1)},
          ArithmeticSource::L => {self.registers.a = self.add(self.registers.l + (if self.registers.f.carry  {1} else {0})); self.pc.wrapping_add(1)},
          ArithmeticSource::HL => {self.registers.a = self.add(self.bus.read_byte(self.registers.get_hl()) + (if self.registers.f.carry  {1} else {0})); self.pc.wrapping_add(1)},
          ArithmeticSource::D8 => {self.registers.a = self.add(self.read_next_byte() + (if self.registers.f.carry  {1} else {0})); self.pc.wrapping_add(2)},
          _ => panic!("not a valid ADD instruction")
        }
      }
      Instruction::SUB(target, source) => {
        match source {
          ArithmeticSource::A => {self.registers.a = self.subtract(self.registers.a); self.pc.wrapping_add(1)},
          ArithmeticSource::B => {self.registers.a = self.subtract(self.registers.b); self.pc.wrapping_add(1)},
          ArithmeticSource::C => {self.registers.a = self.subtract(self.registers.c); self.pc.wrapping_add(1)},
          ArithmeticSource::D => {self.registers.a = self.subtract(self.registers.d); self.pc.wrapping_add(1)},
          ArithmeticSource::E => {self.registers.a = self.subtract(self.registers.e); self.pc.wrapping_add(1)},
          ArithmeticSource::H => {self.registers.a = self.subtract(self.registers.h); self.pc.wrapping_add(1)},
          ArithmeticSource::L => {self.registers.a = self.subtract(self.registers.l); self.pc.wrapping_add(1)},
          ArithmeticSource::HL => {self.registers.a = self.subtract(self.bus.read_byte(self.registers.get_hl())); self.pc.wrapping_add(1)},
          ArithmeticSource::D8 => {self.registers.a = self.subtract(self.read_next_byte()); self.pc.wrapping_add(2)},
          _ => panic!("not a valid SUB instruction")
        }
      }
      Instruction::AND(target, source) => {
        match source {
          ArithmeticSource::HL => {
            let value = self.bus.read_byte(self.registers.get_hl());
            self.registers.a = self.registers.a & value;
            if self.registers.a == 0 { self.registers.f = registers::FlagsRegister::from(0b10100000) } else { self.registers.f = registers::FlagsRegister::from(0b00100000) }
            self.pc.wrapping_add(1)
          }
          ArithmeticSource::D8 => {
            let value = self.read_next_byte();
            self.registers.a = self.registers.a & value;
            if self.registers.a == 0 { self.registers.f = registers::FlagsRegister::from(0b10100000) } else { self.registers.f = registers::FlagsRegister::from(0b00100000) }
            self.pc.wrapping_add(2)
          }
          _ => {
            let value = match source {
              ArithmeticSource::A => self.registers.a,
              ArithmeticSource::B => self.registers.b,
              ArithmeticSource::C => self.registers.c,
              ArithmeticSource::D => self.registers.d,
              ArithmeticSource::E => self.registers.e,
              ArithmeticSource::H => self.registers.h,
              ArithmeticSource::L => self.registers.l,
              _ => panic!("not a valid AND instruction")
            };
            self.registers.a = self.registers.a & value;
            if self.registers.a == 0 { self.registers.f = registers::FlagsRegister::from(0b10100000) } else { self.registers.f = registers::FlagsRegister::from(0b00100000) }
            self.pc.wrapping_add(1)
          }
        }
      }
      Instruction::XOR(target, source) => {
        match source {
          ArithmeticSource::HL => {
            let value = self.bus.read_byte(self.registers.get_hl());
            self.registers.a = self.registers.a ^ value;
            if self.registers.a == 0 { self.registers.f = registers::FlagsRegister::from(0b10000000) } else { self.registers.f = registers::FlagsRegister::from(0b00000000) }
            self.pc.wrapping_add(1)
          }
          ArithmeticSource::D8 => {
            let value = self.read_next_byte();
            self.registers.a = self.registers.a ^ value;
            if self.registers.a == 0 { self.registers.f = registers::FlagsRegister::from(0b10000000) } else { self.registers.f = registers::FlagsRegister::from(0b00000000) }
            self.pc.wrapping_add(2)
          }
          _ => {
            let value = match source {
              ArithmeticSource::A => self.registers.a,
              ArithmeticSource::B => self.registers.b,
              ArithmeticSource::C => self.registers.c,
              ArithmeticSource::D => self.registers.d,
              ArithmeticSource::E => self.registers.e,
              ArithmeticSource::H => self.registers.h,
              ArithmeticSource::L => self.registers.l,
              _ => panic!("not a valid XOR instruction")
            };
            self.registers.a = self.registers.a ^ value;
            if self.registers.a == 0 { self.registers.f = registers::FlagsRegister::from(0b10000000) } else { self.registers.f = registers::FlagsRegister::from(0b00000000) }
            self.pc.wrapping_add(1)
          }
        }
      }
      Instruction::OR(target, source) => {
        match source {
          ArithmeticSource::HL => {
            let value = self.bus.read_byte(self.registers.get_hl());
            self.registers.a = self.registers.a | value;
            if self.registers.a == 0 { self.registers.f = registers::FlagsRegister::from(0b10000000) } else { self.registers.f = registers::FlagsRegister::from(0b00000000) }
            self.pc.wrapping_add(1)
          }
          ArithmeticSource::D8 => {
            let value = self.read_next_byte();
            self.registers.a = self.registers.a | value;
            if self.registers.a == 0 { self.registers.f = registers::FlagsRegister::from(0b10000000) } else { self.registers.f = registers::FlagsRegister::from(0b00000000) }
            self.pc.wrapping_add(2)
          }
          _ => {
            let value = match source {
              ArithmeticSource::A => self.registers.a,
              ArithmeticSource::B => self.registers.b,
              ArithmeticSource::C => self.registers.c,
              ArithmeticSource::D => self.registers.d,
              ArithmeticSource::E => self.registers.e,
              ArithmeticSource::H => self.registers.h,
              ArithmeticSource::L => self.registers.l,
              _ => panic!("not a valid XOR instruction")
            };
            self.registers.a = self.registers.a | value;
            if self.registers.a == 0 { self.registers.f = registers::FlagsRegister::from(0b10000000) } else { self.registers.f = registers::FlagsRegister::from(0b00000000) }
            self.pc.wrapping_add(1)
          }
        }
      }
      Instruction::CP(target, source) => {
        match source {
          ArithmeticSource::A => {let val = self.subtract(self.registers.a); self.pc.wrapping_add(1)},
          ArithmeticSource::B => {let val = self.subtract(self.registers.b); self.pc.wrapping_add(1)},
          ArithmeticSource::C => {let val = self.subtract(self.registers.c); self.pc.wrapping_add(1)},
          ArithmeticSource::D => {let val = self.subtract(self.registers.d); self.pc.wrapping_add(1)},
          ArithmeticSource::E => {let val = self.subtract(self.registers.e); self.pc.wrapping_add(1)},
          ArithmeticSource::H => {let val = self.subtract(self.registers.h); self.pc.wrapping_add(1)},
          ArithmeticSource::L => {let val = self.subtract(self.registers.l); self.pc.wrapping_add(1)},
          ArithmeticSource::HL => {let val = self.subtract(self.bus.read_byte(self.registers.get_hl())); self.pc.wrapping_add(1)},
          ArithmeticSource::D8 => {let val = self.subtract(self.read_next_byte()); self.pc.wrapping_add(2)},
          _ => panic!("not a valid CP instruction")
        }
      }
      Instruction::JP(test, target) => {
        let should_jump = match test {
          JumpTest::NotZero => !self.registers.f.zero,
          JumpTest::NotCarry => !self.registers.f.carry,
          JumpTest::Zero => self.registers.f.zero,
          JumpTest::Carry => self.registers.f.carry,
          JumpTest::Always => true,
        };
        if should_jump {
          match target {
            JumpTarget::HL => self.registers.get_hl(),
            JumpTarget::D16 => self.read_next_word(),
            _ => panic!("invalid target for JP operation")
          }
        } else {
          self.pc.wrapping_add(3)
        }
      }
      Instruction::JR(test, target) => {
        let should_jump = match test {
          JumpTest::Always => true,
          JumpTest::NotZero => !self.registers.f.zero,
          JumpTest::Zero => self.registers.f.zero,
          JumpTest::NotCarry => !self.registers.f.carry,
          JumpTest::Carry => self.registers.f.carry,
        };
        let mut next_pc = self.pc.wrapping_add(2);
        if should_jump {
          next_pc = next_pc.wrapping_add_signed((self.read_next_byte() as i8).into());
        }
        next_pc
      }
      Instruction::LD(load_type) => {
        match load_type {
          LoadType::Byte(target, source) => {
            let source_value = match source {
              LoadSource::A => self.registers.a,
              LoadSource::B => self.registers.b,
              LoadSource::C => self.registers.c,
              LoadSource::D => self.registers.d,
              LoadSource::E => self.registers.e,
              LoadSource::H => self.registers.h,
              LoadSource::L => self.registers.l,
              LoadSource::D8 => self.read_next_byte(),
              _ => { panic!("incorrect opcode mapping for LD") }
            };
            match target {
              LoadTarget::A => self.registers.a = source_value,
              LoadTarget::B => self.registers.b = source_value,
              LoadTarget::C => self.registers.c = source_value,
              LoadTarget::D => self.registers.d = source_value,
              LoadTarget::E => self.registers.e = source_value,
              LoadTarget::H => self.registers.h = source_value,
              LoadTarget::L => self.registers.l = source_value,
              LoadTarget::BC => self.bus.write_byte(self.registers.get_bc(), source_value),
              LoadTarget::DE => self.bus.write_byte(self.registers.get_de(), source_value),
              LoadTarget::HL => self.bus.write_byte(self.registers.get_hl(), source_value),
              _ => { panic!("incorrect opcode mapping for LD") }
            };
            match source {
              LoadSource::D8 => self.pc.wrapping_add(2),
              _                  => self.pc.wrapping_add(1),
            }
          }
          LoadType::Word(target, source) => {
            let source_value = match source {
              LoadSource::D16 => self.read_next_word(),
              LoadSource::SP => self.sp,
              LoadSource::SP8 => self.sp.wrapping_add_signed((self.read_next_byte() as i8).into()),
              LoadSource::HL => self.registers.get_hl(),
              _ => { panic!("incorrect opcode mapping for LD") }
            };
            match target {
              LoadTarget::A => self.registers.a = self.bus.read_byte(source_value),
              LoadTarget::BC => self.registers.set_bc(source_value),
              LoadTarget::DE => self.registers.set_de(source_value),
              LoadTarget::HL => self.registers.set_hl(source_value),
              LoadTarget::D16 => {let addressd16 = self.read_next_word(); self.bus.write_byte(addressd16, (source_value & 0xFF) as u8); self.bus.write_byte(addressd16+1, ((source_value & 0xFF00) >> 8) as u8)},
              LoadTarget::SP => self.sp = source_value,
              _ => { panic!("incorrect opcode mapping for LD") }
            };
            match source {
              LoadSource::D16 => self.pc.wrapping_add(3),
              LoadSource::SP => self.pc.wrapping_add(3),
              LoadSource::SP8 => self.pc.wrapping_add(2),
              _                  => self.pc.wrapping_add(1),
            }
          }
          LoadType::AToAddress(target, source) => {
            let source_value = match source {
              LoadSource::A => self.registers.a,
              LoadSource::B => self.registers.b,
              LoadSource::C => self.registers.c,
              LoadSource::D => self.registers.d,
              LoadSource::E => self.registers.e,
              LoadSource::H => self.registers.h,
              LoadSource::L => self.registers.l,
              LoadSource::D8 => self.read_next_byte(),
              _ => { panic!("incorrect opcode mapping for LD") }
            };
            match target {
              LoadTarget::BC => self.bus.write_byte(self.registers.get_bc(), source_value),
              LoadTarget::DE => self.bus.write_byte(self.registers.get_de(), source_value),
              LoadTarget::HL => self.bus.write_byte(self.registers.get_hl(), source_value),
              LoadTarget::D16 => self.bus.write_byte(self.read_next_word(), source_value),
              LoadTarget::HLI => {self.bus.write_byte(self.registers.get_hl(), source_value); self.registers.set_hl(self.registers.get_hl() + 1)},
              LoadTarget::HLD => {self.bus.write_byte(self.registers.get_hl(), source_value); self.registers.set_hl(self.registers.get_hl() - 1)},
              LoadTarget::AddrPC => self.bus.write_byte(0xFF00 + (self.registers.c as u16), source_value),
              _ => { panic!("incorrect opcode mapping for LD") }
            };
            match target {
              LoadTarget::D16 => self.pc.wrapping_add(3),
              _ =>  match source {
                      LoadSource::D8 => self.pc.wrapping_add(2),
                      _                  => self.pc.wrapping_add(1),
                    },
            }
          }
          LoadType::AddressToA(target, source) => {
            let source_value = match source {
              LoadSource::D16 => self.read_next_word(),
              LoadSource::BC => self.registers.get_bc(),
              LoadSource::DE => self.registers.get_de(),
              LoadSource::HL => self.registers.get_hl(),
              LoadSource::HLI => {let val = self.registers.get_hl(); self.registers.set_hl(val + 1); val},
              LoadSource::HLD => {let val = self.registers.get_hl(); self.registers.set_hl(val - 1); val},
              LoadSource::AddrPC => 0xFF00 + (self.registers.c as u16),
              _ => { panic!("incorrect opcode mapping for LD") }
            };
            match target {
              LoadTarget::A => self.registers.a = self.bus.read_byte(source_value),
              LoadTarget::B => self.registers.b = self.bus.read_byte(source_value),
              LoadTarget::C => self.registers.c = self.bus.read_byte(source_value),
              LoadTarget::D => self.registers.d = self.bus.read_byte(source_value),
              LoadTarget::E => self.registers.e = self.bus.read_byte(source_value),
              LoadTarget::H => self.registers.h = self.bus.read_byte(source_value),
              LoadTarget::L => self.registers.l = self.bus.read_byte(source_value),
              _ => { panic!("incorrect opcode mapping for LD") }
            };
            match source {
              LoadSource::D16 => self.pc.wrapping_add(3),
              _                  => self.pc.wrapping_add(1),
            }
          }
          //some other load types: Word, AFromIndirect, IndirectFromA, AFromByteAddress, ByteAddressFromA
          //_ => { panic!("todo: implement other load types") }
        }
      }
      Instruction::LDH(load_type) => {
        match load_type {
          LoadType::Byte(target, source) => {
            match target {
              LoadTarget::D8 => {self.bus.write_byte(0xFF00 + (self.read_next_byte() as u16), self.registers.a); self.pc.wrapping_add(2)},
              LoadTarget::A => {self.registers.a = self.bus.read_byte(0xFF00 + (self.read_next_byte() as u16)); self.pc.wrapping_add(2)},
              _ => { panic!("incorrect opcode mapping for LDH") }
            }
          }
          _ => { panic!("incorrect opcode mapping for LDH") }
        }
      }
      Instruction::PUSH(target) => {
        let value = match target {
          StackTarget::BC => self.registers.get_bc(),
          StackTarget::DE => self.registers.get_de(),
          StackTarget::HL => self.registers.get_hl(),
          StackTarget::AF => self.registers.get_af(),
        };
        self.push(value);
        self.pc.wrapping_add(1)
      }
      Instruction::POP(target) => {
        let result = self.pop();
        match target {
          StackTarget::BC => self.registers.set_bc(result),
          StackTarget::DE => self.registers.set_de(result),
          StackTarget::HL => self.registers.set_hl(result),
          StackTarget::AF => self.registers.set_af(result),
        };
        self.pc.wrapping_add(1)
      }
      Instruction::INC(target) => {
        fn increment_register(register: &mut u8, flags: &mut registers::FlagsRegister) {
          let before = *register;  // Store the value before the increment
          *register = register.wrapping_add(1);
          flags.zero = *register == 0;
          flags.subtract = false;
          flags.half_carry = (before & 0xF) + (1 & 0xF) > 0xF;
        }
        match target {
          IncDecTarget::A => increment_register(&mut self.registers.a, &mut self.registers.f),
          IncDecTarget::B => increment_register(&mut self.registers.b, &mut self.registers.f),
          IncDecTarget::C => increment_register(&mut self.registers.c, &mut self.registers.f),
          IncDecTarget::D => increment_register(&mut self.registers.d, &mut self.registers.f),
          IncDecTarget::E => increment_register(&mut self.registers.e, &mut self.registers.f),
          IncDecTarget::H => increment_register(&mut self.registers.h, &mut self.registers.f),
          IncDecTarget::L => increment_register(&mut self.registers.l, &mut self.registers.f),
          IncDecTarget::BC => self.registers.set_bc(self.registers.get_bc()+1),
          IncDecTarget::DE => self.registers.set_de(self.registers.get_de()+1),
          IncDecTarget::HL => self.registers.set_hl(self.registers.get_hl()+1),
          IncDecTarget::PntrHL => {let mut deref = self.bus.read_byte(self.registers.get_hl()); increment_register(&mut deref, &mut self.registers.f); self.bus.write_byte(self.registers.get_hl(), deref)},
          IncDecTarget::SP => self.sp = self.sp.wrapping_add(1),
        }
        self.pc.wrapping_add(1)
      }
      Instruction::DEC(target) => {
        fn decrement_register(register: &mut u8, flags: &mut registers::FlagsRegister) {
          *register = register.wrapping_sub(1);
          flags.zero = *register == 0;
          flags.subtract = true;
          flags.half_carry = (*register & 0xF) + (1 & 0xF) > 0xF;
        }
        match target {
          IncDecTarget::A => decrement_register(&mut self.registers.a, &mut self.registers.f),
          IncDecTarget::B => decrement_register(&mut self.registers.b, &mut self.registers.f),
          IncDecTarget::C => decrement_register(&mut self.registers.c, &mut self.registers.f),
          IncDecTarget::D => decrement_register(&mut self.registers.d, &mut self.registers.f),
          IncDecTarget::E => decrement_register(&mut self.registers.e, &mut self.registers.f),
          IncDecTarget::H => decrement_register(&mut self.registers.h, &mut self.registers.f),
          IncDecTarget::L => decrement_register(&mut self.registers.l, &mut self.registers.f),
          IncDecTarget::BC => self.registers.set_bc(self.registers.get_bc()-1),
          IncDecTarget::DE => self.registers.set_de(self.registers.get_de()-1),
          IncDecTarget::HL => self.registers.set_hl(self.registers.get_hl()-1),
          IncDecTarget::PntrHL => {let mut deref = self.bus.read_byte(self.registers.get_hl()); decrement_register(&mut deref, &mut self.registers.f); self.bus.write_byte(self.registers.get_hl(), deref)},
          IncDecTarget::SP => self.sp = self.sp.wrapping_sub(1),
        }
        self.pc.wrapping_add(1)
      }
      Instruction::RR(target) => {
        match target {
          RotateTarget::A => CPU::rotate_right(&mut self.registers.a, &mut self.registers.f),
          RotateTarget::B => CPU::rotate_right(&mut self.registers.b, &mut self.registers.f),
          RotateTarget::C => CPU::rotate_right(&mut self.registers.c, &mut self.registers.f),
          RotateTarget::D => CPU::rotate_right(&mut self.registers.d, &mut self.registers.f),
          RotateTarget::E => CPU::rotate_right(&mut self.registers.e, &mut self.registers.f),
          RotateTarget::H => CPU::rotate_right(&mut self.registers.h, &mut self.registers.f),
          RotateTarget::L => CPU::rotate_right(&mut self.registers.l, &mut self.registers.f),
          RotateTarget::HL => {let mut deref = self.bus.read_byte(self.registers.get_hl()); CPU::rotate_right(&mut deref, &mut self.registers.f); self.bus.write_byte(self.registers.get_hl(), deref)},
        }
        self.pc.wrapping_add(2)
      }
      Instruction::RRA() => {
        CPU::rotate_right(&mut self.registers.a, &mut self.registers.f);
        self.registers.f.zero = false;
        self.pc.wrapping_add(1)
      }
      Instruction::SRL(target) => {
        fn register_shift(register: &mut u8, flags: &mut registers::FlagsRegister){
          flags.carry = (0b1 & *register) != 0;
          *register >>= 1;
          flags.zero = *register == 0;
          flags.subtract = false;
          flags.half_carry = false;
        }
        match target {
          RotateTarget::A => register_shift(&mut self.registers.a, &mut self.registers.f),
          RotateTarget::B => register_shift(&mut self.registers.b, &mut self.registers.f),
          RotateTarget::C => register_shift(&mut self.registers.c, &mut self.registers.f),
          RotateTarget::D => register_shift(&mut self.registers.d, &mut self.registers.f),
          RotateTarget::E => register_shift(&mut self.registers.e, &mut self.registers.f),
          RotateTarget::H => register_shift(&mut self.registers.h, &mut self.registers.f),
          RotateTarget::L => register_shift(&mut self.registers.l, &mut self.registers.f),
          RotateTarget::HL => {let mut deref = self.bus.read_byte(self.registers.get_hl()); register_shift(&mut deref, &mut self.registers.f); self.bus.write_byte(self.registers.get_hl(), deref)},
        }
        self.pc.wrapping_add(2)
      }
      Instruction::CALL(test, target) => {
        let jump_condition = match test {
          JumpTest::NotZero => !self.registers.f.zero,
          JumpTest::NotCarry => !self.registers.f.carry,
          JumpTest::Zero => self.registers.f.zero,
          JumpTest::Carry => self.registers.f.carry,
          JumpTest::Always => true,
        };
        self.call(jump_condition)
      }
      Instruction::RET(test) => {
        let jump_condition = match test {
          JumpTest::NotZero => !self.registers.f.zero,
          JumpTest::NotCarry => !self.registers.f.carry,
          JumpTest::Zero => self.registers.f.zero,
          JumpTest::Carry => self.registers.f.carry,
          JumpTest::Always => true,
        };
        self.return_(jump_condition)
      }
      Instruction::RETI() => {
        self.ime = true;
        self.return_(true)
      }
      Instruction::DI() => {
        self.ime = false;
        self.pc.wrapping_add(1)
      }
      Instruction::EI() => {
        self.pc = self.pc.wrapping_add(1);
        self.step();
        self.ime = true;
        self.pc
      }
      Instruction::NOP() => {
        self.pc.wrapping_add(1)
      }
      Instruction::HALT() => {
        self.is_halted = true;
        self.pc.wrapping_add(1)
      }
      _ => { panic!("todo: Instruction: {:?} not yet implemented", instruction) }
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

  fn add16(&mut self, value: u16) -> u16 {
    let (new_value, did_overflow) = self.registers.get_hl().overflowing_add(value);
    self.registers.f.subtract = false;
    self.registers.f.carry = did_overflow;
    self.registers.f.half_carry = (self.registers.get_hl() & 0xFFF) + (value & 0xFFF) > 0xFFF;
    new_value
  }

  fn subtract(&mut self, value: u8) -> u8 {
    let (new_value, did_overflow) = self.registers.a.overflowing_sub(value);
    self.registers.f.zero = new_value == 0;
    self.registers.f.subtract = true;
    self.registers.f.carry = did_overflow;
    self.registers.f.half_carry = (new_value & 0xF) + (value & 0xF) > 0xF;
    new_value
  }

  fn rotate_right(register: &mut u8, flags: &mut registers::FlagsRegister){
    let b7 = flags.carry;
    flags.carry = (0b1 & *register) != 0;
    *register >>= 1;
    if b7 {*register += 0b10000000}
    flags.zero = *register == 0;
    flags.subtract = false;
    flags.half_carry = false;
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
