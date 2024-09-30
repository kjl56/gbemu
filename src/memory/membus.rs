
use crate::gpu::gpu;

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

pub struct MemoryBus {
  pub memory: [u8; 0xFFFF],
  pub gpu: gpu::GPU,
  pub oam_blocked: bool,
  pub eram_blocked: bool,
}

impl MemoryBus {
  pub fn new() -> MemoryBus {
    MemoryBus {
      memory: [0; 0xFFFF],
      gpu: gpu::GPU::new(),
      eram_blocked: true,
      oam_blocked: false,
    }
  }

  pub fn read_byte(&self, address: u16) -> u8 {
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
      0xFF44 => {0x90} //used for gameboy doctor
      IO_REGISTERS_BEGIN ..= IO_REGISTERS_END => {self.memory[address]} //replace later like with vram, not all i/o is both readable and writeable
      HRAM_BEGIN ..= HRAM_END => {self.memory[address]}
      INTERRUPT_REGISTER => {self.memory[address]}
      _ => panic!("Attempt to read outside defined memory bus range")
    }
  }

  pub fn write_byte(&mut self, address: u16, value: u8) {
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

