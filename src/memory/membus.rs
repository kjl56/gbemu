
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

pub struct MemoryBus<'a> {
  pub memory: [u8; 0x10000],
  pub gpu: gpu::GPU<'a>,
  pub oam_blocked: bool,
  pub eram_blocked: bool,
}

impl MemoryBus<'_> {
  pub fn new(sdlcanvas: &mut sdl2::render::WindowCanvas) -> MemoryBus {
    MemoryBus {
      memory: [0; 0x10000],
      gpu: gpu::GPU::new(sdlcanvas),
      eram_blocked: true,
      oam_blocked: false,
    }
  }

  pub fn read_byte(&self, address: u16) -> u8 {
    let address = address as usize;
    //let value = 
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
      OAM_BEGIN ..= OAM_END => {
        //self.memory[address]
        self.gpu.read_oam(address - OAM_BEGIN)
      }
      BLOCKED_RAM_BEGIN ..= BLOCKED_RAM_END => {if self.oam_blocked {0xFF} else {0x00}} //behavior depends on hardware revision (DMG behavior implemented here, minus corruption)
      0xFF40 => {u8::from(&self.gpu.LCDC)}
      //0xFF44 => {0x90} //used for gameboy doctor
      0xFF42 => {self.gpu.SCY}
      0xFF43 => {self.gpu.SCX}
      0xFF44 => {self.gpu.LY}
      0xFF45 => {self.gpu.LYC}
      0xFF4A => {self.gpu.WY}
      0xFF4B => {self.gpu.WX}
      IO_REGISTERS_BEGIN ..= IO_REGISTERS_END => {self.memory[address]} //replace later like with vram, not all i/o is both readable and writeable
      HRAM_BEGIN ..= HRAM_END => {self.memory[address]}
      INTERRUPT_REGISTER => {self.memory[address]}
      _ => panic!("Attempt to read outside defined memory bus range")
    }
    //println!("read byte: {} from address: {}", value, address);
    //return value;
  }

  pub fn write_byte(&mut self, address: u16, value: u8) {
    let address = address as usize;
    //println!("writing byte: {} to address: {}", value, address);
    match address {
      ROM_BANK_00_BEGIN ..= ROM_BANK_00_END => {} //replace later like with vram, writes trigger many different behaviors
      ROM_BANK_01_BEGIN ..= ROM_BANK_01_END => {} //depending on what register is attempted to be written to and what MBC is present in the game cartrige
      VRAM_BEGIN ..= VRAM_END => {
        //println!("vram written to at address: {}", address - VRAM_BEGIN);
        self.gpu.write_vram(address - VRAM_BEGIN, value)
      }
      ERAM_BEGIN ..= ERAM_END => {if self.eram_blocked {/*blocked*/} else {self.memory[address] = value}}
      WRAM_BEGIN ..= WRAM_END => {self.memory[address] = value}
      WRAM_01_BEGIN ..= WRAM_01_END => {self.memory[address] = value}
      ECHORAM_BEGIN ..= ECHORAM_END => {self.memory[address - 0x2000] = value} //mirror of wram (address - 0x2000)
      OAM_BEGIN ..= OAM_END => {//replace later like with vram, direct writes only work during HBlank and VBlank periods (PPU stuff)
        //self.memory[address] = value
        //println!("vram written to at address: {}", address - OAM_BEGIN);
        self.gpu.write_oam(address - OAM_BEGIN, value)
      } 
      BLOCKED_RAM_BEGIN ..= BLOCKED_RAM_END => {/*blocked*/}
      0xFF40 => {self.gpu.LCDC = value.into()}
      0xFF42 => {self.gpu.SCY = value}
      0xFF43 => {self.gpu.SCX = value}
      0xFF44 => {self.gpu.LY = value}
      0xFF45 => {self.gpu.LYC = value}
      0xFF4A => {self.gpu.WY = value}
      0xFF4B => {self.gpu.WX = value}
      IO_REGISTERS_BEGIN ..= IO_REGISTERS_END => {self.memory[address] = value} //replace later like with vram, not all i/o is both readable and writeable
      HRAM_BEGIN ..= HRAM_END => {self.memory[address] = value}
      INTERRUPT_REGISTER => {self.memory[address] = value}
      _ => panic!("Attempt to write outside defined memory bus range")
    }
  }
}

