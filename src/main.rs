
use std::env;
use std::fs;

mod cpu;
mod memory;
mod gpu;
use crate::cpu::cpu::CPU;

fn main() {
  let args: Vec<String> = env::args().collect();
  let rom_file = &args[1];

  println!("Hello, world!");
  println!("loading rom: {}", rom_file);
  let cartrige = fs::read(rom_file).expect("should have been able to read file");
  //println!("{:?}", cartrige);

  let mut cpu =  CPU::new();

  let mut x = 0;
  while x < 0x8000 {
    cpu.bus.memory[x] = cartrige[x];
    x += 1;
  }
  //println!("{:?}", cpu.bus.memory);
  let mut i = 0;
  while i < 50000 {
    cpu.step();
    i = i + 1;
  }
}
