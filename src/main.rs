
use std::env;
use std::fs;

mod cpu;
mod memory;
mod gpu;
use crate::cpu::cpu::CPU;

extern crate sdl2;

use sdl2::pixels::Color;
use sdl2::rect::Point;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use std::time::Duration;

fn main() {
  let args: Vec<String> = env::args().collect();
  let rom_file = &args[1];

  println!("Hello, world!");
  println!("loading rom: {}", rom_file);
  let cartrige = fs::read(rom_file).expect("should have been able to read file");
  //println!("{:?}", cartrige);

  
  let sdl_context = sdl2::init().unwrap();
  let video_subsystem = sdl_context.video().unwrap();
  
  let window = video_subsystem.window("rust-sdl2 demo", 160*3, 144*3)
  .position_centered()
  .build()
  .unwrap();
  
  let mut canvas = window.into_canvas().build().unwrap();

  let mut cpu =  CPU::new(&mut canvas);

  let mut x = 0;
  while x < 0x8000 {
    cpu.bus.memory[x] = cartrige[x];
    x += 1;
  }

  //canvas.set_draw_color(Color::RGB(0, 255, 255));
  //canvas.clear();
  //canvas.present();
  let mut event_pump = sdl_context.event_pump().unwrap();
  //let mut i = 0;
    'running: loop {
      //i = (i + 1) % 255;
      //canvas.set_draw_color(Color::RGB(i, 64, 255 - i));
      //canvas.clear();
      for event in event_pump.poll_iter() {
          match event {
              Event::Quit {..} |
              Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
                  break 'running
              },
              _ => {}
          }
      }
      // The rest of the game loop goes here...
      for mcycles in 0..17556 {
        cpu.step();
      }
      cpu.bus.gpu.frame();
      //canvas.draw_point(Point::new(400, 300));

      //canvas.present();
      //::std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
    }
    //println!("{:?}", cpu.bus.gpu.vram);
}
