
use std::collections::VecDeque;

use sdl2::pixels::Color;
use sdl2::rect::Point;

const VRAM_BEGIN: usize = 0x8000;
const VRAM_END: usize = 0x9FFF;
const VRAM_SIZE: usize = VRAM_END - VRAM_BEGIN + 1;
//0X8000 - 0X97FF is for tile data
//one tile is 16 bytes, leaving space for 384 tiles
//a tile is 8 x 8 pixels, 2 bits per pixel for color (allowing for 4 colors)
//tiles grouped into 3 blocks:
//block 0: 0x8000 - 0x87FF
//block 1: 0x8800 - 0x8FFF
//block 2: 0x9000 - 0x97FF
//tiles indexed using 8-bit integer using one of two methods:
//"0x8000 method": uses 0x8000 as base pointer with unsigned addressing, tiles 0-127 are in block 0, and tiles 128-255 are in block 1
//"0x8800 method": uses 0x9000 as base pointer with signed addressing, tiles 0-127 are in block 2, tiles -128 to -1 (ie. 128-255) are in block 1
//objects always use 0x8000 method, bg and window use 0x8000 method if LCDC.4 == 1, else uses 0x8800 method
//0x9800 - 0x9FFF is for tile maps
//two 32 x 32 tile maps are present
//first map uses 0x9800 - 0x9BFF, second map uses 0x9C00 - 0x9FFF
//each can be used for either background or window

enum PPUMode {
  Zero,
  One,
  Two,
  Three,
}

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

struct FIFOPixel {
  color: u8,
  palette: u8,
  sprite_priority: u8,
  background_priority: bool,
}

pub struct GPU<'a>{
    vram: [u8; VRAM_SIZE],
    tile_set: [Tile; 384],
    canvas: &'a mut sdl2::render::WindowCanvas,
    LCDC: u8,
    mode: PPUMode,
    background_FIFO: VecDeque<FIFOPixel>,
    object_FIFO: VecDeque<FIFOPixel>,
    i: u8,
}

impl GPU<'_> {
  pub fn new(sdlcanvas: &mut sdl2::render::WindowCanvas) -> GPU {
    GPU {
      vram: [0; VRAM_SIZE],
      tile_set: [default_tile(); 384],
      canvas: sdlcanvas,
      LCDC: 0b00000000,
      mode: PPUMode::Two,
      background_FIFO: VecDeque::with_capacity(16),
      object_FIFO: VecDeque::with_capacity(16),
      i: 0,
    }
  }

  //one "dot" = 2^22 Hz (~4.194 MHz), i.e. one tick of the clock (cpu instructs take at least 4 ticks)
  fn oam_scan() {//mode 2, 80 dots
    //oam inaccessible (EXCEPT by DMA (io register 0xFF46))
  }
  fn pixel_fetch(&mut self) {
    //get tile
    let mut tilemap = 0x0000;
    let mut xcoord: u8 = 0;
    let mut ycoord: u8 = 0;
    if ((self.LCDC & 0b00001000) == 1 /*& xcoordNotInWindow*/) {tilemap = 0x9C00;}
    else if ((self.LCDC & 0b01000000) == 1 /*& xcoordInsideWindow*/) {tilemap = 0x9C00;}
    else {tilemap = 0x9800;}
    //if tile is window tile { xcoord = windowtilexcoord; ycoord = windowtileycoord}
    //else { xcoord = ((SCX / 8) + xcoord) & 0x1F; ycoord = (currentscanline + SCY) & 255; } //xcoord will be between 0 and 31, ycoord between 0 and 255
    //use xcoord and ycoord to get tile from vram (if the cpu is blocking vram, tile value read as 0xFF)

    //get tile data low
    //0 = 8800–97FF; 1 = 8000–8FFF
    if self.LCDC & 0b00010000 == 1 {}
    else {}

    //get tile data high


    //sleep

    //push

  }
  fn pixel_draw(&mut self) {//mode 3, 172-289 dots
    //oam inaccessible (EXCEPT by DMA (io register 0xFF46))
    //vram inaccessible
    //CGB palettes inaccessible

    self.background_FIFO.clear();
    self.object_FIFO.clear();
    self.pixel_fetch();
  }
  fn horizontal_blank() {//mode 0, 87-204 dots

  }
  fn vertical_blank() {//mode 1, 456 x 10 dots
    
  }

  pub fn frame(&mut self) {
    
    self.i = (self.i+1) % 255;
    self.canvas.set_draw_color(Color::RGB(0, 0, 0));
    self.canvas.clear();
    self.canvas.set_draw_color(Color::RGB(self.i, 64, 255 - self.i));
    self.canvas.draw_point(Point::new(80, 72));
    self.canvas.present();
  }

  pub fn read_vram(&self, address: usize) -> u8 {
    match self.mode {
      PPUMode::Three => 0xFF,
      _ => self.vram[address],
    }
  }

  pub fn write_vram(&mut self, index: usize, value: u8) {
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