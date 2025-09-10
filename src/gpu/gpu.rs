
use std::collections::VecDeque;

use sdl2::pixels::Color;
use sdl2::rect::Point;
use sdl2::rect::Rect;
use sdl2::render::Canvas;

const VRAM_BEGIN: usize = 0x8000;
const VRAM_END: usize = 0x9FFF;
const VRAM_SIZE: usize = VRAM_END - VRAM_BEGIN + 1;
const OAM_BEGIN: usize = 0xFE00;
const OAM_END: usize = 0xFE9F;
const OAM_SIZE: usize = OAM_END - OAM_BEGIN + 1;

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
//and contains 1-byte indexes of the tiles to be displayed
//tiles fetched using "0x8000" or "0x8800" method based on LCDC.4
//SCX (0xFF42) and SCY (0xFF43) used to scroll background by specifying origin (top left corner) of the display over top the background map
//bottom right coordinates of the viewport is calculated as follows: bottom := (SCY + 143) % 256 and right := (SCX + 159) % 256
//in DMG mode, LCDC.0 can disable the background (and the window)
//a window layer overlays the background and is not scrollable, rendered starting from top left tile
//window position can be moved with registers WX (0xFF4A) and WY (0xFF4B), top left corner is (WX+7, WY)
//window is toggled using LCDC.5 (only works in DMG mode when LCDC.0 == 1)

#[derive(PartialEq)]
enum PPUMode {
  Zero,
  One,
  Two,
  Three,
}

#[derive(Copy,Clone,PartialEq,Debug)]
enum TilePixelValue {
    Zero,
    One,
    Two,
    Three,
    None,
}

type Tile = [[TilePixelValue; 8]; 8];
fn default_tile() -> Tile {
  [[TilePixelValue::Zero; 8]; 8]
}
fn empty_tile() -> Tile {
    [[TilePixelValue::Zero; 8]; 8]
}

//0xFE00 - 0xFE9F is the oam (object attribute memory)
//each object has 4 bytes, allowing 40 total objects in the oam at once
//byte 0: y coordinate from the top (screen is coords 16 - 160, 0-15 partially hides an object above screen, 160 will hide below screen since y-coord is the very top pixel of the object)
//byte 1: x coordinate from the left (screen is coords 8 - 168)
//byte 2: tile index (if LCDC.2 == 0 {(8x8 mode)use byte to select 1 object tile from block 0 or 1} else {(8x16 mode)select 2 object tiles})
//byte 3: attributes/flags: each bit is a flag as follows:
  //bit 7 = Priority: 0 = no, 1 = BG and Window colors 1–3 are drawn over this OBJ
  //bit 6 = Y flip: 0 = normal, 1 = entire object is mirrored
  //bit 5 = X flip: 0 = normal, 1 = entire object is mirrored
  //bit 4 = DMG palette: 0 = OBP0, 1 = OBP1
  //bit 3 = Bank(CGB only): 0 = fetch tile from VRAM bank 0, 1 = fetch from bank 1 	
  //bit 2	1	0 = CGB palette: which of OBP0 - OBP7 to use
//data usually written to OAM using the DMA function (IO register 0xFF46) (can be written to normally during PPU mode 0 and 1)
#[derive(Copy,Clone,Debug)]
struct OAMObject {
  ycoord: u8,
  xcoord: u8,
  tile_index: u8,
  flags: u8,
}

struct FIFOPixel {
  color: TilePixelValue,
  palette: u8,
  sprite_priority: u8,
  background_priority: bool,
}

pub struct LCDC { //LCD Control, io register 0xFF40
  seven: bool, //LCD and PPU enable
  six: bool,   //window tile map area, 0 = 9800-9BFF, 1 = 9C00-9FFF
  five: bool,  //window enable
  four: bool,  //BG & Window tile data area: 0 = 8800–97FF; 1 = 8000–8FFF
  three: bool, //BG tile map area: 0 = 9800–9BFF; 1 = 9C00–9FFF
  two: bool,   //OBJ size: 0 = 8×8; 1 = 8×16
  one: bool,   //OBJ enable
  zero: bool,  //BG & Window enable / priority [Different meaning in CGB Mode]: 0 = Off; 1 = On
}

impl std::convert::From<&LCDC> for u8 {
  fn from(lcdc: &LCDC) -> u8 {
    (if lcdc.seven {1} else {0}) << 7 |
    (if lcdc.six   {1} else {0}) << 6 |
    (if lcdc.five  {1} else {0}) << 5 |
    (if lcdc.four  {1} else {0}) << 4 |
    (if lcdc.three {1} else {0}) << 3 |
    (if lcdc.two   {1} else {0}) << 2 |
    (if lcdc.one   {1} else {0}) << 1 |
    (if lcdc.zero  {1} else {0}) << 0 
  }
} 
impl std::convert::From<u8> for LCDC {
  fn from(byte: u8) -> Self {
    let seven = ((byte >> 7) & 0b1) != 0;
    let six =   ((byte >> 6) & 0b1) != 0;
    let five =  ((byte >> 5) & 0b1) != 0;
    let four =  ((byte >> 4) & 0b1) != 0;
    let three = ((byte >> 3) & 0b1) != 0;
    let two =   ((byte >> 2) & 0b1) != 0;
    let one =   ((byte >> 1) & 0b1) != 0;
    let zero =  ((byte >> 0) & 0b1) != 0;

    LCDC {
      seven,
      six,
      five,
      four,
      three,
      two,
      one,
      zero,
    }
  }
}

struct STAT { //LCD STATus, io register 0xFF41

}

pub struct GPU<'a>{
    pub vram: [u8; VRAM_SIZE],
    oam: [u8; OAM_SIZE],
    tile_set: [Tile; 384],
    oam_set: [OAMObject; 40],
    canvas: &'a mut sdl2::render::WindowCanvas,
    pub LCDC: LCDC, //io register 0xFF40
    pub SCY: u8, //background viewport y position, io register 0xFF42
    pub SCX: u8, //background viewport x position, io register 0xFF43
    pub LY: u8,  //LCD Y coordinate, io register 0xFF44 (read-only for cpu)
    pub LYC: u8, //LY Compare, io register 0xFF45, constantly compared to LY, when equal sets related flag in STAT and (if enabled) triggers a STAT interrupt
    LX : u8,
    pub WY: u8, //window Y coordinate, io register 0xFF4A
    pub WX: u8, //window X coordinate + 7, io register 0xFF4B
    mode: PPUMode,
    scanline_objects: VecDeque<OAMObject>,
    background_FIFO: VecDeque<FIFOPixel>,
    object_FIFO: VecDeque<FIFOPixel>,
    i: u8,
}

impl GPU<'_> {
  pub fn new(sdlcanvas: &mut sdl2::render::WindowCanvas) -> GPU {
    GPU {
      vram: [0; VRAM_SIZE],
      oam: [0; OAM_SIZE],
      tile_set: [default_tile(); 384],
      oam_set: [
        OAMObject {
          ycoord: 0,
          xcoord: 0,
          tile_index: 0,
          flags: 0,
        }; 40],
      canvas: sdlcanvas,
      LCDC: LCDC {seven: false, six: false, five: false, four: false, three: false, two: false, one: false, zero: false,},
      SCY: 0,
      SCX: 0,
      LY: 0,
      LYC: 0,
      LX: 0,
      WY: 0,
      WX: 0,
      mode: PPUMode::Two,
      scanline_objects: VecDeque::with_capacity(10),
      background_FIFO: VecDeque::with_capacity(16),
      object_FIFO: VecDeque::with_capacity(16),
      i: 0,
    }
  }
  
  //during oam scan, the ppu compares LY to each object's Y position to select up to 10 objects to draw on that line
  //the ppu scans the OAM sequentially, selecting the first 10 suitably positioned objects
  //when there is overlap between 2 object's opaque pixels, priority is determined as follows:
    //in DMG: smaller x coordinate has priority, if tied then whichever object comes first in the OAM
    //in CGB: whichever object comes first in the OAM
  //the "BG over OBJ" flag (byte 3-bit 7) is only checked after object priority is determined

  //one "dot" = 2^22 Hz (~4.194 MHz), i.e. one tick of the clock (cpu instructs take at least 4 ticks)
  fn oam_scan(&mut self) {//mode 2, 80 dots
    self.mode = PPUMode::Two;
    //oam inaccessible (EXCEPT by DMA (io register 0xFF46))
    self.scanline_objects.clear();
    for object in self.oam_set {
      if self.scanline_objects.len() < 10 {
        let objectTop = object.ycoord;
        let objectBottom = object.ycoord + (if self.LCDC.two {16} else {8});
        if ((self.LY >= objectTop) & (self.LY < objectBottom)) {
          //println!("on scanline {:?}: {:?}", self.LY, object);
          self.scanline_objects.push_back(object);
        }
      }
    }
  }/*
  fn pixel_fetch(&mut self, xcoord: &mut u8, ycoord: &mut u8) {
    //get tile
    let mut tilemap: usize = 0x0000;
    let xcoordInsideWindow: bool = if (*xcoord < 8) | (*xcoord > 168) {false} else {true};
    if (self.LCDC.three & !xcoordInsideWindow) {tilemap = 0x9C00 - VRAM_BEGIN;}
    else if (self.LCDC.six & xcoordInsideWindow) {tilemap = 0x9C00 - VRAM_BEGIN;}
    else {tilemap = 0x9800 - VRAM_BEGIN;}
    //if tile is window tile 
    if self.LCDC.five { *xcoord = self.WX; *ycoord = self.WY}
    else { 
      *xcoord = ((self.SCX / 8) + *xcoord) & 0x1F; 
      *ycoord = (self.LY.wrapping_add(self.SCY)) & 255; 
    } //xcoord will be between 0 and 31, ycoord between 0 and 255
    //use xcoord and ycoord to get tile from vram (if the cpu is blocking vram, tile value read as 0xFF)
    let tile_offset = (*xcoord as u16 + ((*ycoord as u16 / 8) * 32)) as usize;
    let tile_address = self.vram[tilemap + tile_offset];
    println!("{:X?}", tile_address);
    //get tile data low and high
    let mut tile = default_tile();
    //0 = 8800–97FF (block 1 & 2); 1 = 8000–8FFF (block 0 & 1)
    if self.LCDC.four {tile = self.tile_set[tile_address as usize];}
    else {tile = self.tile_set[(0x100 + (tile_address as i8 as i16)) as usize];}
    //println!("{:?}", tile);
    //push a row of 8 pixels to background fifo
    let pixelrow = tile[(*ycoord % 8) as usize];
    if self.background_FIFO.is_empty() {
      for pixel in pixelrow {
        let temp = FIFOPixel {
          color: pixel,
          palette: 0,
          sprite_priority: 0,
          background_priority: false,
        };
        self.background_FIFO.push_back(temp);
      }
    }


    //sleep

    //push



    //sprite
    //each pixel of the target object row is checked. On CGB, horizontal flip is checked here. 
    //If the target object pixel is not white and the pixel in the OAM FIFO is white, 
    //or if the pixel in the OAM FIFO has higher priority than the target object’s pixel, 
    //then the pixel in the OAM FIFO is replaced with the target object’s properties.
    for object in &self.scanline_objects {
      if (*xcoord >= object.xcoord) & (*xcoord <= (object.xcoord + 8)) {
        let tile_address = object.tile_index + if (!self.LCDC.two) {0} else { if self.LY > (object.ycoord + 7) {1} else {0}};//check if LY is on second tile
        let tile = self.tile_set[tile_address as usize];
        //let pixelrow = tile[(if !self.LCDC.two {*ycoord % 8} else {}) as usize];
        let pixelrow = tile[(*ycoord % 8) as usize];
        if self.object_FIFO.is_empty() {
          for pixel in pixelrow {
            let temp = FIFOPixel {
              color: pixel,
              palette: (object.flags & 0b00010000),
              sprite_priority: 1,
              background_priority: (object.flags & 0b10000000) == 1,
            };
            self.object_FIFO.push_back(temp);
          }
        }
        break;
      }
    }
    
    //Before any mixing is done, if the OAM FIFO doesn’t have at least 8 pixels in it then transparent pixels with the lowest priority are pushed onto the OAM FIFO
    let mut filled: bool = self.object_FIFO.len() >= 8;
    while !filled {
      let temp = FIFOPixel {
        color: TilePixelValue::Zero,
        palette: 0,
        sprite_priority: 0,
        background_priority: true,
      };
      self.object_FIFO.push_back(temp);
      filled = self.object_FIFO.len() >= 8;
    }
  }
  fn pixel_draw(&mut self) -> TilePixelValue {//mode 3, 172-289 dots
    if self.mode == PPUMode::Two {
      self.mode = PPUMode::Three;
      //oam inaccessible (EXCEPT by DMA (io register 0xFF46))
      //vram inaccessible
      //CGB palettes inaccessible
      
      self.background_FIFO.clear();
      self.object_FIFO.clear();
    }
    let mut xcoord: u8 = self.LX;
    let mut ycoord: u8 = self.LY;
    let mut pixelcolor: TilePixelValue = TilePixelValue::None;


    //the two fifos start to be mixed
    //If there are pixels in the background and OAM FIFOs then a pixel is popped off each
    if (!self.background_FIFO.is_empty() & !self.object_FIFO.is_empty()) {
      let mut bgpixel = self.background_FIFO.pop_front().unwrap();
      let mut objpixel = self.object_FIFO.pop_front().unwrap();
      let mut bgpriority: bool = false;
      //If OAM pixel is not transparent and LCDC.1 is enabled then OAM pixel’s background priority property is used if it’s the same or higher priority as the background pixel’s background priority
      if (objpixel.color != TilePixelValue::Zero) & self.LCDC.one {bgpriority = objpixel.background_priority;}
      //if LCDC.0 == 0 pixel color value of background is 0, else color value is whatever is popped off background fifo
      if !self.LCDC.zero {bgpixel.color = TilePixelValue::Zero;}
      if bgpriority {
        if bgpixel.color != TilePixelValue::Zero {
          //When the pixel popped off the background FIFO has a color value other than 0 and it has priority then the object pixel will be discarded
          //At this point, on DMG, the color of the pixel is retrieved from the BGP register and pushed to the LCD
          pixelcolor = bgpixel.color;
        } else {
          pixelcolor = objpixel.color;
        }
      } else {
        //When an object pixel has priority, the color value is retrieved from the popped pixel from the OAM FIFO
        //On DMG the color for the pixel is retrieved from either the OBP1 or OBP0 register depending on the pixel’s palette property. 
        //If the palette property is 1 then OBP1 is used, otherwise OBP0 is used. The pixel is then pushed to the LCD.
        //On CGB when palette access is blocked, a black pixel is pushed to the LCD
        pixelcolor = objpixel.color;
      }
    } else {
      self.pixel_fetch(&mut xcoord, &mut ycoord);
      pixelcolor = self.pixel_draw();
    }
    return pixelcolor;
    //if background fifo is empty or current pixel is 160 or more, dont push a pixel to screen
  }*/
  fn horizontal_blank(&mut self) {//mode 0, 87-204 dots
    self.mode = PPUMode::Zero;
  }
  fn vertical_blank(&mut self) {//mode 1, 456 x 10 dots
    self.mode = PPUMode::One;
    self.LY = 0;
  }
  fn scanline_fetch(&mut self) -> [TilePixelValue; 160] {
    //ignore objects and windows for now
    let mut scanlinepixels = [TilePixelValue::Zero; 160];
    let mapstart: usize = if (self.LCDC.three) {0x9C00 - VRAM_BEGIN} else {0x9800 - VRAM_BEGIN};
    //current tilemap pixel line
    let pixelrow = self.SCY.wrapping_add(self.LY);
    for col in 0..160 {
      let pixelcol = self.SCX.wrapping_add(col);
      //now with the background x and y coords, need to fetch the appropriate tile from the tile map
      let tileaddress = self.vram[mapstart + ((pixelrow as u16 / 8 * 32) + (pixelcol as u16 / 8)) as usize];
      let tile = self.tile_set[tileaddress as usize];
      scanlinepixels[col as usize] = tile[(pixelrow % 8) as usize][(pixelcol % 8) as usize];
    }
    return scanlinepixels;
  }

  pub fn frame(&mut self) {
    //println!("{:?}", self.vram);
    //println!("in oam set: {:?}", self.oam_set);
    //println!("oam data: {:?}", self.oam);
    //println!("tile set: {:?}", self.tile_set);
    //let mapstart: usize = 0x9800 - VRAM_BEGIN; 
    //let slice = &self.vram[mapstart..];
    //println!("tile map: {:X?}", slice); //first map uses 0x9800 - 0x9BFF, second map uses 0x9C00 - 0x9FFF
    
    let mut pixel_data = Vec::new();
    let memprint: u8 = 0;
    if memprint == 0 {
      while self.LY < 144 {
        //println!("scanline {:?}", self.LY);
        self.oam_scan();
        let pixelline = self.scanline_fetch();
        //while self.LX < 160 {
          //let pixelcolor = self.pixel_draw();
        for pixelcolor in pixelline {
          pixel_data.push((self.LX, self.LY, match pixelcolor {
            TilePixelValue::Zero => Color::RGB(255, 255, 255),
            TilePixelValue::One => Color::RGB(170, 170, 170),
            TilePixelValue::Two => Color::RGB(85, 85, 85),
            TilePixelValue::Three => Color::RGB(0, 0, 0),
            TilePixelValue::None => Color::RGB(255, 105, 180),
          }));
          self.LX += 1;
        }
        self.horizontal_blank();
        self.LX = 0;
        self.LY += 1;
      }
    }
    
    //printing tile data
    if memprint == 1 {
      let mut row: u8 = 0;
      let mut col: u8 = 0;
      for tile in self.tile_set {
        if col == 16 { col = 0; row += 1;} 
        let mut i: u8 = 0;
        while i < 8 {
          let mut j: u8 = 0;
          while j < 8 {
            let pixelcolor = tile[i as usize][j as usize];
            pixel_data.push(((col * 8) + j, (row * 8) + i, match pixelcolor {
              TilePixelValue::Zero => Color::RGB(255, 255, 255),
              TilePixelValue::One => Color::RGB(170, 170, 170),
              TilePixelValue::Two => Color::RGB(85, 85, 85),
              TilePixelValue::Three => Color::RGB(0, 0, 0),
              TilePixelValue::None => Color::RGB(255, 105, 180),
            }));
            j += 1;
          }
          i += 1;
        }
        col += 1;
      }
    }
    
    //printing tile map
    if memprint == 2 {
      let map1start: usize = 0x9800 - VRAM_BEGIN;
      let map1end: usize = 0x9BFF - VRAM_BEGIN; 
      let slice = &self.vram[map1start..=map1end];
      let mut row: u8 = 0;
      let mut col: u8 = 0;
      for tileaddress in slice {
        let tile = self.tile_set[*tileaddress as usize];
        
        if col == 32 { col = 0; row += 1;} 
        let mut i: u8 = 0;
        while i < 8 {
          let mut j: u8 = 0;
          while j < 8 {
            let pixelcolor = tile[i as usize][j as usize];
            pixel_data.push(((col * 8) + j, (row * 8) + i, match pixelcolor {
              TilePixelValue::Zero => Color::RGB(255, 255, 255),
              TilePixelValue::One => Color::RGB(170, 170, 170),
              TilePixelValue::Two => Color::RGB(85, 85, 85),
              TilePixelValue::Three => Color::RGB(0, 0, 0),
              TilePixelValue::None => Color::RGB(255, 105, 180),
            }));
            j += 1;
          }
          i += 1;
        }
        col += 1;
        
      }
    }
    

    let texture_creator = self.canvas.texture_creator();
    let mut texture = texture_creator
        .create_texture_target(texture_creator.default_pixel_format(), 160, 144)
        .unwrap();
    let result = self.canvas.with_texture_canvas(&mut texture, |texture_canvas| {
        texture_canvas.set_draw_color(Color::RGBA(0, 255, 0, 255));
        texture_canvas.clear();
        //println!("{:?}", pixel_data);
        for (lx, ly, pixel_color) in pixel_data {
          texture_canvas.set_draw_color(pixel_color);
          texture_canvas.draw_point(Point::new(lx.into(),ly.into()));
        }
    });

    self.vertical_blank();
    
    self.canvas.copy(&texture, None, None);
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

  pub fn read_oam(&self, address: usize) -> u8 {
    self.oam[address]
  }

  pub fn write_oam(&mut self, index: usize, value: u8) {
    self.oam[index] = value;

    let setIndex = index / 4;
    match index % 4 {
      0 => self.oam_set[setIndex].ycoord = value,
      1 => self.oam_set[setIndex].xcoord = value,
      2 => self.oam_set[setIndex].tile_index = value,
      3 => self.oam_set[setIndex].flags = value,
      _ => unreachable!(),
    } 
  }
}
