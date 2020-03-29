use crate::canvas::Canvas;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::Color;
use sdl2::pixels::PixelFormatEnum::RGB888;
use sdl2::rect::Point;
use sdl2::render::RenderTarget;
use std::sync::mpsc;
use std::thread;
use std::time::{Duration, Instant};

#[derive(Copy, Clone)]
pub enum Message {
    Clear(u8, u8, u8),
    SetPixel(i32, i32, u8, u8, u8),
}

impl Canvas {
    pub fn life_view(&mut self, window_name: &'static str) {
        let (tx, rx) = mpsc::channel();

        let width = self.width() as u32;
        let height = self.height() as u32;

        let h = thread::spawn(move || {
            let sdl_context = sdl2::init().unwrap();
            let video_subsystem = sdl_context.video().unwrap();

            let window = video_subsystem
                .window(window_name, 800, 600)
                .position_centered()
                .build()
                .unwrap();

            let mut window_canvas = window.into_canvas().build().unwrap();

            let texcreator = window_canvas.texture_creator();
            let mut tex = texcreator
                .create_texture_target(RGB888, width, height)
                .unwrap();

            window_canvas
                .with_texture_canvas(&mut tex, |canvas| {
                    for y in 0..height as i32 {
                        for x in 0..width as i32 {
                            let color = if (x + y) % 2 == 0 {
                                Color::RGB(30, 30, 30)
                            } else {
                                Color::RGB(70, 70, 70)
                            };
                            canvas.set_draw_color(color);
                            canvas.draw_point(Point::new(x, y)).unwrap();
                        }
                    }
                })
                .unwrap();

            let mut event_pump = sdl_context.event_pump().unwrap();
            'running: loop {
                for event in event_pump.poll_iter() {
                    match event {
                        Event::Quit { .. }
                        | Event::KeyDown {
                            keycode: Some(Keycode::Escape),
                            ..
                        } => break 'running,
                        _ => {}
                    }
                }

                let deadline = Instant::now() + Duration::from_millis(100);

                while let Ok(msg) = rx.recv_timeout(Duration::from_millis(10)) {
                    match msg {
                        Message::Clear(r, g, b) => window_canvas
                            .with_texture_canvas(&mut tex, clear_canvas(r, g, b))
                            .unwrap(),
                        Message::SetPixel(x, y, r, g, b) => window_canvas
                            .with_texture_canvas(&mut tex, set_pixel(x, y, r, g, b))
                            .unwrap(),
                    }
                    if Instant::now() > deadline {
                        break;
                    }
                }

                window_canvas.clear();
                window_canvas.copy(&tex, None, None).unwrap();
                window_canvas.present();
            }
        });
        self.threads.push(h);
        self.listeners.push(tx);
    }
}

fn clear_canvas<T: RenderTarget>(r: u8, g: u8, b: u8) -> impl FnOnce(&mut sdl2::render::Canvas<T>) {
    move |canvas| {
        canvas.set_draw_color(Color::RGB(r, g, b));
        canvas.clear();
    }
}

fn set_pixel<T: RenderTarget>(
    x: i32,
    y: i32,
    r: u8,
    g: u8,
    b: u8,
) -> impl FnOnce(&mut sdl2::render::Canvas<T>) {
    move |canvas| {
        canvas.set_draw_color(Color::RGB(r, g, b));
        canvas.draw_point(Point::new(x, y)).unwrap();
    }
}
