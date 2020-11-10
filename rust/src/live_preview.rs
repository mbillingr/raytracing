use minifb::{Key, KeyRepeat, Window, WindowOptions};
use std::sync::mpsc;
use std::sync::mpsc::{Receiver, Sender};
use std::thread;
use std::thread::JoinHandle;
use std::time::{Duration, Instant};

#[derive(Copy, Clone)]
pub enum Message {
    Clear(u8, u8, u8),
    SetPixel(i32, i32, u8, u8, u8),
}

impl Message {
    pub fn clear(c: crate::color::Color) -> Self {
        let (r, g, b) = c.to_u8();
        Message::Clear(r, g, b)
    }

    pub fn set_pixel(x: u32, y: u32, c: crate::color::Color) -> Self {
        let (r, g, b) = c.to_u8();
        Message::SetPixel(x as i32, y as i32, r, g, b)
    }
}

#[derive(Copy, Clone)]
pub enum Event {
    Forward(f64),
    Side(f64),
    Up(f64),
    Yaw(f64),
}

pub fn live_preview(
    width: usize,
    height: usize,
    window_name: &'static str,
) -> (JoinHandle<()>, Sender<Message>, Receiver<Event>) {
    let (tx, rx) = mpsc::channel();
    let (event_tx, event_rx) = mpsc::channel();

    let aspect = width as f64 / height as f64;

    let winsize = 900;

    let win_w;
    let win_h;
    if width >= height {
        win_w = winsize;
        win_h = (winsize as f64 / aspect) as usize;
    } else {
        win_w = (winsize as f64 * aspect) as usize;
        win_h = winsize;
    }

    let h = thread::spawn(move || {
        let mut window = Window::new(window_name, win_w, win_h, WindowOptions::default()).unwrap();

        let mut buffer = vec![0; win_w * win_h];

        let mut move_step = 1.0;
        let turn_step = 0.1;

        while window.is_open() {
            let deadline = Instant::now() + Duration::from_millis(100);

            while let Ok(msg) = rx.recv_timeout(Duration::from_millis(10)) {
                match msg {
                    Message::Clear(r, g, b) => {
                        let rgb32 = rgb(r, g, b);
                        buffer.iter_mut().for_each(|p| *p = rgb32);
                    }

                    Message::SetPixel(x, y, r, g, b) => {
                        let x = x as usize * win_w / width;
                        let y = y as usize * win_h / height;
                        buffer[x + y * win_w] = rgb(r, g, b);
                    }
                }
                if Instant::now() > deadline {
                    break;
                }
            }

            if window.is_key_down(Key::LeftShift) || window.is_key_down(Key::RightShift) {
                if window.is_key_pressed(Key::Up, KeyRepeat::Yes) {
                    event_tx.send(Event::Up(-move_step)).unwrap();
                }

                if window.is_key_pressed(Key::Down, KeyRepeat::Yes) {
                    event_tx.send(Event::Up(move_step)).unwrap();
                }

                if window.is_key_pressed(Key::Left, KeyRepeat::Yes) {
                    event_tx.send(Event::Side(-move_step)).unwrap();
                }

                if window.is_key_pressed(Key::Right, KeyRepeat::Yes) {
                    event_tx.send(Event::Side(move_step)).unwrap();
                }
            } else {
                if window.is_key_pressed(Key::Up, KeyRepeat::Yes) {
                    event_tx.send(Event::Forward(move_step)).unwrap();
                }

                if window.is_key_pressed(Key::Down, KeyRepeat::Yes) {
                    event_tx.send(Event::Forward(-move_step)).unwrap();
                }

                if window.is_key_pressed(Key::Left, KeyRepeat::Yes) {
                    event_tx.send(Event::Yaw(turn_step)).unwrap();
                }

                if window.is_key_pressed(Key::Right, KeyRepeat::Yes) {
                    event_tx.send(Event::Yaw(-turn_step)).unwrap();
                }

                if window.is_key_pressed(Key::NumPadPlus, KeyRepeat::Yes) {
                    move_step *= 1.5;
                    println!("Move Step: {}", move_step);
                }

                if window.is_key_pressed(Key::NumPadMinus, KeyRepeat::Yes) {
                    move_step /= 1.5;
                    println!("Move Step: {}", move_step);
                }
            }

            window.update_with_buffer(&buffer, win_w, win_h).unwrap();
        }
    });

    (h, tx, event_rx)
}

fn rgb(r: u8, g: u8, b: u8) -> u32 {
    b as u32 + g as u32 * 256 + r as u32 * 65536
}
