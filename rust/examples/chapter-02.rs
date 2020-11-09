use raytracing::canvas::Canvas;
use raytracing::color::color;
use raytracing::live_preview::{live_preview, Message};
use raytracing::tuple::{point, vector, Point, Vector};
use std::fs::File;
use std::time::Duration;

fn main() {
    let (width, height) = (64, 64);
    let mut canvas = Canvas::new(width, height);

    let (h, tx) = live_preview(width as usize, height as usize, "Chapter 2");

    let env = Env {
        gravity: vector(0, -1, 0),
        wind: vector(-0.2, 0, 0),
    };

    let mut projectile = Projectile {
        pos: point(2, 5, 0),
        vel: vector(1, 1.5, 0) * 5,
    };

    let c = color(0.5, 0.5, 0.8);

    let dt = 0.1;
    while projectile.pos.y() > 0.0 {
        let x = projectile.pos.x().round() as u32;
        let y = projectile.pos.y().round() as u32;
        if x < width && y < height {
            canvas.set_pixel(x, height - 1 - y, c);
            tx.send(Message::set_pixel(x, height - 1 - y, c)).unwrap();
        }
        tick(&env, &mut projectile, dt);
        std::thread::sleep(Duration::from_millis(10));
    }

    let mut f = File::create("pictures/chapter-02.png").unwrap();
    canvas.write_png(&mut f).unwrap();

    h.join().unwrap();
}

fn tick(env: &Env, p: &mut Projectile, dt: f64) {
    p.pos = p.pos + p.vel * dt;
    p.vel = p.vel + (env.gravity + env.wind) * dt;
}

struct Projectile {
    pos: Point,
    vel: Vector,
}

struct Env {
    gravity: Vector,
    wind: Vector,
}
