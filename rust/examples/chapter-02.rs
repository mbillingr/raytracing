use raytracing::canvas::Canvas;
use raytracing::color::color;
use raytracing::tuple::{point, vector, Tuple};
use std::fs::File;
use std::time::Duration;

fn main() {
    let (width, height) = (64, 64);
    let mut canvas = Canvas::new(width, height);
    canvas.life_view("Canvas view");

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
        let x = projectile.pos.x().round() as usize;
        let y = projectile.pos.y().round() as usize;
        if x < width && y < height {
            canvas.set_pixel(x, height - 1 - y, c);
        }
        tick(&env, &mut projectile, dt);
        std::thread::sleep(Duration::from_millis(10));
    }

    let mut f = File::create("pictures/chapter-02.ppm").unwrap();
    canvas.write_ppm(&mut f).unwrap();
}

fn tick(env: &Env, p: &mut Projectile, dt: f64) {
    p.pos = p.pos + p.vel * dt;
    p.vel = p.vel + (env.gravity + env.wind) * dt;
}

struct Projectile {
    pos: Tuple,
    vel: Tuple,
}

struct Env {
    gravity: Tuple,
    wind: Tuple,
}
