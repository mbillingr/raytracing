use raytracing::canvas::Canvas;
use raytracing::color::color;
use raytracing::live_preview::{live_preview, Message};
use raytracing::ray::{hit, Intersection, Ray};
use raytracing::shapes::sphere;
use raytracing::tuple::{point, vector};
use std::fs::File;

fn main() {
    let (width, height) = (512, 512);
    let mut canvas = Canvas::new(width, height);

    let (h, tx) = live_preview(width as usize, height as usize, "Chapter 5");

    let scene = vec![sphere()];

    let eye = point(0, 0, -10);
    let look = vector(0, 0, 3); // magnitude = focal length

    let mut mi = std::f64::INFINITY;
    let mut ma = std::f64::NEG_INFINITY;
    for j in 0..height {
        let y = 0.5 - j as f64 / (height - 1) as f64;
        for i in 0..width {
            let x = i as f64 / (width - 1) as f64 - 0.5;
            let ray = Ray::new(eye, look + vector(x, y, 0));

            let intersections: Vec<_> = scene
                .iter()
                .flat_map(|shape| shape.intersect(&ray))
                .collect();
            let h = hit(&intersections);

            let c = match h {
                None => color(0.5, 0.5, 0.5),
                Some(Intersection { t, .. }) => {
                    mi = mi.min(t);
                    ma = ma.max(t);
                    let d = (3.3 - t) * 3.0;
                    color(0, d, 1)
                }
            };

            canvas.set_pixel(i, j, c);
            tx.send(Message::set_pixel(i, j, c)).unwrap();
        }
    }

    println!("{} {}", mi, ma);

    let mut f = File::create("pictures/chapter-05.png").unwrap();
    canvas.write_png(&mut f).unwrap();

    h.join().unwrap();
}
