use raytracing::canvas::Canvas;
use raytracing::color::color;
use raytracing::lights::{Light, PointLight};
use raytracing::live_preview::{live_preview, Message};
use raytracing::materials::Phong;
use raytracing::ray::{hit, IntersectionState, Ray};
use raytracing::shapes::sphere;
use raytracing::tuple::{point, vector};
use std::fs::File;

fn main() {
    let (width, height) = (512, 512);
    let mut canvas = Canvas::new(width, height);

    let (h, tx) = live_preview(width as usize, height as usize, "Chapter 6");

    let mut obj = sphere();
    obj.set_material(
        Phong::default()
            .with_rgb(0.2, 0.8, 0.9)
            .with_shininess(20.0),
    );

    let scene = vec![obj];
    let light = PointLight::new(point(1, 9, -10), color(1, 1, 1));

    let eye = point(0, 0, -10);
    let look = vector(0, 0, 3); // magnitude = focal length

    for j in 0..height {
        let y = 0.5 - j as f64 / (height - 1) as f64;
        for i in 0..width {
            let x = i as f64 / (width - 1) as f64 - 0.5;
            let ray = Ray::new(eye, (look + vector(x, y, 0)).normalized());

            let intersections: Vec<_> = scene
                .iter()
                .flat_map(|shape| shape.intersect(&ray))
                .collect();
            let h = hit(&intersections);

            if let Some(intersection) = h {
                let t = intersection.t;
                let obj = intersection.obj;
                let p = ray.position(t);
                let eyev = -ray.direction();
                let normalv = obj.normal_at(p, &intersection);
                let comps = IntersectionState {
                    t,
                    obj,
                    inside: false,
                    point: p,
                    over_point: p,
                    under_point: p,
                    eyev,
                    normalv,
                    reflectv: vector(0, 0, 0),
                    n1: 1.0,
                    n2: 1.0,
                    mat1: None,
                    mat2: None,
                };
                let color = obj.material().lighting(light.incoming_at(p), &comps, false);
                canvas.set_pixel(i, j, color);
                tx.send(Message::set_pixel(i, j, color)).unwrap();
            }
        }
    }

    let mut f = File::create("pictures/chapter-06.png").unwrap();
    canvas.write_png(&mut f).unwrap();

    h.join().unwrap();
}
