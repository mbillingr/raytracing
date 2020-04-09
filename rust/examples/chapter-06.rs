use raytracing::canvas::Canvas;
use raytracing::color::color;
use raytracing::lights::PointLight;
use raytracing::materials::Phong;
use raytracing::ray::{hit, Intersection, Ray};
use raytracing::shapes::sphere;
use raytracing::tuple::{point, vector};
use std::fs::File;

fn main() {
    let (width, height) = (512, 512);
    let mut canvas = Canvas::new(width, height);
    canvas.life_view("Canvas view");

    let mut obj = sphere();
    obj.set_material(Phong::new(color(0.2, 0.8, 0.9), 0.1, 0.9, 0.9, 20.0));

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

            if let Some(Intersection { t, obj }) = h {
                let p = ray.position(t);
                let eyev = -ray.direction();
                let normalv = obj.normal_at(p);
                let color = obj
                    .material()
                    .lighting(&sphere(), &light, p, eyev, normalv, false);
                canvas.set_pixel(i, j, color);
            }
        }
    }

    let mut f = File::create("pictures/chapter-06.ppm").unwrap();
    canvas.write_ppm(&mut f).unwrap();
}
