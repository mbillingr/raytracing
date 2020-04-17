use raytracing::camera::Camera;
use raytracing::color::color;
use raytracing::lights::PointLight;
use raytracing::materials::Phong;
use raytracing::matrix::{translation, scaling, shearing};
use raytracing::pattern::checkers_pattern;
use raytracing::shapes::{plane, cube};
use raytracing::tuple::{point, vector};
use raytracing::world::World;
use std::f64::consts::PI;
use std::fs::File;

fn main() {
    let mut world = World::empty();

    world.add_light(PointLight::new(point(-10, 10, -10), color(1, 1, 1)));

    let floor_material = Phong::default()
        .with_pattern(checkers_pattern(
            color(0.25, 0.25, 0.75),
            color(0.25, 0.75, 0.25),
        ))
        .with_ambient(0.25)
        .with_diffuse(0.9)
        .with_specular(0.0);

    let floor = plane()
        .with_material(floor_material.clone())
        .with_transform(translation(0, -1, 0));
    world.add_shape(floor);

    let water_material = Phong::default()
        .with_color(color(0.1, 0.1, 0.5))
        .with_ambient(0.0)
        .with_diffuse(1.0)
        .with_specular(0.5)
        .with_reflective(1.0)
        .with_transparency(1.0)
        .with_refractive_index(1.3);

    let water = plane()
        .with_cast_shadow(false)
        .with_material(water_material.clone())
        .with_transform(translation(0, 0, 0));
    world.add_shape(water);

    let sky = plane()
        .with_transform(translation(0, 1000, 0))
        .with_material(
            Phong::default()
                .with_color(color(0.8, 0.8, 1))
                .with_ambient(1.0)
                .with_diffuse(1.0)
                .with_specular(0.0),
        );
    world.add_shape(sky);

    for i in -5 .. 5 {
        let x = i as f64 * 1.0 - 0.5;
        let z = 5.0 - i as f64 * 1.0;
        let s = i as f64 * 0.2;
        let right = cube()
            .with_transform(translation(x, 0, z) * shearing(s, 0, 0, 0, 0, 0) * scaling(0.1, 1.0, 0.1))
            .with_material(
                Phong::default()
                    .with_color(color(1, 0.2, 0.3))
                    .with_ambient(0.5)
                    .with_diffuse(1.0)
                    .with_specular(0.8),
            );
        world.add_shape(right);
    }

    let mut camera = Camera::new(900, 450, PI / 3.0).with_view_transform(
        point(0, 1.5, -5),
        point(0, 1, 0),
        vector(0, 1, 0),
    );
    camera.set_multisampling(64);

    let image = camera.render_live(&world, "Chapter 12");

    let mut f = File::create("pictures/chapter-12.ppm").unwrap();
    image.write_ppm(&mut f).unwrap();
}
