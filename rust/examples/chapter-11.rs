use raytracing::camera::Camera;
use raytracing::color::color;
use raytracing::lights::PointLight;
use raytracing::materials::Phong;
use raytracing::matrix::{scaling, translation};
use raytracing::pattern::checkers_pattern;
use raytracing::shapes::{plane, sphere};
use raytracing::tuple::{point, vector};
use raytracing::world::World;
use std::f64::consts::PI;
use std::fs::File;

fn main() {
    let mut world = World::empty();

    world.add_light(PointLight::new(point(-10, 10, -10), color(1, 1, 1)));

    let floor_material = Phong::default()
        .with_pattern(checkers_pattern(
            color(0.75, 0.25, 0.5),
            color(0.25, 0.75, 0.5),
        ))
        .with_ambient(0.25)
        .with_diffuse(0.9)
        .with_specular(0.0);

    let floor = plane().with_material(floor_material.clone());
    world.add_shape(floor);

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

    let left = sphere()
        .with_transform(translation(-1.5, 1, 0.5))
        .with_material(
            Phong::default()
                .with_color(color(0, 0, 0))
                .with_ambient(0.0)
                .with_diffuse(0.0)
                .with_specular(0.8)
                .with_reflective(1.0),
        );
    world.add_shape(left);

    let right = sphere()
        .with_transform(translation(1.5, 1, 0.5))
        .with_material(
            Phong::default()
                .with_color(color(0, 0, 0))
                .with_ambient(0.0)
                .with_diffuse(0.0)
                .with_specular(0.8)
                .with_transparency(1.0)
                .with_refractive_index(1.5),
        );
    world.add_shape(right);

    let right = sphere()
        .with_transform(translation(1.5, 1, 0.5) * scaling(0.5, 0.5, 0.5))
        .with_material(
            Phong::default()
                .with_color(color(0, 0, 0))
                .with_ambient(0.0)
                .with_diffuse(0.0)
                .with_specular(0.8)
                .with_transparency(1.0)
                .with_refractive_index(1.0),
        );
    world.add_shape(right);

    let mut camera = Camera::new(900, 450, PI / 3.0).with_view_transform(
        point(0, 1.5, -5),
        point(0, 1, 0),
        vector(0, 1, 0),
    );
    camera.set_multisampling(64);

    let image = camera.render_live(&world, "Chapter 11");

    let mut f = File::create("pictures/chapter-11.ppm").unwrap();
    image.write_ppm(&mut f).unwrap();
}
