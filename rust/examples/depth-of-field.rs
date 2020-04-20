use raytracing::camera::Camera;
use raytracing::color::color;
use raytracing::lights::PointLight;
use raytracing::materials::Phong;
use raytracing::matrix::translation;
use raytracing::pattern::checkers_pattern;
use raytracing::shapes::{plane, sphere};
use raytracing::tuple::{point, vector};
use raytracing::world::World;
use std::f64::consts::PI;
use std::fs::File;

fn main() {
    let mut world = World::empty();

    world.add_light(PointLight::new(point(-10, 10, -10), color(0.5, 0.5, 0.5)));
    world.add_light(PointLight::new(point(-10, 10, 10), color(0.5, 0.5, 0.5)));

    let floor_material = Phong::default()
        .with_pattern(checkers_pattern(color(0.1, 0.1, 0.1), color(0.9, 0.9, 0.9)))
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

    let front = sphere()
        .with_transform(translation(-1.5, 1, 0.5))
        .with_material(
            Phong::default()
                .with_rgb(0.1, 1.0, 0.5)
                .with_diffuse(0.7)
                .with_specular(0.3),
        );
    world.add_shape(front);

    let back = sphere()
        .with_transform(translation(0.5, 1, 10.5))
        .with_material(
            Phong::default()
                .with_rgb(0.1, 1.0, 0.5)
                .with_diffuse(0.7)
                .with_specular(0.3),
        );
    world.add_shape(back);

    let behind = sphere()
        .with_transform(translation(1.5, 1, -3.5))
        .with_material(
            Phong::default()
                .with_rgb(0.5, 0.1, 1.0)
                .with_diffuse(0.7)
                .with_specular(0.3),
        );
    world.add_shape(behind);

    let middle = sphere()
        .with_transform(translation(2.0, 1, 4.5))
        .with_material(
            Phong::default()
                .with_rgb(0.0, 0.0, 0.0)
                .with_ambient(0.0)
                .with_diffuse(0.0)
                .with_specular(1.0)
                .with_reflective(1.0)
        );
    world.add_shape(middle);

    let mut camera = Camera::new(900, 450, PI / 3.0).with_view_transform(
        point(0, 1.5, -5),
        point(0, 1, 0),
        vector(0, 1, 0),
    );
    camera.set_allowed_standard_error(5e-3);
    camera.set_focal_distance(10.0);
    camera.set_aperture_size(0.25);

    let image = camera.render_live(&world, "Depth of Field Example");

    let mut f = File::create("pictures/depth-of-field.ppm").unwrap();
    image.write_ppm(&mut f).unwrap();
}
