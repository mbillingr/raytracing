use raytracing::camera::Camera;
use raytracing::color::{color, BLACK, WHITE};
use raytracing::lights::PointLight;
use raytracing::materials::Phong;
use raytracing::matrix::{rotation, scaling, translation};
use raytracing::pattern::{checkers_pattern, gradient_pattern, ring_pattern};
use raytracing::shapes::{plane, sphere};
use raytracing::tuple::{point, vector};
use raytracing::world::World;
use std::f64::consts::PI;
use std::fs::File;

fn main() {
    let mut world = World::empty();

    world.add_light(PointLight::new(point(-10, 10, -10), color(1, 1, 1)));

    let floor_material = Phong::default()
        .with_pattern(ring_pattern(color(0.75, 0.25, 0.5), color(0.25, 0.75, 0.5)))
        .with_specular(0.0);

    let floor = plane().with_material(floor_material.clone());
    world.add_item(floor);

    let sky = plane()
        .with_transform(translation(0, 1000, 0))
        .with_material(
            Phong::default()
                .with_color(color(0.8, 0.8, 1))
                .with_ambient(1.0)
                .with_diffuse(1.0)
                .with_specular(0.0),
        );
    world.add_item(sky);

    let middle = sphere()
        .with_transform(translation(-0.5, 1, 0.5))
        .with_material(
            Phong::default()
                .with_pattern(
                    gradient_pattern(color(0.75, 0.75, 0.5), color(0.1, 0.5, 1))
                        .with_transform(scaling(0.1, 0.1, 0.1) * rotation(42.0, vector(4, 2, 3))),
                )
                .with_diffuse(0.7)
                .with_specular(0.3),
        );
    world.add_item(middle);

    let right = sphere()
        .with_transform(translation(1.5, 0.5, -0.5) * scaling(0.5, 0.5, 0.5))
        .with_material(
            Phong::default()
                .with_pattern(checkers_pattern(WHITE, BLACK).with_transform(scaling(0.1, 0.1, 0.1)))
                .with_diffuse(0.7)
                .with_specular(0.3),
        );
    world.add_item(right);

    let left = sphere()
        .with_transform(translation(-1.5, 0.33, -0.75) * scaling(0.33, 0.33, 0.33))
        .with_material(
            Phong::default()
                .with_color(color(1, 0.8, 0.1))
                .with_diffuse(0.7)
                .with_specular(0.3),
        );
    world.add_item(left);

    let mut camera = Camera::new(900, 450, PI / 3.0).with_view_transform(
        point(0, 1.5, -5),
        point(0, 1, 0),
        vector(0, 1, 0),
    );
    camera.set_allowed_standard_error(1e-2);

    let image = camera.render_live(&world, "Chapter 10");

    let mut f = File::create("pictures/chapter-10.ppm").unwrap();
    image.write_ppm(&mut f).unwrap();
}
