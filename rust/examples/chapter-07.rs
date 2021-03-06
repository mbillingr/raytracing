use raytracing::camera::Camera;
use raytracing::color::color;
use raytracing::lights::{AmbientLight, PointLight};
use raytracing::materials::Phong;
use raytracing::matrix::{rotation_x, rotation_y, scaling, translation};
use raytracing::shapes::sphere;
use raytracing::tuple::{point, vector};
use raytracing::world::World;
use std::f64::consts::PI;
use std::fs::File;

fn main() {
    let mut world = World::empty();

    world.add_light(AmbientLight::new(color(0.1, 0.1, 0.1)));
    world.add_light(PointLight::new(point(-10, 10, -10), color(1, 1, 1)));

    let floor_material = Phong::default()
        .with_color(color(1, 0.9, 0.9))
        .with_specular(0.0)
        .with_shininess(100.0);

    let floor = sphere()
        .with_transform(scaling(10, 0.01, 10))
        .with_material(floor_material.clone());
    world.add_item(floor);

    let left_wall = sphere()
        .with_transform(
            translation(0, 0, 5)
                * rotation_y(-PI / 4.0)
                * rotation_x(-PI / 2.0)
                * scaling(10, 0.01, 10),
        )
        .with_material(floor_material.clone());
    world.add_item(left_wall);

    let right_wall = sphere()
        .with_transform(
            translation(0, 0, 5)
                * rotation_y(PI / 4.0)
                * rotation_x(PI / 2.0)
                * scaling(10, 0.01, 10),
        )
        .with_material(floor_material.clone());
    world.add_item(right_wall);

    let middle = sphere()
        .with_transform(translation(-0.5, 1, 0.5))
        .with_material(
            Phong::default()
                .with_rgb(0.1, 1.0, 0.5)
                .with_diffuse(0.7)
                .with_specular(0.3),
        );
    world.add_item(middle);

    let right = sphere()
        .with_transform(translation(1.5, 0.5, -0.5) * scaling(0.5, 0.5, 0.5))
        .with_material(
            Phong::default()
                .with_rgb(0.5, 1.0, 0.1)
                .with_diffuse(0.7)
                .with_specular(0.3),
        );
    world.add_item(right);

    let left = sphere()
        .with_transform(translation(-1.5, 0.33, -0.75) * scaling(0.33, 0.33, 0.33))
        .with_material(
            Phong::default()
                .with_rgb(1.0, 0.8, 0.1)
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

    let image = camera.render_live(&world, "Chapter 7");

    let mut f = File::create("pictures/chapter-07.png").unwrap();
    image.write_png(&mut f).unwrap();
}
