use raytracing::camera::Camera;
use raytracing::color::color;
use raytracing::lights::PointLight;
use raytracing::materials::Phong;
use raytracing::matrix::{rotation_x, rotation_y, scaling, translation};
use raytracing::shapes::sphere;
use raytracing::tuple::{point, vector};
use raytracing::world::World;
use std::f64::consts::PI;
use std::fs::File;

fn main() {
    let mut world = World::empty();

    world.add_light(PointLight::new(point(-10, 10, -10), color(1, 1, 1)));

    let floor_material = Phong::new(color(1, 0.9, 0.9), 0.1, 0.9, 0.0, 100.0, 0.0);

    let floor = sphere()
        .with_transform(scaling(10, 0.01, 10))
        .with_material(floor_material.clone());
    world.add_shape(floor);

    let left_wall = sphere()
        .with_transform(
            translation(0, 0, 5)
                * rotation_y(-PI / 4.0)
                * rotation_x(-PI / 2.0)
                * scaling(10, 0.01, 10),
        )
        .with_material(floor_material.clone());
    world.add_shape(left_wall);

    let right_wall = sphere()
        .with_transform(
            translation(0, 0, 5)
                * rotation_y(PI / 4.0)
                * rotation_x(PI / 2.0)
                * scaling(10, 0.01, 10),
        )
        .with_material(floor_material.clone());
    world.add_shape(right_wall);

    let middle = sphere()
        .with_transform(translation(-0.5, 1, 0.5))
        .with_material(Phong::new(color(0.1, 1, 0.5), 0.1, 0.7, 0.3, 200.0, 0.0));
    world.add_shape(middle);

    let right = sphere()
        .with_transform(translation(1.5, 0.5, -0.5) * scaling(0.5, 0.5, 0.5))
        .with_material(Phong::new(color(0.5, 1, 0.1), 0.1, 0.7, 0.3, 200.0, 0.0));
    world.add_shape(right);

    let left = sphere()
        .with_transform(translation(-1.5, 0.33, -0.75) * scaling(0.33, 0.33, 0.33))
        .with_material(Phong::new(color(1, 0.8, 0.1), 0.1, 0.7, 0.3, 200.0, 0.0));
    world.add_shape(left);

    let camera = Camera::new(900, 450, PI / 3.0).with_view_transform(
        point(0, 1.5, -5),
        point(0, 1, 0),
        vector(0, 1, 0),
    );

    let image = camera.render_live(&world, "Chapter 7");

    let mut f = File::create("pictures/chapter-07.ppm").unwrap();
    image.write_ppm(&mut f).unwrap();
}
