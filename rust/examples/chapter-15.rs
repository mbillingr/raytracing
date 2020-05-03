use raytracing::camera::Camera;
use raytracing::color::color;
use raytracing::lights::PointLight;
use raytracing::materials::Phong;
use raytracing::matrix::{rotation_x, rotation_y, scaling, translation};
use raytracing::obj_loader::ObjParser;
use raytracing::pattern::checkers_pattern;
use raytracing::shapes::{build_bounding_tree, plane, Group};
use raytracing::tuple::{point, vector};
use raytracing::world::World;
use std::f64::consts::PI;
use std::fs::File;
use std::io::Read;

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

    let mut data = String::new();
    File::open("../data/teapot.obj")
        .unwrap()
        .read_to_string(&mut data)
        .unwrap();
    let teapot: Group = ObjParser::parse_str(&data).into();
    let teapot = teapot
        .with_transform(rotation_y(PI / 4.0) * rotation_x(-PI / 2.0) * scaling(0.2, 0.2, 0.2));
    let teapot = build_bounding_tree(teapot);
    world.add_item(teapot.into());

    world.finalize_scene();

    let from = point(0, 5, -8);
    let to = point(0, 1, 0);

    let mut camera = Camera::new(900, 450, PI / 3.0).with_view_transform(from, to, vector(0, 1, 0));
    camera.set_allowed_standard_error(1e-2);
    camera.set_min_samples(10);
    camera.set_focal_distance((to - from).len());
    camera.set_aperture_size(0.1);

    let image = camera.render_live(&world, "Chapter 15");

    let mut f = File::create("pictures/chapter-15.ppm").unwrap();
    image.write_ppm(&mut f).unwrap();
}
