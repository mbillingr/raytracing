use raytracing::camera::Camera;
use raytracing::color::color;
use raytracing::lights::PointLight;
use raytracing::materials::Phong;
use raytracing::math::Squared;
use raytracing::matrix::{rotation_x, rotation_y, rotation_z, scaling, translation};
use raytracing::shapes::{bounding_group, group, sphere, Cylinder, SceneItem, Shape};
use raytracing::tuple::{point, vector};
use raytracing::world::World;
use std::f64::consts::PI;
use std::fs::File;

fn build_joint(length: f64, radius: f64, mat: Phong) -> SceneItem {
    let ball = sphere()
        .with_material(mat.clone())
        .with_transform(scaling(radius, radius, radius));
    let stick = Shape::new(Cylinder::new(0.0, length, false))
        .with_material(mat)
        .with_transform(scaling(radius, 1.0, radius));
    let mut group = group();
    group.add_child(ball);
    group.add_child(stick);
    group.into()
}

fn build_approx_torus(r1: f64, r2: f64, n_segments: u16, mat: Phong) -> SceneItem {
    let segment_angle = PI * 2.0 / n_segments as f64;
    let segment_len =
        (segment_angle.sin().squared() + (1.0 - segment_angle.cos()).squared()).sqrt() * r1;
    let segment_transform = translation(segment_len / 2.0, 0.0, r1 * (0.5 * segment_angle).cos())
        * rotation_z(PI / 2.0);
    let mut group = bounding_group();
    for i in 0..n_segments {
        let segment = build_joint(segment_len, r2, mat.clone())
            .with_transform(rotation_y(segment_angle * i as f64) * segment_transform);
        group.add_child(segment);
    }
    group.into()
}

fn main() {
    let mut world = World::empty();

    world.add_light(PointLight::new(point(-10, 10, -10), color(1, 1, 1)));

    world.add_item(
        build_approx_torus(1.0, 0.25, 6, Phong::default()).with_transform(translation(0, 0.25, 0)),
    );
    world.add_item(
        build_approx_torus(1.0, 0.25, 6, Phong::default())
            .with_transform(translation(-2, 1.5, 1) * rotation_x(1)),
    );
    world.add_item(
        build_approx_torus(1.0, 0.25, 6, Phong::default())
            .with_transform(translation(2.1, 1.4, 1) * rotation_z(1)),
    );

    world.finalize_scene();

    let mut camera = Camera::new(900, 450, PI / 3.0).with_view_transform(
        point(0, 1.5, -5),
        point(0, 1, 0),
        vector(0, 1, 0),
    );
    camera.set_allowed_standard_error(1e-2);
    camera.set_min_samples(3);
    //camera.set_focal_distance(5.0);
    //camera.set_aperture_size(0.1);

    let image = camera.render_live(&world, "Chapter 14");

    let mut f = File::create("pictures/chapter-14.ppm").unwrap();
    image.write_ppm(&mut f).unwrap();
}
