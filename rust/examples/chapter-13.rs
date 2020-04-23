use raytracing::camera::Camera;
use raytracing::color::color;
use raytracing::lights::PointLight;
use raytracing::materials::Phong;
use raytracing::matrix::{rotation_z, scaling, shearing, translation};
use raytracing::pattern::{checkers_pattern, gradient_pattern};
use raytracing::shapes::{cylinder, planar_heightmap, plane, sphere};
use raytracing::tuple::{point, vector};
use raytracing::world::World;
use std::f64::consts::PI;
use std::fs::File;

fn main() {
    let mut world = World::empty();

    world.add_light(PointLight::new(point(-10, 10, -10), color(1, 1, 1)));

    let floor_material = Phong::default()
        .with_pattern(checkers_pattern(
            color(0.2, 0.7, 0.3),
            color(0.25, 0.75, 0.2),
        ))
        .with_ambient(0.25)
        .with_diffuse(0.9)
        .with_specular(0.0);

    let floor = plane()
        .with_material(floor_material.clone())
        .with_transform(translation(0, -1, 0));

    let water_material = Phong::default()
        .with_color(color(0.1, 0.1, 0.5))
        .with_ambient(0.0)
        .with_diffuse(0.5)
        .with_specular(0.9)
        .with_reflective(1.0)
        .with_transparency(1.0)
        .with_refractive_index(1.3);
    world.add_shape(floor);

    let water = planar_heightmap(-1000.0, 1000.0, -1.1, 1.1, -2.0, 1000.0, |x, z| {
        let r = (x * x + z * z).sqrt();
        -0.1 * (r * 10.0).sin() / r
    })
    .with_material(water_material.clone());
    world.add_shape(water);

    let drop = sphere()
        .with_transform(translation(0, 0.5, 0) * scaling(0.3, 0.3, 0.3))
        .with_material(water_material);
    world.add_shape(drop);

    let sky = sphere()
        .with_transform(scaling(1000, 1000, 1000))
        .with_material(
            Phong::default()
                .with_pattern(
                    gradient_pattern(color(0.9, 0.9, 1), color(0.2, 0.2, 0.8))
                        .with_transform(rotation_z(PI / 2.0)),
                )
                .with_ambient(1.0)
                .with_diffuse(0.0)
                .with_specular(0.0),
        );
    world.add_shape(sky);

    for i in -5..5 {
        let x = i as f64 * 1.0 - 0.5;
        let z = 5.0 - i as f64 * 1.0;
        let s = i as f64 * 0.2;
        let right = cylinder()
            .with_transform(
                translation(x, 0, z) * shearing(s, 0, 0, 0, 0, 0) * scaling(0.1, 2.5, 0.1),
            )
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
    camera.set_allowed_standard_error(1e-2);
    camera.set_min_samples(10);
    camera.set_focal_distance(5.0);
    camera.set_aperture_size(0.1);

    let image = camera.render_live(&world, "Chapter 12");

    let mut f = File::create("pictures/chapter-13.ppm").unwrap();
    image.write_ppm(&mut f).unwrap();
}
