use raytracing::camera::Camera;
use raytracing::color::color;
use raytracing::lights::SphereLight;
use raytracing::materials::Phong;
use raytracing::matrix::{scaling, translation};
use raytracing::shapes::{cube, plane, sphere};
use raytracing::tuple::{point, vector};
use raytracing::world::World;
use std::f64::consts::PI;
use std::fs::File;

fn main() {
    let mut world = World::empty();

    world.add_light(SphereLight::new(
        point(-5, 2, -5),
        2.0,
        color(1.5, 1.5, 1.5),
    ));

    let middle = sphere()
        .with_transform(translation(-5, 2, -5) * scaling(2, 2, 2))
        .with_material(
            Phong::default()
                .with_rgb(1.0, 1.0, 1.0)
                .with_ambient(1.0)
                .with_diffuse(0.0)
                .with_specular(0.0)
                .with_reflective(0.0),
        )
        .with_cast_shadow(false);
    world.add_item(middle);

    let floor_material = Phong::default()
        .with_color(color(1.0, 1.0, 1.0))
        .with_diffuse(0.8)
        .with_ambient(0.2)
        .with_specular(0.0);

    let floor = plane().with_material(floor_material.clone());
    world.add_item(floor);

    let middle = cube()
        .with_transform(translation(0.0, 1, 0.0))
        .with_material(
            Phong::default()
                .with_rgb(1.0, 1.0, 1.0)
                .with_ambient(0.2)
                .with_diffuse(0.8)
                .with_specular(0.0)
                .with_reflective(0.0),
        );
    world.add_item(middle);

    let mut camera = Camera::new(900, 450, PI / 3.0).with_view_transform(
        point(9, 15, -19),
        point(1.5, 1, 1),
        vector(0, 1, 0),
    );
    camera.set_allowed_standard_error(1e-2);
    camera.set_min_samples(10);

    let image = camera.render_live(&world, "Soft Shadows");

    let mut f = File::create("pictures/soft_shadow.ppm").unwrap();
    image.write_ppm(&mut f).unwrap();
}
