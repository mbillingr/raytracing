use raytracing::camera::Camera;
use raytracing::color::color;
use raytracing::lights::RealisticPointLight;
use raytracing::materials::Phong;
use raytracing::matrix::{rotation_x, rotation_y, rotation_z, scaling, translation};
use raytracing::shapes::{cube, plane, sphere};
use raytracing::tuple::{point, vector};
use raytracing::world::World;
use std::f64::consts::PI;
use std::fs::File;

fn main() {
    pretty_env_logger::init();

    let mut world = World::empty();

    world.add_light(RealisticPointLight::new(
        point(0, 8, 4),
        color(1, 1, 1) * 200,
    ));

    let white_material = Phong::new(color(1, 1, 1), 0.0, 0.5, 0.0, 100.0, 0.0, 0.0, 1.0);
    let red_material = Phong::new(color(1, 0.3, 0.3), 0.0, 0.5, 0.0, 100.0, 0.0, 0.0, 1.0);
    let green_material = Phong::new(color(0.3, 1, 0.3), 0.0, 0.5, 0.0, 100.0, 0.0, 0.0, 1.0);

    let floor = plane().with_material(white_material.clone());
    let ceiling = plane()
        .with_material(white_material.clone())
        .with_transform(translation(0, 10, 0));
    let back_wall = plane()
        .with_material(white_material.clone())
        .with_transform(translation(0, 0, 5) * rotation_x(PI / 2.0));
    let left_wall = plane()
        .with_material(red_material.clone())
        .with_transform(translation(-5, 0, 0) * rotation_z(PI / 2.0));
    let right_wall = plane()
        .with_material(green_material.clone())
        .with_transform(translation(5, 0, 0) * rotation_z(PI / 2.0));
    let front_wall = plane()
        .with_material(white_material.clone())
        .with_transform(translation(0, 0, -5) * rotation_x(PI / 2.0));

    let box1 = cube()
        .with_material(white_material.clone())
        .with_transform(translation(-2, 2, 2) * rotation_y(0.8) * scaling(1, 2, 1));

    let ball = sphere()
        .with_material(
            Phong::default()
                .with_rgb(1.0, 1.0, 1.0)
                .with_diffuse(0.0)
                .with_specular(0.0)
                .with_shininess(250.0)
                .with_transparency(0.9)
                .with_refractive_index(1.5)
                .with_reflective(0.0),
        )
        .with_transform(translation(2, 1.5, 2) * scaling(1.5, 1.5, 1.5));

    world.add_item(floor);
    world.add_item(ceiling);
    world.add_item(back_wall);
    world.add_item(left_wall);
    world.add_item(right_wall);
    world.add_item(front_wall);
    world.add_item(box1);
    world.add_item(ball);

    world.finalize_scene();

    let from = point(0, 5, -4.9);
    let to = point(0, 5, 0);

    let mut camera = Camera::new(600, 600, PI / 2.0).with_view_transform(from, to, vector(0, 1, 0));
    camera.set_allowed_standard_error(1e-2);
    camera.set_min_samples(10);

    world.enable_direct_illumination(true);
    world.enable_direct_photon_map(false);
    world.enable_diffuse_photon_map(false);
    world.enable_caustic_photon_map(false);
    let image = camera.render_live(&world, "Photon Map Example: direct light only");

    let mut f = File::create("pictures/photon-map-01-trace_direct_only.png").unwrap();
    image.write_png(&mut f).unwrap();
    log::debug!(
        "Average brightness (ray trace): {:?}",
        image.average_brightness()
    );

    world.enable_direct_illumination(false);
    world.enable_direct_photon_map(true);
    world.enable_diffuse_photon_map(false);
    world.enable_caustic_photon_map(true);
    world.compute_photon_map(75_000_000, 100);
    let image = camera.render_live(&world, "Photon Map Example: direct and caustic photons");
    let mut f = File::create("pictures/photon-map-02-direct_and_caustic_photons.png").unwrap();
    image.write_png(&mut f).unwrap();
    log::debug!(
        "Average brightness (photon map): {:?}",
        image.average_brightness()
    );

    world.enable_direct_illumination(true);
    world.enable_direct_photon_map(false);
    world.enable_diffuse_photon_map(true);
    world.enable_caustic_photon_map(true);
    world.compute_photon_map(75_000_000, 100);
    let image = camera.render_live(
        &world,
        "Photon Map Example: direct light and global illumination",
    );
    let mut f =
        File::create("pictures/photon-map-03_traced_direct_diffuse_and_caustic_photons.png")
            .unwrap();
    image.write_png(&mut f).unwrap();
    log::debug!(
        "Average brightness (ray + indirect photons): {:?}",
        image.average_brightness()
    );
}
