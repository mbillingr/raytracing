use raytracing::camera::Camera;
use raytracing::color::color;
use raytracing::lights::{AmbientLight, Beam};
use raytracing::materials::Phong;
use raytracing::shapes::plane;
use raytracing::tuple::{point, vector};
use raytracing::world::World;
use std::f64::consts::PI;
use std::fs::File;

fn main() {
    pretty_env_logger::init();

    let mut world = World::empty();

    world.add_light(Beam::new(
        point(0, 1, 0),
        vector(-2, 0, 2),
        vector(0.3, 0, 0.3),
        color(1, 0.1, 0.1) * 1000,
    ));

    world.add_light(AmbientLight::new(color(0.1, 0.1, 0.1)));

    let white_material = Phong::new(color(1, 1, 1), 0.0, 0.5, 0.0, 100.0, 0.0, 0.0, 1.0);

    let floor = plane().with_material(white_material.clone());

    world.add_item(floor);

    world.finalize_scene();

    let from = point(0, 8, 0);
    let to = point(0, 0, 0);

    let mut camera = Camera::new(600, 600, PI / 2.0).with_view_transform(from, to, vector(0, 0, 1));
    camera.set_allowed_standard_error(1e2);
    camera.set_min_samples(10);

    world.enable_direct_illumination(true);
    world.enable_direct_photon_map(false);
    world.enable_diffuse_photon_map(false);
    world.enable_caustic_photon_map(false);
    let image = camera.render_live(&world, "Light Beam example");
    let mut f = File::create("pictures/light-beam-01-trace_direct_only.png").unwrap();
    image.write_png(&mut f).unwrap();
    log::debug!(
        "Average brightness (ray trace): {:?}",
        image.average_brightness()
    );

    world.enable_direct_illumination(false);
    world.enable_direct_photon_map(true);
    world.enable_diffuse_photon_map(false);
    world.enable_caustic_photon_map(true);
    world.compute_photon_map(75_000, 1, 0.1);
    let image = camera.render_live(&world, "Light Beam example");
    let mut f = File::create("pictures/light-beam-02-direct_and_caustic_photons.png").unwrap();
    image.write_png(&mut f).unwrap();
    log::debug!(
        "Average brightness (photon map): {:?}",
        image.average_brightness()
    );

    world.enable_direct_illumination(true);
    world.enable_direct_photon_map(false);
    world.enable_diffuse_photon_map(true);
    world.enable_caustic_photon_map(true);
    world.compute_photon_map(75_000, 1, 0.1);
    let image = camera.render_live(&world, "Light Beam example");
    let mut f =
        File::create("pictures/light-beam-03_traced_direct_diffuse_and_caustic_photons.png")
            .unwrap();
    image.write_png(&mut f).unwrap();
    log::debug!(
        "Average brightness (ray + indirect photons): {:?}",
        image.average_brightness()
    );
}
