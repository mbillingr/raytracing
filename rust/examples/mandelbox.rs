use raytracing::camera::Camera;
use raytracing::color::color;
use raytracing::lights::{AmbientLight, PointLight};
use raytracing::materials::Phong;
use raytracing::matrix::{rotation_x, rotation_y, scaling, translation};
use raytracing::shapes::{mandelbox, MandelMaterial};
use raytracing::tuple::{point, vector};
use raytracing::world::World;
use std::f64::consts::PI;
use std::fs::File;

fn main() {
    let mut world = World::empty();

    world.add_light(AmbientLight::new(color(0.1, 0.1, 0.1)));
    world.add_light(PointLight::new(point(-5, 10, -10), color(1, 1, 1)));

    let material = Phong::default()
        .with_color(color(1, 0.9, 0.9))
        .with_specular(0.9)
        .with_shininess(100.0);

    let material = MandelMaterial::default();

    let fractal = mandelbox(3.0, 10).with_material(material);
    world.add_item(fractal);

    let mut camera = Camera::new(1500, 800, PI / 3.0).with_view_transform(
        point(-3, 1.5, -7),
        point(0, 0, 0),
        vector(0, 1, 0),
    );
    camera.set_allowed_standard_error(1e-2);

    let image = camera.render_live(&world, "Mandelbox");

    let mut f = File::create("pictures/mandelbox.png").unwrap();
    image.write_png(&mut f).unwrap();
}
