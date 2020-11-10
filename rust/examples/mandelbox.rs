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

    let material = MandelMaterial::default();

    let fractal = mandelbox(-1.5, 50).with_material(material);
    world.add_item(fractal);

    let mut camera = Camera::new(900, 600, PI / 3.0).with_view_transform(
        point(0, 0, -10),
        point(0, 0, 0),
        vector(0, 1, 0),
    );
    camera.set_allowed_standard_error(f64::INFINITY);

    let image = camera.render_interactive(&world, "Mandelbox");

    let mut f = File::create("pictures/mandelbox.png").unwrap();
    image.write_png(&mut f).unwrap();
}
