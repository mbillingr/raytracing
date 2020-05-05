use rand::{thread_rng, Rng};
use raytracing::camera::Camera;
use raytracing::color::color;
use raytracing::lights::PointLight;
use raytracing::materials::Phong;
use raytracing::matrix::{rotation_x, rotation_z, scaling, translation, Matrix};
use raytracing::pattern::checkers_pattern;
use raytracing::shapes::{
    build_bounding_tree, csg_difference, csg_intersection, cube, group, plane, sphere, SceneItem,
    Shape,
};
use raytracing::tuple::{point, vector, Vector};
use raytracing::world::World;
use std::f64::consts::PI;
use std::fs::File;

fn dice(mat1: Phong, mat2: Phong) -> SceneItem {
    let body = dice_body(mat1);
    let body = csg_difference(body, dice_side1(mat2.clone()));
    let body = csg_difference(
        body,
        dice_side6(mat2.clone()).with_transform(scaling(1, -1, 1)),
    );
    let body = csg_difference(
        body,
        dice_side2(mat2.clone()).with_transform(rotation_x(PI / 2.0)),
    );
    let body = csg_difference(
        body,
        dice_side5(mat2.clone()).with_transform(rotation_x(-PI / 2.0)),
    );
    let body = csg_difference(
        body,
        dice_side3(mat2.clone()).with_transform(rotation_z(PI / 2.0)),
    );
    let body = csg_difference(
        body,
        dice_side4(mat2.clone()).with_transform(rotation_z(-PI / 2.0)),
    );

    body.into()
}

fn dice_side1(mat: Phong) -> SceneItem {
    dice_point(0.0, 0.0, &mat).into()
}

fn dice_side2(mat: Phong) -> SceneItem {
    group()
        .with_child(dice_point(-0.8, -0.8, &mat))
        .with_child(dice_point(0.8, 0.8, &mat))
        .into()
}

fn dice_side3(mat: Phong) -> SceneItem {
    group()
        .with_child(dice_point(0.0, 0.0, &mat))
        .with_child(dice_point(-1.0, -1.0, &mat))
        .with_child(dice_point(1.0, 1.0, &mat))
        .into()
}

fn dice_side4(mat: Phong) -> SceneItem {
    group()
        .with_child(dice_point(-0.8, -0.8, &mat))
        .with_child(dice_point(-0.8, 0.8, &mat))
        .with_child(dice_point(0.8, -0.8, &mat))
        .with_child(dice_point(0.8, 0.8, &mat))
        .into()
}

fn dice_side5(mat: Phong) -> SceneItem {
    group()
        .with_child(dice_point(0.0, 0.0, &mat))
        .with_child(dice_point(-1.0, -1.0, &mat))
        .with_child(dice_point(1.0, -1.0, &mat))
        .with_child(dice_point(-1.0, 1.0, &mat))
        .with_child(dice_point(1.0, 1.0, &mat))
        .into()
}

fn dice_side6(mat: Phong) -> SceneItem {
    group()
        .with_child(dice_point(-1.0, -1.0, &mat))
        .with_child(dice_point(-1.0, 0.0, &mat))
        .with_child(dice_point(-1.0, 1.0, &mat))
        .with_child(dice_point(1.0, -1.0, &mat))
        .with_child(dice_point(1.0, 0.0, &mat))
        .with_child(dice_point(1.0, 1.0, &mat))
        .into()
}

fn dice_point(i: f64, j: f64, mat: &Phong) -> Shape {
    sphere()
        .with_material(mat.clone())
        .with_transform(translation(0.5 * i, 1, 0.5 * j) * scaling(0.2, 0.1, 0.2))
}

fn dice_body(mat: Phong) -> SceneItem {
    csg_intersection(
        cube().with_material(mat.clone()),
        sphere()
            .with_material(mat)
            .with_transform(scaling(1.5, 1.5, 1.5)),
    )
    .into()
}

fn main() {
    let mut world = World::empty();

    world.add_light(PointLight::new(point(-9, 8, -7), color(1, 1, 1)));

    let floor_material = Phong::default()
        .with_pattern(
            checkers_pattern(color(0.75, 0.75, 0.75), color(0.9, 0.9, 0.9))
                .with_transform(scaling(0.1, 0.1, 0.1)),
        )
        .with_ambient(0.5)
        .with_diffuse(0.5)
        .with_specular(0.0);

    let floor = plane()
        .with_material(floor_material.clone())
        .with_transform(translation(0, 0, 2) * rotation_x(PI / 2.0));
    world.add_shape(floor);

    let glass = Phong::new(color(0, 0, 0), 0.0, 0.0, 0.9, 500.0, 1.0, 1.0, 1.5);

    let a = sphere()
        .with_material(glass.clone())
        .with_transform(translation(0, 0, 0.8));
    let b = sphere()
        .with_material(glass.clone())
        .with_transform(translation(0, 0, -0.8));
    let lens = csg_intersection(a, b)
        .with_transform(translation(0.0, 0, 0))
        .with_cast_shadow(false);
    world.add_item(lens.into());

    let mut dices = group();

    for i in -8..=8 {
        for j in -4..=4 {
            let mut rnd = thread_rng();
            let hue = rnd.gen_range(0.0, 360.0);
            let mat1 = Phong::default()
                .with_hsv(hue, 0.8, 1.0)
                .with_ambient(0.5)
                .with_diffuse(0.5);
            let mat2 = Phong::default()
                .with_hsv(hue + 180.0, 0.8, 1.0)
                .with_ambient(0.5)
                .with_diffuse(0.5);
            let size = rnd.gen_range(0.05, 0.1);
            let pos_x = i as f64 * 0.4 + rnd.gen_range(-0.1, 0.1);
            let pos_y = j as f64 * 0.4 + rnd.gen_range(-0.1, 0.1);

            let phi = rnd.gen_range(0.0, 2.0 * PI);
            let rotax: [f64; 3] = rnd.gen();
            let rotax: Vector = rotax.into();

            dices.add_child(dice(mat1, mat2).with_transform(
                translation(pos_x, pos_y, 1.8)
                    * Matrix::rotate(Matrix::identity(), phi, rotax.normalized())
                    * scaling(size, size, size),
            ));
        }
    }

    world.add_item(build_bounding_tree(dices, 2).into());

    world.finalize_scene();

    let from = point(0, 0, -3);
    let to = point(0, 0, 0);

    let mut camera = Camera::new(900, 450, PI / 3.0).with_view_transform(from, to, vector(0, 1, 0));
    camera.set_allowed_standard_error(1e-2);
    camera.set_min_samples(10);

    let image = camera.render_live(&world, "Chapter 16");

    let mut f = File::create("pictures/chapter-16.ppm").unwrap();
    image.write_ppm(&mut f).unwrap();
}
