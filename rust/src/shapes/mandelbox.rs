use crate::aabb::Aabb;
use crate::approx_eq::ApproximateEq;
use crate::color::{color, Color, BLACK, WHITE};
use crate::lights::IncomingLight;
use crate::materials::{Material, Phong};
use crate::photon_map::TravellingPhoton;
use crate::ray::{Intersection, IntersectionState, Ray};
use crate::shapes::{Geometry, Shape};
use crate::tuple::point;
use crate::tuple::{vector, Point, Vector, ORIGIN};
use crate::world::World;
use std::any::Any;
use std::cell::Cell;
use std::sync::atomic::AtomicUsize;

thread_local! {
    static LAST_DEPTH: Cell<usize> = Cell::new(0);
}

pub fn mandelbox(scale: f64, n_iter: usize) -> Shape {
    Shape::new(Mandelbox::new(scale, n_iter))
}

#[derive(Debug, Copy, Clone)]
pub struct Mandelbox {
    scale: f64,
    n_iter: usize,
}

impl Mandelbox {
    pub fn new(scale: f64, n_iter: usize) -> Self {
        Mandelbox { scale, n_iter }
    }
}

impl Geometry for Mandelbox {
    fn duplicate(&self) -> Box<dyn Geometry> {
        Box::new(*self)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn is_similar(&self, other: &dyn Geometry) -> bool {
        other
            .as_any()
            .downcast_ref::<Mandelbox>()
            .map(|o| self.scale.approx_eq(&o.scale) && self.n_iter == o.n_iter)
            .unwrap_or(false)
    }

    fn intersect<'a>(&self, obj: &'a Shape, local_ray: &Ray) -> Vec<Intersection<'a>> {
        const MIN_DISTANCE: f64 = 1e-6;
        const MAX_DISTANCE: f64 = 6.0;
        const FUDGE_FACTOR: f64 = 0.9;
        let mut c = local_ray.origin();
        let mut ray_len = 0.0;
        let mut last_d = f64::INFINITY;
        let mut n_steps = 0;
        loop {
            let d = estimate_distance(c, self.scale, self.n_iter);
            if d <= MIN_DISTANCE {
                LAST_DEPTH.with(|s| s.set(n_steps));
                return vec![Intersection::new(ray_len, obj)];
            }
            if d >= MAX_DISTANCE && d > last_d {
                LAST_DEPTH.with(|s| s.set(n_steps));
                return vec![];
            }
            n_steps += 1;
            ray_len += d * FUDGE_FACTOR;
            c = local_ray.origin() + local_ray.direction() * ray_len;
            last_d = d;
        }
    }

    fn normal_at(&self, local_point: Point, _: &Intersection) -> Vector {
        let diff_step = 1e-2;
        let dist0 = estimate_distance(local_point, self.scale, self.n_iter);
        let dist_x = estimate_distance(
            local_point + vector(diff_step, 0.0, 0.0),
            self.scale,
            self.n_iter,
        );
        let dist_y = estimate_distance(
            local_point + vector(0.0, diff_step, 0.0),
            self.scale,
            self.n_iter,
        );
        let dist_z = estimate_distance(
            local_point + vector(0.0, 0.0, diff_step),
            self.scale,
            self.n_iter,
        );
        let gradient = vector(dist_x - dist0, dist_y - dist0, dist_z - dist0) / diff_step;
        gradient.normalized()
    }

    fn aabb(&self) -> Aabb {
        Aabb::new(-2.0, 2.0, -2.0, 2.0, -2.0, 2.0)
    }
}

#[derive(Debug, Clone, Default)]
pub struct MandelMaterial {
    phong: Phong,
}

impl MandelMaterial {}

impl Material for MandelMaterial {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn box_clone(&self) -> Box<dyn Material> {
        Box::new(self.clone())
    }

    fn is_similar(&self, other: &dyn Material) -> bool {
        unimplemented!()
    }

    fn color_at(&self, comps: &IntersectionState) -> Color {
        let branches = trace_branches(comps.point, 3.0, 2);
        let mut col = BLACK;
        for b in &branches {
            match b {
                0 => col = col + color(1, 0, 0),
                1 => col = col + color(0, 1, 0),
                2 => col = col + color(0, 0, 1),
                _ => unreachable!(),
            }
        }
        col / branches.len() as f64
    }

    fn lighting(&self, light: IncomingLight, comps: &IntersectionState, in_shadow: bool) -> Color {
        unimplemented!()
    }

    fn shade_hit(&self, world: &World, comps: &IntersectionState, remaining_bounces: u32) -> Color {
        let surface_color = self.color_at(&comps);

        /*let xs = world.intersect(&Ray::new(comps.over_point, comps.normalv));

        if xs.is_empty() {
            surface_color
        } else {
            if xs[0].t > 1.0 {
                surface_color
            } else {
                surface_color * xs[0].t
            }
        }*/
        surface_color * (100.0 / LAST_DEPTH.with(|s| s.get()) as f64)
    }

    fn photon_hit(
        &self,
        photon: TravellingPhoton,
        comps: &IntersectionState,
        enable_diffuse: bool,
    ) -> (Option<TravellingPhoton>, Option<TravellingPhoton>) {
        unimplemented!()
    }

    fn refractive_index(&self) -> f64 {
        self.phong.refractive_index()
    }
}

pub fn estimate_distance(c: Point, scale: f64, n_iter: usize) -> f64 {
    let mut z = c;
    let mut dr = 1.0;
    for _ in 0..n_iter {
        let (z1, dr1) = box_fold(z, dr, 1.0);
        let (z1, dr1) = sphere_fold(z1, dr1, 0.25, 1.0);
        z = (z1 - ORIGIN) * scale + c;
        dr = dr1 * scale.abs() + 1.0;
    }
    (z - ORIGIN).len() / dr.abs()
}

fn box_fold(z: Point, dz: f64, limit: f64) -> (Point, f64) {
    let bound = (z
        .max(&point(-limit, -limit, -limit))
        .min(&point(limit, limit, limit))
        - point(0, 0, 0))
        * 2.0;
    let z_out = ORIGIN + (ORIGIN - z) + bound;
    (z_out, dz)
}

fn sphere_fold(z: Point, dz: f64, inner_r2: f64, outer_r2: f64) -> (Point, f64) {
    let z_dir = z - ORIGIN;
    let r2 = z_dir.square_len();
    let scale = match r2 {
        _ if r2 < inner_r2 => outer_r2 / inner_r2,
        _ if r2 < outer_r2 => outer_r2 / r2,
        _ => 1.0,
    };
    let z_out = ORIGIN + z_dir * scale;
    (z_out, dz * scale)
}

pub fn trace_branches(c: Point, scale: f64, n_iter: usize) -> Vec<usize> {
    let mut z = c;
    let mut branches = vec![];
    for _ in 0..n_iter {
        z = trace_box_fold(z, &mut branches, 1.0);
        z = trace_sphere_fold(z, &mut branches, 0.25, 1.0);
        z = (z - ORIGIN) * scale + c;
    }
    branches
}

fn trace_box_fold(z: Point, branches: &mut Vec<usize>, limit: f64) -> Point {
    let bound = (z
        .max(&point(-limit, -limit, -limit))
        .min(&point(limit, limit, limit))
        - point(0, 0, 0))
        * 2.0;
    let z_out = ORIGIN + (ORIGIN - z) + bound;
    z_out
}

fn trace_sphere_fold(z: Point, branches: &mut Vec<usize>, inner_r2: f64, outer_r2: f64) -> Point {
    let z_dir = z - ORIGIN;
    let r2 = z_dir.square_len();
    let scale = match r2 {
        _ if r2 < inner_r2 => {
            branches.push(0);
            outer_r2 / inner_r2
        }
        _ if r2 < outer_r2 => {
            branches.push(1);
            outer_r2 / r2
        }
        _ => {
            branches.push(2);
            1.0
        }
    };
    let z_out = ORIGIN + z_dir * scale;
    z_out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::approx_eq::ApproximateEq;
    use crate::tuple::vector;

    #[test]
    fn box_fold_does_not_change_origin() {
        let z_in = point(0.0, 0.0, 0.0);
        let dz_in = 1.0;
        let (z_out, dz_out) = box_fold(z_in, dz_in, 1.0);
        assert_eq!(z_out, z_in);
        assert_eq!(dz_out, dz_in);
    }

    #[test]
    fn box_fold_does_not_change_boundary() {
        let z_in = point(-1.0, 1.0, 1.0);
        let dz_in = 1.0;
        let (z_out, dz_out) = box_fold(z_in, dz_in, 1.0);
        assert_eq!(z_out, z_in);
        assert_eq!(dz_out, dz_in);
    }

    #[test]
    fn box_fold_outside_boundaries() {
        let z_in = point(-1.5, 2.0, 3.5);
        let dz_in = 1.0;
        let (z_out, dz_out) = box_fold(z_in, dz_in, 1.0);
        assert_eq!(z_out, point(-0.5, 0.0, -1.5));
        assert_eq!(dz_out, dz_in);
    }

    #[test]
    fn sphere_fold_does_not_change_origin() {
        let z_in = point(0.0, 0.0, 0.0);
        let dz_in = 0.0;
        let (z_out, dz_out) = sphere_fold(z_in, dz_in, 1.0, 2.0);
        assert_eq!(z_out, z_in);
        assert_eq!(dz_out, 0.0);
    }

    #[test]
    fn sphere_fold_linear_scaling_inside_inner() {
        let z_in = point(0.1, 0.2, 0.3);
        let dz_in = 1.0;
        let (z_out, dz_out) = sphere_fold(z_in, dz_in, 1.0, 2.0);
        assert_eq!(z_out, point(0.2, 0.4, 0.6));
        assert_eq!(dz_out, dz_in * 2.0);
    }

    #[test]
    fn sphere_fold_sphere_inversion_inside_outer() {
        let z_in = point(1.2, 0.0, 0.0);
        let dz_in = 1.0;
        let (z_out, dz_out) = sphere_fold(z_in, dz_in, 1.0, 2.0);
        assert_eq!(z_out, point(5.0 / 3.0, 0.0, 0.0));
        assert_eq!(dz_out, dz_in * 2.0 / (1.2 * 1.2));
    }

    #[test]
    fn sphere_fold_scaling_at_inner() {
        let z_in = point(1.0, 0.0, 0.0);
        let dz_in = 1.0;
        let (z_out, dz_out) = sphere_fold(z_in, dz_in, 1.0, 2.0);
        assert_eq!(z_out, point(2.0, 0.0, 0.0));
        assert_eq!(dz_out, dz_in * 2.0);
    }

    #[test]
    fn sphere_fold_scaling_at_outer() {
        let z_in = point(2.0, 0.0, 0.0);
        let dz_in = 1.0;
        let (z_out, dz_out) = sphere_fold(z_in, dz_in, 1.0, 2.0);
        assert_eq!(z_out, z_in);
        assert_eq!(dz_out, dz_in);
    }

    #[test]
    fn distance_estimation_at_origin() {
        let c = ORIGIN;
        let d = estimate_distance(c, 1.0, 1);
        assert_eq!(d, 0.0);
    }

    #[test]
    fn distance_estimation_far_away() {
        let c = point(100.0, 0.0, 0.0);
        let d = estimate_distance(c, -1.5, 100);
        assert_almost_eq!(d, 98.0)
    }

    #[test]
    fn intersect_misses() {
        let m = mandelbox(-1.5, 100);
        let r = Ray::new(point(10.0, 3.0, 0.0), vector(-1, 0, 0));

        let xs = m.intersect(&r);
        assert_eq!(xs, vec![]);
    }

    #[test]
    fn intersect_center() {
        let m = mandelbox(-1.5, 100);
        let r = Ray::new(point(10.0, 0.0, 0.0), vector(-1, 0, 0));

        let xs = m.intersect(&r);
        assert_eq!(xs.len(), 1);
        assert_almost_eq!(xs[0].t, 8.0);
    }
}
