mod sphere;

pub use sphere::Sphere;

use crate::materials::Phong;
use crate::matrix::Matrix;
use crate::ray::{Intersection, Ray};
use crate::tuple::Tuple;
use std::fmt::Debug;

pub trait Shape: Debug {
    fn intersect(&self, ray: &Ray) -> Vec<Intersection>;
    fn normal_at(&self, world_point: Tuple) -> Tuple;

    fn set_transform(&mut self, t: Matrix);
    fn transform(&self) -> &Matrix;
    fn inv_transform(&self) -> &Matrix;

    fn set_material(&mut self, m: Phong);
    fn material(&self) -> &Phong;
}

impl PartialEq for dyn Shape + '_ {
    fn eq(&self, other: &Self) -> bool {
        self as *const _ == other as *const _
    }
}
