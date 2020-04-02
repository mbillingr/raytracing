mod sphere;

pub use sphere::Sphere;

use crate::matrix::Matrix;
use crate::ray::{Intersection, Ray};
use std::fmt::Debug;

pub trait Shape: Debug {
    fn intersect(&self, ray: &Ray) -> Vec<Intersection>;
    fn transform(&self) -> &Matrix;
    fn set_transform(&mut self, t: Matrix);
}

impl PartialEq for dyn Shape + '_ {
    fn eq(&self, other: &Self) -> bool {
        self as *const _ == other as *const _
    }
}
