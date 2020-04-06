mod sphere;

pub use sphere::Sphere;

use crate::materials::Phong;
use crate::matrix::Matrix;
use crate::ray::{Intersection, Ray};
use crate::tuple::{Point, Vector};
use std::any::Any;
use std::fmt::Debug;

pub trait Shape: Debug + Any {
    fn as_any(&self) -> &dyn Any;
    fn is_similar(&self, other: &dyn Shape) -> bool;

    fn intersect(&self, ray: &Ray) -> Vec<Intersection>;
    fn normal_at(&self, world_point: Point) -> Vector;

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
