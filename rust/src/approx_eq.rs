use crate::color::Color;
use crate::tuple::Tuple;

pub trait ApproximateEq<T = Self> {
    fn approx_eq(&self, other: &T) -> bool;
}

const EPSILON: f64 = 1e-6;

impl ApproximateEq for f64 {
    fn approx_eq(&self, other: &Self) -> bool {
        (self - other).abs() < EPSILON
    }
}

impl ApproximateEq for Tuple {
    fn approx_eq(&self, other: &Self) -> bool {
        self.x().approx_eq(&other.x())
            && self.y().approx_eq(&other.y())
            && self.z().approx_eq(&other.z())
            && self.w().approx_eq(&other.w())
    }
}

impl ApproximateEq for Color {
    fn approx_eq(&self, other: &Self) -> bool {
        self.red().approx_eq(&other.red())
            && self.green().approx_eq(&other.green())
            && self.blue().approx_eq(&other.blue())
    }
}
