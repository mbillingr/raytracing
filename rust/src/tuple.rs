use std::ops::{Add, Div, Mul, Neg, Sub};

pub fn tuple(x: impl Into<f64>, y: impl Into<f64>, z: impl Into<f64>, w: impl Into<f64>) -> Tuple {
    Tuple::new(x.into(), y.into(), z.into(), w.into())
}

pub fn point(x: impl Into<f64>, y: impl Into<f64>, z: impl Into<f64>) -> Tuple {
    Tuple::point(x.into(), y.into(), z.into())
}

pub fn vector(x: impl Into<f64>, y: impl Into<f64>, z: impl Into<f64>) -> Tuple {
    Tuple::vector(x.into(), y.into(), z.into())
}

#[derive(Debug, Copy, Clone)]
pub struct Tuple(f64, f64, f64, f64);

impl Tuple {
    pub fn new(x: f64, y: f64, z: f64, w: f64) -> Self {
        Tuple(x, y, z, w)
    }

    pub fn point(x: f64, y: f64, z: f64) -> Self {
        Tuple::new(x, y, z, 1.0)
    }

    pub fn vector(x: f64, y: f64, z: f64) -> Self {
        Tuple::new(x, y, z, 0.0)
    }

    pub fn x(&self) -> f64 {
        self.0
    }

    pub fn y(&self) -> f64 {
        self.1
    }

    pub fn z(&self) -> f64 {
        self.2
    }

    pub fn w(&self) -> f64 {
        self.3
    }

    pub fn is_point(&self) -> bool {
        self.w() == 1.0
    }

    pub fn is_vector(&self) -> bool {
        self.w() == 0.0
    }

    pub fn add(&self, other: &Self) -> Self {
        Tuple::new(
            self.0 + other.0,
            self.1 + other.1,
            self.2 + other.2,
            self.3 + other.3,
        )
    }

    pub fn sub(&self, other: &Self) -> Self {
        Tuple::new(
            self.0 - other.0,
            self.1 - other.1,
            self.2 - other.2,
            self.3 - other.3,
        )
    }

    pub fn neg(&self) -> Self {
        Tuple::new(-self.0, -self.1, -self.2, -self.3)
    }

    pub fn scale(&self, s: f64) -> Self {
        Tuple::new(self.0 * s, self.1 * s, self.2 * s, self.3 * s)
    }

    pub fn magnitude(&self) -> f64 {
        self.dot(self).sqrt()
    }

    pub fn normalize(&self) -> Self {
        self.scale(1.0 / self.magnitude())
    }

    pub fn dot(&self, rhs: &Self) -> f64 {
        self.0 * rhs.0 + self.1 * rhs.1 + self.2 * rhs.2 + self.3 * rhs.3
    }

    pub fn cross(&self, rhs: &Self) -> Self {
        debug_assert!(self.is_vector());
        debug_assert!(rhs.is_vector());
        vector(
            self.y() * rhs.z() - self.z() * rhs.y(),
            self.z() * rhs.x() - self.x() * rhs.z(),
            self.x() * rhs.y() - self.y() * rhs.x(),
        )
    }
}

const EPSILON: f64 = 1e-6;

pub fn float_equal(a: f64, b: f64) -> bool {
    (a - b).abs() < EPSILON
}

impl Add for Tuple {
    type Output = Tuple;
    fn add(self, other: Self) -> Self::Output {
        Tuple::add(&self, &other)
    }
}

impl Sub for Tuple {
    type Output = Tuple;
    fn sub(self, other: Self) -> Self::Output {
        Tuple::sub(&self, &other)
    }
}

impl Neg for Tuple {
    type Output = Tuple;
    fn neg(self) -> Self::Output {
        Tuple::neg(&self)
    }
}

impl<T: Into<f64>> Mul<T> for Tuple {
    type Output = Tuple;
    fn mul(self, rhs: T) -> Self::Output {
        Tuple::scale(&self, rhs.into())
    }
}

impl Mul<Tuple> for f64 {
    type Output = Tuple;
    fn mul(self, rhs: Tuple) -> Self::Output {
        Tuple::scale(&rhs, self)
    }
}

impl Mul<Tuple> for i32 {
    type Output = Tuple;
    fn mul(self, rhs: Tuple) -> Self::Output {
        Tuple::scale(&rhs, self as f64)
    }
}

impl<T: Into<f64>> Div<T> for Tuple {
    type Output = Tuple;
    fn div(self, rhs: T) -> Self::Output {
        Tuple::scale(&self, 1.0 / rhs.into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::approx_eq::ApproximateEq;

    /// Only for testing, implement an inaccurate PartialEq
    impl PartialEq for Tuple {
        fn eq(&self, other: &Self) -> bool {
            self.approx_eq(other)
        }
    }

    ///  A tuple with w=1.0 is a point
    #[test]
    fn tuple_as_point() {
        let a = Tuple::new(4.3, -4.2, 3.1, 1.0);
        assert_eq!(a.x(), 4.3);
        assert_eq!(a.y(), -4.2);
        assert_eq!(a.z(), 3.1);
        assert_eq!(a.w(), 1.0);
        assert!(a.is_point());
        assert!(!a.is_vector());
    }

    ///  A tuple with w=0 is a vector
    #[test]
    fn tuple_as_vector() {
        let a = Tuple::new(4.3, -4.2, 3.1, 0.0);
        assert_eq!(a.x(), 4.3);
        assert_eq!(a.y(), -4.2);
        assert_eq!(a.z(), 3.1);
        assert_eq!(a.w(), 0.0);
        assert!(!a.is_point());
        assert!(a.is_vector());
    }

    /// point() creates tuples with w=1
    #[test]
    fn point_factory() {
        let p = point(4.0, -4.0, 3.0);
        assert_eq!(p, tuple(4.0, -4.0, 3.0, 1.0));
    }

    /// vector() creates tuples with w=0
    #[test]
    fn vector_factory() {
        let p = vector(4.0, -4.0, 3.0);
        assert_eq!(p, tuple(4.0, -4.0, 3.0, 0.0));
    }

    /// Adding two tuples
    #[test]
    fn add_tuples() {
        let a1 = tuple(3.0, -2.0, 5.0, 1.0);
        let a2 = tuple(-2.0, 3.0, 1.0, 0.0);
        assert_eq!(a1 + a2, tuple(1.0, 1.0, 6.0, 1.0));
    }

    /// Subtracting two points
    #[test]
    fn sub_points() {
        let p1 = point(3.0, 2.0, 1.0);
        let p2 = point(5.0, 6.0, 7.0);
        assert_eq!(p1 - p2, vector(-2.0, -4.0, -6.0));
    }

    /// Subtracting a vector from a point
    #[test]
    fn sub_vector_from_point() {
        let p = point(3.0, 2.0, 1.0);
        let v = vector(5.0, 6.0, 7.0);
        assert_eq!(p - v, point(-2.0, -4.0, -6.0));
    }

    /// Subtracting two vectors
    #[test]
    fn sub_vectors() {
        let v1 = vector(3.0, 2.0, 1.0);
        let v2 = vector(5.0, 6.0, 7.0);
        assert_eq!(v1 - v2, vector(-2.0, -4.0, -6.0));
    }

    /// Subtracting a vector from the zero vector
    #[test]
    fn sub_vec_from_zero() {
        let zero = vector(0.0, 0.0, 0.0);
        let v = vector(1.0, -2.0, 3.0);
        assert_eq!(zero - v, vector(-1.0, 2.0, -3.0));
    }

    /// Negate a tuple
    #[test]
    fn negate_tuple() {
        let a = tuple(1.0, -2.0, 3.0, -4.0);
        assert_eq!(-a, tuple(-1.0, 2.0, -3.0, 4.0));
    }

    /// Multiplying a tuple by a scalar
    #[test]
    fn tuple_mul_scalar() {
        let a = tuple(1, -2, 3, -4);
        assert_eq!(a * 3.5, tuple(3.5, -7, 10.5, -14));
        assert_eq!(3.5 * a, tuple(3.5, -7, 10.5, -14));
    }

    /// Multiplying a tuple by a fraction
    #[test]
    fn tuple_mul_frac() {
        let a = tuple(1, -2, 3, -4);
        assert_eq!(a * 0.5, tuple(0.5, -1, 1.5, -2));
        assert_eq!(0.5 * a, tuple(0.5, -1, 1.5, -2));
    }

    /// Dividing a tuple by a scalar
    #[test]
    fn tuple_div_scalar() {
        let a = tuple(1, -2, 3, -4);
        assert_eq!(a / 2, tuple(0.5, -1, 1.5, -2));
    }

    /// Magnitude of vector(1, 0, 0)
    #[test]
    fn magnitude_unit_x() {
        let v = vector(1, 0, 0);
        assert_eq!(v.magnitude(), 1.0);
    }

    /// Magnitude of vector(0, 1, 0)
    #[test]
    fn magnitude_unit_y() {
        let v = vector(0, 1, 0);
        assert_eq!(v.magnitude(), 1.0);
    }

    /// Magnitude of vector(0, 0, 1)
    #[test]
    fn magnitude_unit_z() {
        let v = vector(0, 0, 1);
        assert_eq!(v.magnitude(), 1.0);
    }

    /// Magnitude of vector(1, 2, 3)
    #[test]
    fn magnitude_positive_vec() {
        let v = vector(1, 2, 3);
        assert_eq!(v.magnitude(), 14f64.sqrt());
    }

    /// Magnitude of vector(-1, -2, -3)
    #[test]
    fn magnitude_negative_vec() {
        let v = vector(-1, -2, -3);
        assert_eq!(v.magnitude(), 14f64.sqrt());
    }

    /// Normalization of vector(4, 0, 0)
    #[test]
    fn normalize_unit_x() {
        let v = vector(4, 0, 0);
        assert_eq!(v.normalize(), vector(1, 0, 0));
    }

    /// Normalization of vector(1, 2, 3)
    #[test]
    fn normalize_vec() {
        let v = vector(1, 2, 3);
        assert_eq!(
            v.normalize(),
            vector(1.0 / 14f64.sqrt(), 2.0 / 14f64.sqrt(), 3.0 / 14f64.sqrt())
        );
    }

    /// Magnitude of normalied vector
    #[test]
    fn magnitude_of_normalized_vector() {
        let v = vector(1, 2, 3);
        assert_eq!(v.normalize().magnitude(), 1.0);
    }

    /// Dot product of two tuples
    #[test]
    fn dot_product() {
        let a = vector(1, 2, 3);
        let b = vector(2, 3, 4);
        assert_eq!(a.dot(&b), 20.0);
    }

    /// Cross product of two vectors
    #[test]
    fn cross_product() {
        let a = vector(1, 2, 3);
        let b = vector(2, 3, 4);
        assert_eq!(a.cross(&b), vector(-1, 2, -1));
        assert_eq!(b.cross(&a), vector(1, -2, 1));
    }
}
