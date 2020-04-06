use std::ops::{Add, Div, Mul, Neg, Sub};
use vecmath::{
    vec3_cross, vec4_add, vec4_dot, vec4_inv_len, vec4_len, vec4_normalized, vec4_scale,
    vec4_square_len, vec4_sub, Vector4,
};

pub fn point(x: impl Into<f64>, y: impl Into<f64>, z: impl Into<f64>) -> Point {
    Point::new(x.into(), y.into(), z.into())
}

pub fn vector(x: impl Into<f64>, y: impl Into<f64>, z: impl Into<f64>) -> Vector {
    Vector::new(x.into(), y.into(), z.into())
}

#[derive(Debug, Copy, Clone)]
pub struct Point(pub(crate) Vector4<f64>);

impl Point {
    fn new(x: f64, y: f64, z: f64) -> Self {
        Point([x, y, z, 1.0])
    }

    pub fn x(&self) -> f64 {
        self.0[0]
    }

    pub fn y(&self) -> f64 {
        self.0[1]
    }

    pub fn z(&self) -> f64 {
        self.0[2]
    }

    pub fn w(&self) -> f64 {
        self.0[3]
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Vector(pub(crate) Vector4<f64>);

impl Vector {
    fn new(x: f64, y: f64, z: f64) -> Self {
        Vector([x, y, z, 0.0])
    }

    pub fn x(&self) -> f64 {
        self.0[0]
    }

    pub fn y(&self) -> f64 {
        self.0[1]
    }

    pub fn z(&self) -> f64 {
        self.0[2]
    }

    pub fn w(&self) -> f64 {
        self.0[3]
    }

    pub fn len(&self) -> f64 {
        vec4_len(self.0)
    }

    pub fn square_len(&self) -> f64 {
        vec4_square_len(self.0)
    }

    pub fn inv_len(&self) -> f64 {
        vec4_inv_len(self.0)
    }

    pub fn normalized(&self) -> Self {
        Vector(vec4_normalized(self.0))
    }

    pub fn dot(&self, rhs: &Self) -> f64 {
        vec4_dot(self.0, rhs.0)
    }

    pub fn cross(&self, rhs: &Self) -> Self {
        let v3 = vec3_cross([self.x(), self.y(), self.z()], [rhs.x(), rhs.y(), rhs.z()]);
        vector(v3[0], v3[1], v3[2])
    }

    pub fn reflect(&self, normal: &Self) -> Self {
        *self - *normal * (2.0 * self.dot(normal))
    }
}

const EPSILON: f64 = 1e-6;

pub fn float_equal(a: f64, b: f64) -> bool {
    (a - b).abs() < EPSILON
}

impl Add<Vector> for Point {
    type Output = Point;
    fn add(self, other: Vector) -> Self::Output {
        vec4_add(self.0, other.0).into()
    }
}

impl Add<Point> for Vector {
    type Output = Point;
    fn add(self, other: Point) -> Self::Output {
        vec4_add(self.0, other.0).into()
    }
}

impl Add<Vector> for Vector {
    type Output = Vector;
    fn add(self, other: Vector) -> Self::Output {
        vec4_add(self.0, other.0).into()
    }
}

impl Sub<Point> for Point {
    type Output = Vector;
    fn sub(self, other: Self) -> Self::Output {
        vec4_sub(self.0, other.0).into()
    }
}

impl Sub<Vector> for Point {
    type Output = Point;
    fn sub(self, other: Vector) -> Self::Output {
        vec4_sub(self.0, other.0).into()
    }
}

impl Sub<Vector> for Vector {
    type Output = Vector;
    fn sub(self, other: Vector) -> Self::Output {
        vec4_sub(self.0, other.0).into()
    }
}

impl Neg for Vector {
    type Output = Vector;
    fn neg(self) -> Self::Output {
        Vector::new(-self.x(), -self.y(), -self.z())
    }
}

impl<T: Into<f64>> Mul<T> for Vector {
    type Output = Vector;
    fn mul(self, rhs: T) -> Self::Output {
        vec4_scale(self.0, rhs.into()).into()
    }
}

impl Mul<Vector> for f64 {
    type Output = Vector;
    fn mul(self, rhs: Vector) -> Self::Output {
        vec4_scale(rhs.0, self).into()
    }
}

impl Mul<Vector> for i32 {
    type Output = Vector;
    fn mul(self, rhs: Vector) -> Self::Output {
        vec4_scale(rhs.0, self as f64).into()
    }
}

impl<T: Into<f64>> Div<T> for Vector {
    type Output = Vector;
    fn div(self, rhs: T) -> Self::Output {
        vec4_scale(self.0, 1.0 / rhs.into()).into()
    }
}

impl From<Vector4<f64>> for Vector {
    fn from(v: Vector4<f64>) -> Self {
        Vector(v)
    }
}

impl From<Vector4<f64>> for Point {
    fn from(v: Vector4<f64>) -> Self {
        Point(v)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::approx_eq::ApproximateEq;

    /// Only for testing, implement an inaccurate PartialEq
    impl PartialEq for Point {
        fn eq(&self, other: &Self) -> bool {
            self.approx_eq(other)
        }
    }

    /// Only for testing, implement an inaccurate PartialEq
    impl PartialEq for Vector {
        fn eq(&self, other: &Self) -> bool {
            self.approx_eq(other)
        }
    }

    /// point() creates tuples with w=1
    #[test]
    fn point_factory() {
        let p = point(4.0, -4.0, 3.0);
        assert_eq!(p.w(), 1.0);
    }

    /// vector() creates tuples with w=0
    #[test]
    fn vector_factory() {
        let p = vector(4.0, -4.0, 3.0);
        assert_eq!(p.w(), 0.0);
    }

    /// Adding two tuples
    #[test]
    fn add_tuples() {
        let p = point(3.0, -2.0, 5.0);
        let v = vector(-2.0, 3.0, 1.0);
        assert_eq!(p + v, point(1.0, 1.0, 6.0));
        assert_eq!(v + p, point(1.0, 1.0, 6.0));
        assert_eq!(v + v, vector(-4.0, 6.0, 2.0));
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
        let a = vector(1.0, -2.0, 3.0);
        assert_eq!(-a, vector(-1.0, 2.0, -3.0));
    }

    /// Multiplying a tuple by a scalar
    #[test]
    fn tuple_mul_scalar() {
        let a = vector(1, -2, 3);
        assert_eq!(a * 3.5, vector(3.5, -7, 10.5));
        assert_eq!(3.5 * a, vector(3.5, -7, 10.5));
    }

    /// Multiplying a tuple by a fraction
    #[test]
    fn tuple_mul_frac() {
        let a = vector(1, -2, 3);
        assert_eq!(a * 0.5, vector(0.5, -1, 1.5));
        assert_eq!(0.5 * a, vector(0.5, -1, 1.5));
    }

    /// Dividing a tuple by a scalar
    #[test]
    fn tuple_div_scalar() {
        let a = vector(1, -2, 3);
        assert_eq!(a / 2, vector(0.5, -1, 1.5));
    }

    /// Magnitude of vector(1, 0, 0)
    #[test]
    fn magnitude_unit_x() {
        let v = vector(1, 0, 0);
        assert_eq!(v.len(), 1.0);
    }

    /// Magnitude of vector(0, 1, 0)
    #[test]
    fn magnitude_unit_y() {
        let v = vector(0, 1, 0);
        assert_eq!(v.len(), 1.0);
    }

    /// Magnitude of vector(0, 0, 1)
    #[test]
    fn magnitude_unit_z() {
        let v = vector(0, 0, 1);
        assert_eq!(v.len(), 1.0);
    }

    /// Magnitude of vector(1, 2, 3)
    #[test]
    fn magnitude_positive_vec() {
        let v = vector(1, 2, 3);
        assert_eq!(v.len(), 14f64.sqrt());
    }

    /// Magnitude of vector(-1, -2, -3)
    #[test]
    fn magnitude_negative_vec() {
        let v = vector(-1, -2, -3);
        assert_eq!(v.len(), 14f64.sqrt());
    }

    /// Normalization of vector(4, 0, 0)
    #[test]
    fn normalize_unit_x() {
        let v = vector(4, 0, 0);
        assert_eq!(v.normalized(), vector(1, 0, 0));
    }

    /// Normalization of vector(1, 2, 3)
    #[test]
    fn normalize_vec() {
        let v = vector(1, 2, 3);
        assert_eq!(
            v.normalized(),
            vector(1.0 / 14f64.sqrt(), 2.0 / 14f64.sqrt(), 3.0 / 14f64.sqrt())
        );
    }

    /// Magnitude of normalied vector
    #[test]
    fn magnitude_of_normalized_vector() {
        let v = vector(1, 2, 3);
        assert_eq!(v.normalized().len(), 1.0);
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

    /// Reflecting a vector approaching at 45deg
    #[test]
    fn reflect_up() {
        let v = vector(1, -1, 0);
        let n = vector(0, 1, 0);
        let r = v.reflect(&n);
        assert_almost_eq!(r, vector(1, 1, 0));
    }

    /// Reflecting a vector off a slanted surface
    #[test]
    fn reflect_45() {
        let v = vector(0, -1, 0);
        let n = vector(
            std::f64::consts::FRAC_1_SQRT_2,
            std::f64::consts::FRAC_1_SQRT_2,
            0,
        );
        let r = v.reflect(&n);
        assert_almost_eq!(r, vector(1, 0, 0));
    }
}
