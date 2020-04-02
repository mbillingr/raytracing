use crate::matrix::Matrix;
use crate::ray::{Intersection, Ray};
use crate::shapes::Shape;
use crate::tuple::{point, vector, Tuple};

#[derive(Debug)]
pub struct Sphere {
    transform: Matrix,
    inv_transform: Matrix,
}

impl Sphere {
    pub fn new() -> Self {
        Sphere {
            transform: Matrix::identity(),
            inv_transform: Matrix::identity(),
        }
    }
}

impl Shape for Sphere {
    fn intersect(&self, ray: &Ray) -> Vec<Intersection> {
        let ray = ray.transform(*self.inv_transform());
        let sphere_to_ray = ray.origin() - point(0, 0, 0);

        let a = ray.direction().square_len();
        let b = 2.0 * ray.direction().dot(&sphere_to_ray);
        let c = sphere_to_ray.square_len() - 1.0;
        let discriminant = b * b - 4.0 * a * c;

        if discriminant < 0.0 {
            intersections![]
        } else {
            intersections![
                Intersection::new(-(b + discriminant.sqrt()) / (2.0 * a), self),
                Intersection::new(-(b - discriminant.sqrt()) / (2.0 * a), self),
            ]
        }
    }

    fn normal_at(&self, world_point: Tuple) -> Tuple {
        let obj_point = *self.inv_transform() * world_point;
        let obj_normal = obj_point - point(0, 0, 0);
        let world_normal = self.inv_transform().transpose() * obj_normal;
        vector(world_normal.x(), world_normal.y(), world_normal.z()).normalized()
    }

    fn set_transform(&mut self, t: Matrix) {
        self.transform = t;
        self.inv_transform = t.inverse();
    }

    fn transform(&self) -> &Matrix {
        &self.transform
    }

    fn inv_transform(&self) -> &Matrix {
        &self.inv_transform
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::approx_eq::ApproximateEq;
    use crate::matrix::{rotation_x, rotation_y, rotation_z, scaling, translation};
    use crate::tuple::{point, vector};
    use std::f64::consts::PI;

    /// A ray intersects a sphere at two points
    #[test]
    fn intersect() {
        let r = Ray::new(point(0, 0, -5), vector(0, 0, 1));
        let s = Sphere::new();
        let xs = s.intersect(&r);
        assert_eq!(xs.len(), 2);
        assert_almost_eq!(xs[0].t, 4.0);
        assert_almost_eq!(xs[1].t, 6.0);
    }

    /// A ray intersects a sphere at a tangent
    #[test]
    fn tangent() {
        let r = Ray::new(point(0, 1, -5), vector(0, 0, 1));
        let s = Sphere::new();
        let xs = s.intersect(&r);
        assert_eq!(xs.len(), 2);
        assert_almost_eq!(xs[0].t, 5.0);
        assert_almost_eq!(xs[1].t, 5.0);
    }

    /// A ray misses a sphere
    #[test]
    fn miss() {
        let r = Ray::new(point(0, 2, -5), vector(0, 0, 1));
        let s = Sphere::new();
        let xs = s.intersect(&r);
        assert!(xs.is_empty(), 0);
    }

    /// A ray originates inside a sphere
    #[test]
    fn inside() {
        let r = Ray::new(point(0, 0, 0), vector(0, 0, 1));
        let s = Sphere::new();
        let xs = s.intersect(&r);
        assert_eq!(xs.len(), 2);
        assert_almost_eq!(xs[0].t, -1.0);
        assert_almost_eq!(xs[1].t, 1.0);
    }

    /// A sphere is behind a ray
    #[test]
    fn behind() {
        let r = Ray::new(point(0, 0, 5), vector(0, 0, 1));
        let s = Sphere::new();
        let xs = s.intersect(&r);
        assert_eq!(xs.len(), 2);
        assert_almost_eq!(xs[0].t, -6.0);
        assert_almost_eq!(xs[1].t, -4.0);
    }

    /// Intersect sets the object on intersection
    #[test]
    fn intersect_obj() {
        let r = Ray::new(point(0, 0, -5), vector(0, 0, 1));
        let s = Sphere::new();
        let xs = s.intersect(&r);
        assert_eq!(xs.len(), 2);
        assert_eq!(
            xs[0].obj as *const _ as *const u8,
            &s as *const _ as *const u8
        );
        assert_eq!(
            xs[1].obj as *const _ as *const u8,
            &s as *const _ as *const u8
        );
    }

    /// A sphere's default transformation
    #[test]
    fn default_transform() {
        let s = Sphere::new();
        assert_eq!(*s.transform(), Matrix::identity());
    }

    /// Changing a sphere's transformation
    #[test]
    fn new_transform() {
        let mut s = Sphere::new();
        let t = translation(2, 3, 4);
        s.set_transform(t);
        assert_eq!(*s.transform(), t);
    }

    /// Intersecting a scaled sphere with a ray
    #[test]
    fn intersect_scaled() {
        let r = Ray::new(point(0, 0, -5), vector(0, 0, 1));
        let mut s = Sphere::new();
        s.set_transform(scaling(2, 2, 2));
        let xs = s.intersect(&r);
        assert_eq!(xs.len(), 2);
        assert_almost_eq!(xs[0].t, 3.0);
        assert_almost_eq!(xs[1].t, 7.0);
    }

    /// Intersecting a translated sphere with a ray
    #[test]
    fn intersect_translated() {
        let r = Ray::new(point(0, 0, -5), vector(0, 0, 1));
        let mut s = Sphere::new();
        s.set_transform(translation(5, 0, 0));
        let xs = s.intersect(&r);
        assert!(xs.is_empty());
    }

    /// Intersecting a rotated sphere with a ray
    #[test]
    fn intersect_rotated() {
        let r = Ray::new(point(0, 0, -5), vector(0, 0, 1));
        let mut s = Sphere::new();
        s.set_transform(rotation_x(1) * rotation_y(2) * rotation_z(3));
        let xs = s.intersect(&r);
        assert_eq!(xs.len(), 2);
        assert_almost_eq!(xs[0].t, 4.0);
        assert_almost_eq!(xs[1].t, 6.0);
    }

    // The normal on a sphere at a point on the x axis
    #[test]
    fn normal_x() {
        let s = Sphere::new();
        let n = s.normal_at(point(1, 0, 0));
        assert_almost_eq!(n, vector(1, 0, 0));
    }

    // The normal on a sphere at a point on the y axis
    #[test]
    fn normal_y() {
        let s = Sphere::new();
        let n = s.normal_at(point(0, 1, 0));
        assert_almost_eq!(n, vector(0, 1, 0));
    }

    // The normal on a sphere at a point on the z axis
    #[test]
    fn normal_z() {
        let s = Sphere::new();
        let n = s.normal_at(point(0, 0, 1));
        assert_almost_eq!(n, vector(0, 0, 1));
    }

    // The normal on a sphere at a nonaxial point
    #[test]
    fn normal_any() {
        let s3 = f64::sqrt(3.0) / 3.0;
        let s = Sphere::new();
        let n = s.normal_at(point(s3, s3, s3));
        assert_almost_eq!(n, vector(s3, s3, s3));
    }

    // The normal is a normalized vector
    #[test]
    fn normal_is_normalized() {
        let s3 = f64::sqrt(3.0) / 3.0;
        let s = Sphere::new();
        let n = s.normal_at(point(s3, s3, s3));
        assert_almost_eq!(n, n.normalized());
    }

    // Computing the normal on a translated sphere
    #[test]
    fn normal_translated() {
        let mut s = Sphere::new();
        s.set_transform(translation(0, 1, 0));
        let n = s.normal_at(point(0, 1.70711, -0.70711));
        assert_almost_eq!(n, vector(0, 0.70711, -0.70711));
    }

    // Computing the normal on a transformed sphere
    #[test]
    fn normal_transformed() {
        let s2 = f64::sqrt(2.0) / 2.0;
        let mut s = Sphere::new();
        s.set_transform(scaling(1, 0.5, 1) * rotation_z(PI / 5.0));
        let n = s.normal_at(point(0, s2, -s2));
        assert_almost_eq!(n, vector(0, 0.97014, -0.24254));
    }
}
