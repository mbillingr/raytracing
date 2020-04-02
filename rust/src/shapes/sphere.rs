use crate::matrix::Matrix;
use crate::ray::{Intersection, Ray};
use crate::shapes::Shape;
use crate::tuple::point;

#[derive(Debug)]
pub struct Sphere {
    transform: Matrix,
}

impl Sphere {
    pub fn new() -> Self {
        Sphere {
            transform: Matrix::identity(),
        }
    }
}

impl Shape for Sphere {
    fn intersect(&self, ray: &Ray) -> Vec<Intersection> {
        let ray = ray.transform(self.transform.inverse());
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

    fn transform(&self) -> &Matrix {
        &self.transform
    }

    fn set_transform(&mut self, t: Matrix) {
        self.transform = t;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::approx_eq::ApproximateEq;
    use crate::matrix::{rotation_x, rotation_y, rotation_z, scaling, translation};
    use crate::tuple::{point, vector};

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
}
