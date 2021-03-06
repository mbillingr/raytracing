use crate::aabb::Aabb;
use crate::materials::Phong;
use crate::ray::{Intersection, Ray};
use crate::shapes::{Geometry, Shape};
use crate::tuple::{point, Point, Vector};
use std::any::Any;

pub fn sphere() -> Shape {
    Shape::new(Sphere::new())
}

/// A helper for producing a sphere with a glassy material
pub fn glass_sphere() -> Shape {
    sphere().with_material(
        Phong::default()
            .with_rgb(1.0, 1.0, 1.0)
            .with_diffuse(0.1)
            .with_specular(0.9)
            .with_shininess(250.0)
            .with_transparency(1.0)
            .with_refractive_index(1.5),
    )
}

#[derive(Debug, Copy, Clone)]
pub struct Sphere;

impl Sphere {
    pub fn new() -> Self {
        Sphere
    }
}

impl Geometry for Sphere {
    fn duplicate(&self) -> Box<dyn Geometry> {
        Box::new(*self)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn is_similar(&self, other: &dyn Geometry) -> bool {
        other.as_any().downcast_ref::<Sphere>().is_some()
    }

    fn intersect<'a>(&self, obj: &'a Shape, local_ray: &Ray) -> Vec<Intersection<'a>> {
        let sphere_to_ray = local_ray.origin() - point(0, 0, 0);

        let a = local_ray.direction().square_len();
        let b = 2.0 * local_ray.direction().dot(&sphere_to_ray);
        let c = sphere_to_ray.square_len() - 1.0;
        let discriminant = b * b - 4.0 * a * c;

        if discriminant < 0.0 {
            intersections![]
        } else {
            intersections![
                Intersection::new(-(b + discriminant.sqrt()) / (2.0 * a), obj),
                Intersection::new(-(b - discriminant.sqrt()) / (2.0 * a), obj),
            ]
        }
    }

    fn normal_at(&self, local_point: Point, _: &Intersection) -> Vector {
        local_point - point(0, 0, 0)
    }

    fn aabb(&self) -> Aabb {
        Aabb::new(-1.0, 1.0, -1.0, 1.0, -1.0, 1.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::approx_eq::ApproximateEq;
    use crate::matrix::Matrix;
    use crate::tuple::{point, vector};

    /// Intersect sets the object on intersection
    #[test]
    fn intersect_obj() {
        let r = Ray::new(point(0, 0, -5), vector(0, 0, 1));
        let s = Sphere::new();
        let dummy_shape = sphere();
        let xs = s.intersect(&dummy_shape, &r);
        assert_eq!(xs.len(), 2);
        assert_eq!(xs[0].obj as *const _, &dummy_shape as *const _,);
        assert_eq!(xs[1].obj as *const _, &dummy_shape as *const _,);
    }

    /// A ray intersects a sphere at two points
    #[test]
    fn intersect() {
        let r = Ray::new(point(0, 0, -5), vector(0, 0, 1));
        let s = Sphere::new();
        let dummy_shape = sphere();
        let xs = s.intersect(&dummy_shape, &r);
        assert_eq!(xs.len(), 2);
        assert_almost_eq!(xs[0].t, 4.0);
        assert_almost_eq!(xs[1].t, 6.0);
    }

    /// A ray intersects a sphere at a tangent
    #[test]
    fn tangent() {
        let r = Ray::new(point(0, 1, -5), vector(0, 0, 1));
        let s = Sphere::new();
        let dummy_shape = sphere();
        let xs = s.intersect(&dummy_shape, &r);
        assert_eq!(xs.len(), 2);
        assert_almost_eq!(xs[0].t, 5.0);
        assert_almost_eq!(xs[1].t, 5.0);
    }

    /// A ray misses a sphere
    #[test]
    fn miss() {
        let r = Ray::new(point(0, 2, -5), vector(0, 0, 1));
        let s = Sphere::new();
        let dummy_shape = sphere();
        let xs = s.intersect(&dummy_shape, &r);
        assert!(xs.is_empty(), 0);
    }

    /// A ray originates inside a sphere
    #[test]
    fn inside() {
        let r = Ray::new(point(0, 0, 0), vector(0, 0, 1));
        let s = Sphere::new();
        let dummy_shape = sphere();
        let xs = s.intersect(&dummy_shape, &r);
        assert_eq!(xs.len(), 2);
        assert_almost_eq!(xs[0].t, -1.0);
        assert_almost_eq!(xs[1].t, 1.0);
    }

    /// A sphere is behind a ray
    #[test]
    fn behind() {
        let r = Ray::new(point(0, 0, 5), vector(0, 0, 1));
        let s = Sphere::new();
        let dummy_shape = sphere();
        let xs = s.intersect(&dummy_shape, &r);
        assert_eq!(xs.len(), 2);
        assert_almost_eq!(xs[0].t, -6.0);
        assert_almost_eq!(xs[1].t, -4.0);
    }

    // The normal on a sphere at a point on the x axis
    #[test]
    fn normal_x() {
        let s = Sphere::new();
        let dummy_shape = sphere();
        let dummy_intersection = Intersection::new(0.0, &dummy_shape);
        let n = s.normal_at(point(1, 0, 0), &dummy_intersection);
        assert_almost_eq!(n, vector(1, 0, 0));
    }

    // The normal on a sphere at a point on the y axis
    #[test]
    fn normal_y() {
        let s = Sphere::new();
        let dummy_shape = sphere();
        let dummy_intersection = Intersection::new(0.0, &dummy_shape);
        let n = s.normal_at(point(0, 1, 0), &dummy_intersection);
        assert_almost_eq!(n, vector(0, 1, 0));
    }

    // The normal on a sphere at a point on the z axis
    #[test]
    fn normal_z() {
        let s = Sphere::new();
        let dummy_shape = sphere();
        let dummy_intersection = Intersection::new(0.0, &dummy_shape);
        let n = s.normal_at(point(0, 0, 1), &dummy_intersection);
        assert_almost_eq!(n, vector(0, 0, 1));
    }

    // The normal on a sphere at a nonaxial point
    #[test]
    fn normal_any() {
        let s3 = f64::sqrt(3.0) / 3.0;
        let s = Sphere::new();
        let dummy_shape = sphere();
        let dummy_intersection = Intersection::new(0.0, &dummy_shape);
        let n = s.normal_at(point(s3, s3, s3), &dummy_intersection);
        assert_almost_eq!(n, vector(s3, s3, s3));
    }

    // The normal is a normalized vector
    #[test]
    fn normal_is_normalized() {
        let s3 = f64::sqrt(3.0) / 3.0;
        let s = Sphere::new();
        let dummy_shape = sphere();
        let dummy_intersection = Intersection::new(0.0, &dummy_shape);
        let n = s.normal_at(point(s3, s3, s3), &dummy_intersection);
        assert_almost_eq!(n, n.normalized());
    }

    /// A helper for producing a sphere with a glassy material
    #[test]
    fn glassy_sphere() {
        let s = glass_sphere();
        let m = s.material().as_any().downcast_ref::<Phong>().unwrap();
        assert_almost_eq!(s.inv_transform(), Matrix::identity());
        assert_almost_eq!(m.transparency(), 1.0);
        assert_almost_eq!(m.refractive_index(), 1.5);
    }
}
