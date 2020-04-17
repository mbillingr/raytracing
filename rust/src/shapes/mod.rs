mod cube;
mod plane;
mod sphere;

pub use cube::cube;
pub use plane::plane;
pub use sphere::{glass_sphere, sphere};

use crate::approx_eq::ApproximateEq;
use crate::color::Color;
use crate::materials::Phong;
use crate::matrix::Matrix;
use crate::pattern::Pattern;
use crate::ray::{Intersection, Ray};
use crate::tuple::{Point, Vector};
use std::any::Any;

pub trait Geometry: 'static + AsAny + std::fmt::Debug + Sync {
    //fn as_any(&self) -> &dyn Any;// { self }
    fn is_similar(&self, other: &dyn Geometry) -> bool;
    fn intersect<'a>(&self, obj: &'a Shape, local_ray: &Ray) -> Vec<Intersection<'a>>;
    fn normal_at(&self, local_point: Point) -> Vector;
}

#[derive(Debug)]
pub struct Shape {
    material: Phong,
    transform: Matrix,
    inv_transform: Matrix,
    cast_shadow: bool,
    geometry: Box<dyn Geometry>,
}

impl Shape {
    pub fn new(geometry: impl Geometry) -> Self {
        Shape {
            material: Phong::default(),
            transform: Matrix::identity(),
            inv_transform: Matrix::identity(),
            cast_shadow: true,
            geometry: Box::new(geometry),
        }
    }

    pub fn intersect(&self, world_ray: &Ray) -> Vec<Intersection> {
        self.geometry
            .intersect(&self, &world_ray.transform(*self.inv_transform()))
    }

    pub fn normal_at(&self, world_point: Point) -> Vector {
        let obj_point = *self.inv_transform() * world_point;
        let obj_normal = self.geometry.normal_at(obj_point);
        let world_normal = self.inv_transform().transpose() * obj_normal;
        Vector::new(world_normal.x(), world_normal.y(), world_normal.z()).normalized()
    }

    pub fn pattern_at(&self, pattern: &Pattern, world_point: Point) -> Color {
        pattern.at(*self.inv_transform() * world_point)
    }

    pub fn with_material(self, material: Phong) -> Self {
        Shape { material, ..self }
    }

    pub fn with_transform(self, transform: Matrix) -> Self {
        Shape {
            transform,
            inv_transform: transform.inverse(),
            ..self
        }
    }

    pub fn material(&self) -> &Phong {
        &self.material
    }

    pub fn material_mut(&mut self) -> &mut Phong {
        &mut self.material
    }

    pub fn set_material(&mut self, material: Phong) {
        self.material = material;
    }

    pub fn inv_transform(&self) -> &Matrix {
        &self.inv_transform
    }

    pub fn set_transform(&mut self, t: Matrix) {
        self.transform = t;
        self.inv_transform = t.inverse();
    }

    pub fn cast_shadow(&self) -> bool {
        self.cast_shadow
    }

    pub fn set_cast_shadow(&mut self, b: bool) {
        self.cast_shadow = b
    }

    pub fn with_cast_shadow(self, cast_shadow: bool) -> Self {
        Shape {
            cast_shadow,
            ..self
        }
    }

    pub fn is_similar(&self, other: &Self) -> bool {
        self.geometry.is_similar(&*other.geometry)
            && self.material.approx_eq(&other.material)
            && self.transform.approx_eq(&other.transform)
    }
}

/*impl PartialEq for dyn Shape + '_ {
    fn eq(&self, other: &Self) -> bool {
        self as *const _ == other as *const _
    }
}*/

pub trait AsAny: Any {
    fn as_any(&self) -> &dyn Any;
}

impl<T: Any> AsAny for T {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::approx_eq::ApproximateEq;
    use crate::matrix::{rotation_x, rotation_y, rotation_z, scaling, translation};
    use crate::tuple::{point, vector};
    use std::f64::consts::{FRAC_1_SQRT_2, PI};
    use std::sync::RwLock;

    fn test_shape() -> Shape {
        Shape::new(TestGeometry::new())
    }

    #[derive(Debug)]
    struct TestGeometry {
        last_ray: RwLock<Option<Ray>>,
    }

    impl TestGeometry {
        fn new() -> Self {
            TestGeometry {
                last_ray: RwLock::new(None),
            }
        }
    }

    impl Geometry for TestGeometry {
        fn is_similar(&self, _: &dyn Geometry) -> bool {
            unimplemented!()
        }

        fn intersect<'a>(&self, _: &'a Shape, local_ray: &Ray) -> Vec<Intersection<'a>> {
            *self.last_ray.write().unwrap() = Some(local_ray.clone());
            vec![]
        }

        fn normal_at(&self, obj_point: Point) -> Vector {
            vector(obj_point.x(), obj_point.y(), obj_point.z())
        }
    }

    /// Intersecting a scaled shape with a ray
    #[test]
    fn intersect_scaled() {
        let r = Ray::new(point(0, 0, -5), vector(0, 0, 1));
        let mut s = test_shape();
        s.set_transform(scaling(2, 2, 2));
        s.intersect(&r);

        let g = (*s.geometry)
            .as_any()
            .downcast_ref::<TestGeometry>()
            .unwrap();
        assert_almost_eq!(
            g.last_ray.read().unwrap().as_ref().unwrap().origin(),
            point(0, 0, -2.5)
        );
        assert_almost_eq!(
            g.last_ray.read().unwrap().as_ref().unwrap().direction(),
            vector(0, 0, 0.5)
        );
    }

    /// Intersecting a translated shape with a ray
    #[test]
    fn intersect_translated() {
        let r = Ray::new(point(0, 0, -5), vector(0, 0, 1));
        let mut s = test_shape();
        s.set_transform(translation(5, 0, 0));
        s.intersect(&r);

        let g = (*s.geometry)
            .as_any()
            .downcast_ref::<TestGeometry>()
            .unwrap();
        assert_almost_eq!(
            g.last_ray.read().unwrap().as_ref().unwrap().origin(),
            point(-5, 0, -5)
        );
        assert_almost_eq!(
            g.last_ray.read().unwrap().as_ref().unwrap().direction(),
            vector(0, 0, 1)
        );
    }

    /// Intersecting a rotated shape with a ray
    #[test]
    fn intersect_rotated() {
        let r = Ray::new(point(0, 0, -5), vector(0, 0, 1));
        let mut s = test_shape();
        s.set_transform(rotation_x(1) * rotation_y(2) * rotation_z(3));
        s.intersect(&r);

        let g = (*s.geometry)
            .as_any()
            .downcast_ref::<TestGeometry>()
            .unwrap();
        assert_almost_eq!(
            g.last_ray.read().unwrap().as_ref().unwrap().origin(),
            point(-3.02564, 3.81859, 1.12423)
        );
        assert_almost_eq!(
            g.last_ray.read().unwrap().as_ref().unwrap().direction(),
            vector(0.60513, -0.76372, -0.22485)
        );
    }

    /// Computing the normal on a translated shape
    #[test]
    fn normal_translated() {
        let mut s = test_shape();
        s.set_transform(translation(0, 1, 0));
        let n = s.normal_at(point(0, 1.70711, 0.70711));
        assert_almost_eq!(n, vector(0, 0.70711, 0.70711));
    }

    /// Computing the normal on a transformed shape
    #[test]
    fn normal_transformed() {
        let mut s = test_shape();
        s.set_transform(scaling(1, 0.5, 1) * rotation_z(PI / 5.0));
        let n = s.normal_at(point(0, FRAC_1_SQRT_2, -FRAC_1_SQRT_2));
        assert_almost_eq!(n, vector(0, 0.97014, -0.24254));
    }
}
