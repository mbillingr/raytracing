use crate::tuple::{Point, Vector};
use crate::shapes::{Shape, Geometry};
use crate::approx_eq::{ApproximateEq, EPSILON};
use crate::ray::{Ray, Intersection};
use crate::aabb::Aabb;

pub fn triangle(p1: Point, p2: Point, p3: Point) -> Shape {
    Shape::new(Triangle::new(p1, p2, p3))
}

#[derive(Debug, Clone)]
pub struct Triangle {
    p1: Point,
    p2: Point,
    p3: Point,
    e1: Vector,
    e2: Vector,
    normal: Vector,
}

impl Triangle {
    pub fn new(p1: Point, p2: Point, p3: Point) -> Self {
        let e1 = p2 - p1;
        let e2 = p3 - p1;
        Triangle {
            p1, p2, p3,
            e1,
            e2,
            normal: e2.cross(&e1).normalized()
        }
    }
}

impl Geometry for Triangle {
    fn duplicate(&self) -> Box<dyn Geometry> {
        Box::new(self.clone())
    }

    fn is_similar(&self, other: &dyn Geometry) -> bool {
        other.as_any().downcast_ref::<Triangle>().map(|o| self.approx_eq(o)).unwrap_or(false)
    }

    fn intersect<'a>(&self, obj: &'a Shape, local_ray: &Ray) -> Vec<Intersection<'a>> {
        let dir_cross_e2 = local_ray.direction().cross(&self.e2);
        let det = self.e1.dot(&dir_cross_e2);

        if det.abs() < EPSILON {
            return vec![]
        }

        let f = 1.0 / det;

        let p1_to_origin = local_ray.origin() - self.p1;
        let u = f * p1_to_origin.dot(&dir_cross_e2);

        if u < 0.0 || u > 1.0 {
            return vec![]
        }

        let origin_cross_e1 = p1_to_origin.cross(&self.e1);
        let v = f * local_ray.direction().dot(&origin_cross_e1);

        if v < 0.0 || (u + v) > 1.0 {
            return vec![]
        }

        let t = f * self.e2.dot(&origin_cross_e1);
        vec![Intersection::new(t, obj)]
    }

    fn normal_at(&self, _: Point) -> Vector {
        self.normal
    }

    fn aabb(&self) -> Aabb {
        Aabb::new(
            self.p1.x().min(self.p2.x()).min(self.p3.x()),
            self.p1.x().max(self.p2.x()).max(self.p3.x()),
            self.p1.y().min(self.p2.y()).min(self.p3.y()),
            self.p1.y().max(self.p2.y()).max(self.p3.y()),
            self.p1.z().min(self.p2.z()).min(self.p3.z()),
            self.p1.z().max(self.p2.z()).max(self.p3.z()),
        )
    }
}

impl ApproximateEq for Triangle {
    fn approx_eq(&self, other: &Triangle) -> bool {
        self.p1.approx_eq(&other.p1) && self.p2.approx_eq(&other.p2) && self.p3.approx_eq(&other.p3)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tuple::{point, vector};

    /// Constructing a triangle
    #[test]
    fn construct() {
        let p1 = point(0, 1, 0);
        let p2 = point(-1, 0, 0);
        let p3 = point(1, 0, 0);
        let t = Triangle::new(p1, p2, p3);
        assert_almost_eq!(t.p1, p1);
        assert_almost_eq!(t.p2, p2);
        assert_almost_eq!(t.p3, p3);
        assert_almost_eq!(t.e1, vector(-1, -1, 0));
        assert_almost_eq!(t.e2, vector(1, -1, 0));
        assert_almost_eq!(t.normal, vector(0, 0, -1));
    }

    /// Finding the normal on a triangle
    #[test]
    fn normal() {
        let t = Triangle::new(point(0, 1, 0), point(-1, 0, 0), point(1, 0, 0));
        let n1 = t.normal_at(point(0, 0.5, 0));
        let n2 = t.normal_at(point(-0.5, 0.75, 0));
        let n3 = t.normal_at(point(0.5, 0.25, 0));
        assert_almost_eq!(n1, t.normal);
        assert_almost_eq!(n2, t.normal);
        assert_almost_eq!(n3, t.normal);
    }

    /// Intersecting a ray parallel to the triangle
    #[test]
    fn intersect_miss() {
        let t = triangle(point(0, 1, 0), point(-1, 0, 0), point(1, 0, 0));
        let r = Ray::new(point(0, -1, -2), vector(0, 1, 0));
        let xs = t.local_intersect(&r);
        assert!(xs.is_empty());
    }

    /// A ray misses the p1-p3 edge
    #[test]
    fn intersect_miss13() {
        let t = triangle(point(0, 1, 0), point(-1, 0, 0), point(1, 0, 0));
        let r = Ray::new(point(1, 1, -2), vector(0, 0, 1));
        let xs = t.local_intersect(&r);
        assert!(xs.is_empty());
    }

    /// A ray misses the p1-p2 edge
    #[test]
    fn intersect_miss12() {
        let t = triangle(point(0, 1, 0), point(-1, 0, 0), point(1, 0, 0));
        let r = Ray::new(point(-1, 1, -2), vector(0, 0, 1));
        let xs = t.local_intersect(&r);
        assert!(xs.is_empty());
    }

    /// A ray misses the p2-p3 edge
    #[test]
    fn intersect_miss23() {
        let t = triangle(point(0, 1, 0), point(-1, 0, 0), point(1, 0, 0));
        let r = Ray::new(point(0, -1, -2), vector(0, 0, 1));
        let xs = t.local_intersect(&r);
        assert!(xs.is_empty());
    }

    /// A ray strikes the triangle
    #[test]
    fn intersect_hit() {
        let t = triangle(point(0, 1, 0), point(-1, 0, 0), point(1, 0, 0));
        let r = Ray::new(point(0, 0.5, -2), vector(0, 0, 1));
        let xs = t.local_intersect(&r);
        assert_eq!(xs.len(), 1);
        assert_almost_eq!(xs[0].t, 2.0);
    }
}
