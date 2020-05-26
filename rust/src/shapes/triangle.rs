use crate::aabb::Aabb;
use crate::approx_eq::{ApproximateEq, EPSILON};
use crate::ray::{Intersection, Ray};
use crate::shapes::{Geometry, Shape};
use crate::tuple::{Point, Vector};
use std::any::Any;

pub fn triangle(p1: Point, p2: Point, p3: Point) -> Shape {
    Shape::new(Triangle::new(p1, p2, p3))
}

pub fn smooth_triangle(
    p1: Point,
    p2: Point,
    p3: Point,
    n1: Vector,
    n2: Vector,
    n3: Vector,
) -> Shape {
    Shape::new(SmoothTriangle::new(p1, p2, p3, n1, n2, n3))
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
            p1,
            p2,
            p3,
            e1,
            e2,
            normal: e2.cross(&e1).normalized(),
        }
    }
}

impl Geometry for Triangle {
    fn duplicate(&self) -> Box<dyn Geometry> {
        Box::new(self.clone())
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn is_similar(&self, other: &dyn Geometry) -> bool {
        other
            .as_any()
            .downcast_ref::<Triangle>()
            .map(|o| self.approx_eq(o))
            .unwrap_or(false)
    }

    fn intersect<'a>(&self, obj: &'a Shape, local_ray: &Ray) -> Vec<Intersection<'a>> {
        intersect_triangle(self.p1, self.e1, self.e2, obj, local_ray)
    }

    fn normal_at(&self, _: Point, _: &Intersection) -> Vector {
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

#[derive(Debug, Clone)]
pub struct SmoothTriangle {
    p1: Point,
    p2: Point,
    p3: Point,
    e1: Vector,
    e2: Vector,
    n1: Vector,
    n2: Vector,
    n3: Vector,
}

impl SmoothTriangle {
    pub fn new(p1: Point, p2: Point, p3: Point, n1: Vector, n2: Vector, n3: Vector) -> Self {
        let e1 = p2 - p1;
        let e2 = p3 - p1;
        SmoothTriangle {
            p1,
            p2,
            p3,
            e1,
            e2,
            n1,
            n2,
            n3,
        }
    }
}

impl Geometry for SmoothTriangle {
    fn duplicate(&self) -> Box<dyn Geometry> {
        Box::new(self.clone())
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn is_similar(&self, other: &dyn Geometry) -> bool {
        other
            .as_any()
            .downcast_ref::<SmoothTriangle>()
            .map(|o| self.approx_eq(o))
            .unwrap_or(false)
    }

    fn intersect<'a>(&self, obj: &'a Shape, local_ray: &Ray) -> Vec<Intersection<'a>> {
        intersect_triangle(self.p1, self.e1, self.e2, obj, local_ray)
    }

    fn normal_at(&self, _: Point, hit: &Intersection) -> Vector {
        (self.n2 * hit.u + self.n3 * hit.v + self.n1 * (1.0 - hit.u - hit.v)).normalized()
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

impl ApproximateEq for SmoothTriangle {
    fn approx_eq(&self, other: &SmoothTriangle) -> bool {
        self.p1.approx_eq(&other.p1) && self.p2.approx_eq(&other.p2) && self.p3.approx_eq(&other.p3)
    }
}

#[derive(Debug, Clone)]
pub struct TriangleMesh {
    faces: Vec<Triangle>,
}

impl TriangleMesh {
    pub fn new() -> Self {
        TriangleMesh { faces: vec![] }
    }

    pub fn from_mesh(vertices: Vec<Point>, triangles: Vec<(usize, usize, usize)>) -> Self {
        let mut faces = vec![];
        for (a, b, c) in triangles {
            faces.push(Triangle::new(vertices[a], vertices[b], vertices[c]));
        }
        TriangleMesh { faces }
    }

    pub fn aint_empty(&self) -> bool {
        !self.faces.is_empty()
    }

    pub fn add_triangle(&mut self, tri: Triangle) {
        self.faces.push(tri)
    }

    pub fn get_triangle(&self, idx: usize) -> Triangle {
        self.faces[idx].clone()
    }
}

impl Geometry for TriangleMesh {
    fn duplicate(&self) -> Box<dyn Geometry> {
        Box::new(self.clone())
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn is_similar(&self, _: &dyn Geometry) -> bool {
        unimplemented!()
    }

    fn intersect<'a>(&self, obj: &'a Shape, local_ray: &Ray) -> Vec<Intersection<'a>> {
        self.faces
            .iter()
            .enumerate()
            .map(|(i, tri)| {
                intersect_triangle(tri.p1, tri.e1, tri.e2, obj, local_ray)
                    .into_iter()
                    .map(move |x| x.with_i(i))
            })
            .flatten()
            .collect()
    }

    fn normal_at(&self, p: Point, hit: &Intersection) -> Vector {
        self.faces[hit.i].normal_at(p, hit)
    }

    fn aabb(&self) -> Aabb {
        let mut aabb = self.faces[0].aabb();
        for tri in &self.faces[1..] {
            aabb = aabb.merge(&tri.aabb())
        }
        aabb
    }
}

fn intersect_triangle<'a>(
    p1: Point,
    e1: Vector,
    e2: Vector,
    obj: &'a Shape,
    ray: &Ray,
) -> Vec<Intersection<'a>> {
    let dir_cross_e2 = ray.direction().cross(&e2);
    let det = e1.dot(&dir_cross_e2);

    if det.abs() < EPSILON {
        return vec![];
    }

    let f = 1.0 / det;

    let p1_to_origin = ray.origin() - p1;
    let u = f * p1_to_origin.dot(&dir_cross_e2);

    if u < 0.0 || u > 1.0 {
        return vec![];
    }

    let origin_cross_e1 = p1_to_origin.cross(&e1);
    let v = f * ray.direction().dot(&origin_cross_e1);

    if v < 0.0 || (u + v) > 1.0 {
        return vec![];
    }

    let t = f * e2.dot(&origin_cross_e1);
    vec![Intersection::new_uv(t, u, v, obj)]
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::shapes::sphere;
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
        let dummy_shape = sphere();
        let dummy_intersection = Intersection::new(0.0, &dummy_shape);
        let n1 = t.normal_at(point(0, 0.5, 0), &dummy_intersection);
        let n2 = t.normal_at(point(-0.5, 0.75, 0), &dummy_intersection);
        let n3 = t.normal_at(point(0.5, 0.25, 0), &dummy_intersection);
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

    /// Constructing a smooth triangle
    #[test]
    fn smooth_triangle() {
        let p1 = point(0, 1, 0);
        let p2 = point(-1, 0, 0);
        let p3 = point(1, 0, 0);
        let n1 = vector(0, 1, 0);
        let n2 = vector(-1, 0, 0);
        let n3 = vector(1, 0, 0);
        let tri = SmoothTriangle::new(p1, p2, p3, n1, n2, n3);
        assert_almost_eq!(tri.p1, p1);
        assert_almost_eq!(tri.p2, p2);
        assert_almost_eq!(tri.p3, p3);
        assert_almost_eq!(tri.n1, n1);
        assert_almost_eq!(tri.n2, n2);
        assert_almost_eq!(tri.n3, n3);
    }

    /// An intersection with smooth triangles stores u and v
    #[test]
    fn intersect_uv() {
        let p1 = point(0, 1, 0);
        let p2 = point(-1, 0, 0);
        let p3 = point(1, 0, 0);
        let n1 = vector(0, 1, 0);
        let n2 = vector(-1, 0, 0);
        let n3 = vector(1, 0, 0);
        let tri = SmoothTriangle::new(p1, p2, p3, n1, n2, n3);
        let dummy_shape = sphere();
        let r = Ray::new(point(-0.2, 0.3, -2), vector(0, 0, 1));
        let xs = tri.intersect(&dummy_shape, &r);
        assert_almost_eq!(xs[0].u, 0.45);
        assert_almost_eq!(xs[0].v, 0.25);
    }

    /// A smooth triangle uses u/v to interpolate the normal
    #[test]
    fn interpolate_normal() {
        let p1 = point(0, 1, 0);
        let p2 = point(-1, 0, 0);
        let p3 = point(1, 0, 0);
        let n1 = vector(0, 1, 0);
        let n2 = vector(-1, 0, 0);
        let n3 = vector(1, 0, 0);
        let tri = SmoothTriangle::new(p1, p2, p3, n1, n2, n3);
        let dummy_shape = sphere();
        let i = Intersection::new_uv(1.0, 0.45, 0.25, &dummy_shape);
        let n = tri.normal_at(point(0, 0, 0), &i);
        assert_almost_eq!(n, vector(-0.5547, 0.83205, 0));
    }

    /// Preparing the normal on a smooth triangle
    #[test]
    fn prepare_normal() {
        let p1 = point(0, 1, 0);
        let p2 = point(-1, 0, 0);
        let p3 = point(1, 0, 0);
        let n1 = vector(0, 1, 0);
        let n2 = vector(-1, 0, 0);
        let n3 = vector(1, 0, 0);
        let tri = SmoothTriangle::new(p1, p2, p3, n1, n2, n3);
        let shape = Shape::new(tri);
        let i = Intersection::new_uv(1.0, 0.45, 0.25, &shape);
        let r = Ray::new(point(-0.2, 0.3, -2), vector(0, 0, 1));
        let xs = [i];
        let comps = i.prepare_computations(&r, &xs);
        assert_almost_eq!(comps.normalv, vector(-0.5547, 0.83205, 0));
    }
}
