use crate::approx_eq::EPSILON;
use crate::math::Squared;
use crate::ray::{Intersection, Ray};
use crate::shapes::{Geometry, Shape};
use crate::tuple::{vector, Point, Vector};
use std::f64::INFINITY;

pub fn cylinder() -> Shape {
    Shape::new(Cylinder::default())
}

#[derive(Debug)]
pub struct Cylinder {
    minimum: f64,
    maximum: f64,
    closed: bool,
}

impl Default for Cylinder {
    fn default() -> Self {
        Cylinder::new(-INFINITY, INFINITY, false)
    }
}

impl Cylinder {
    pub fn new(minimum: f64, maximum: f64, closed: bool) -> Self {
        Cylinder {
            minimum,
            maximum,
            closed,
        }
    }

    fn intersect_caps<'a>(
        &self,
        obj: &'a Shape,
        ray: &Ray,
        mut xs: Vec<Intersection<'a>>,
    ) -> Vec<Intersection<'a>> {
        if self.closed && ray.direction().y().abs() > EPSILON {
            let t_min = (self.minimum - ray.origin().y()) / ray.direction().y();
            if check_cap(ray, t_min, 1.0) {
                xs.push(Intersection::new(t_min, obj));
            }

            let t_max = (self.maximum - ray.origin().y()) / ray.direction().y();
            if check_cap(ray, t_max, 1.0) {
                xs.push(Intersection::new(t_max, obj));
            }
        }
        xs.sort_unstable_by(|a, b| {
            a.t.partial_cmp(&b.t)
                .expect("Unable to compare intersection distances")
        });
        xs
    }
}

fn check_cap(ray: &Ray, t: f64, radius: f64) -> bool {
    let p = ray.position(t);
    p.x().squared() + p.z().squared() <= radius.squared()
}

impl Geometry for Cylinder {
    fn is_similar(&self, other: &dyn Geometry) -> bool {
        other.as_any().downcast_ref::<Cylinder>().is_some()
    }

    fn intersect<'a>(&self, obj: &'a Shape, ray: &Ray) -> Vec<Intersection<'a>> {
        let a = ray.direction().x().squared() + ray.direction().z().squared();
        let b = 2.0 * ray.origin().x() * ray.direction().x()
            + 2.0 * ray.origin().z() * ray.direction().z();
        let c = ray.origin().x().squared() + ray.origin().z().squared() - 1.0;

        if a.abs() < EPSILON {
            self.intersect_caps(obj, ray, intersections![])
        } else {
            let discriminant = b * b - 4.0 * a * c;
            if discriminant < 0.0 {
                intersections![]
            } else {
                let mut xs = vec![];

                let t0 = (b + discriminant.sqrt()) / (-2.0 * a);
                let y0 = ray.origin().y() + t0 * ray.direction().y();
                if y0 > self.minimum && y0 < self.maximum {
                    xs.push(Intersection::new(t0, obj));
                }

                let t1 = (b - discriminant.sqrt()) / (-2.0 * a);
                let y1 = ray.origin().y() + t1 * ray.direction().y();
                if y1 > self.minimum && y1 < self.maximum {
                    xs.push(Intersection::new(t1, obj));
                }

                self.intersect_caps(obj, ray, xs)
            }
        }
    }

    fn normal_at(&self, p: Point) -> Vector {
        let dist = p.x().squared() + p.z().squared();

        if dist < 1.0 {
            if p.y() >= self.maximum - EPSILON {
                return vector(0, 1, 0);
            } else if p.y() <= self.minimum + EPSILON {
                return vector(0, -1, 0);
            }
        }

        vector(p.x(), 0, p.z())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::approx_eq::ApproximateEq;
    use crate::tuple::{point, vector};

    /// The default cylinder
    #[test]
    fn defaults() {
        let geometry = Cylinder::default();
        assert_almost_eq!(geometry.minimum, -INFINITY);
        assert_almost_eq!(geometry.maximum, INFINITY);
        assert!(!geometry.closed);
    }

    /// A ray misses a cylinder
    #[test]
    fn miss() {
        for (p, d) in vec![
            (point(1, 0, 0), vector(0, 1, 0)),
            (point(0, 0, 0), vector(0, 1, 0)),
            (point(0, 0, -5), vector(1, 1, 1)),
        ] {
            let geometry = Cylinder::default();
            let r = Ray::new(p, d.normalized());
            let dummy_shape = cylinder();
            let xs = geometry.intersect(&dummy_shape, &r);
            assert_eq!(xs.len(), 0);
        }
    }

    /// A ray strikes a cylinder
    #[test]
    fn hit() {
        for (origin, direction, t0, t1) in vec![
            (point(1, 0, -5), vector(0, 0, 1), 5.0, 5.0),
            (point(0, 0, -5), vector(0, 0, 1), 4.0, 6.0),
            (point(0.5, 0, -5), vector(0.1, 1, 1), 6.80798, 7.08872),
        ] {
            let geometry = Cylinder::default();
            let r = Ray::new(origin, direction.normalized());
            let dummy_shape = cylinder();
            let xs = geometry.intersect(&dummy_shape, &r);
            assert_eq!(xs.len(), 2);
            assert_almost_eq!(xs[0].t, t0);
            assert_almost_eq!(xs[1].t, t1);
        }
    }

    /// Normal vector on a cylinder
    #[test]
    fn normal() {
        for (point, normal) in vec![
            (point(1, 0, 0), vector(1, 0, 0)),
            (point(0, 5, -1), vector(0, 0, -1)),
            (point(0, 2, -1), vector(0, 0, -1)),
            (point(-1, 1, 0), vector(-1, 0, 0)),
        ] {
            let geometry = Cylinder::default();
            assert_almost_eq!(geometry.normal_at(point), normal);
        }
    }

    /// Intersecting a constrained cylinder
    #[test]
    fn intersect_constrained() {
        for (origin, direction, count) in vec![
            (point(0, 1.5, 0), vector(0.1, 1, 0), 0),
            (point(0, 3, -5), vector(0, 0, 1), 0),
            (point(0, 0, -5), vector(0, 0, 1), 0),
            (point(0, 2, -5), vector(0, 0, 1), 0),
            (point(0, 1, -5), vector(0, 0, 1), 0),
            (point(0, 1.5, -2), vector(0, 0, 1), 2),
        ] {
            let geometry = Cylinder::new(1.0, 2.0, false);
            let r = Ray::new(origin, direction.normalized());
            let dummy_shape = cylinder();
            let xs = geometry.intersect(&dummy_shape, &r);
            assert_eq!(xs.len(), count);
        }
    }

    /// Intersecting a closed cylinder
    #[test]
    fn intersect_closed() {
        for (origin, direction, count) in vec![
            (point(0, 3, 0), vector(0, -1, 0), 2),
            (point(0, 3, -2), vector(0, -1, 2), 2),
            (point(0, 4, -2), vector(0, -1, 1), 2), // corner case
            (point(0, 0, -2), vector(0, 1, 2), 2),
            (point(0, -1, -2), vector(0, 1, 1), 2), // corner case
        ] {
            let geometry = Cylinder::new(1.0, 2.0, true);
            let r = Ray::new(origin, direction.normalized());
            let dummy_shape = cylinder();
            let xs = geometry.intersect(&dummy_shape, &r);
            assert_eq!(xs.len(), count);
        }
    }

    /// Normal vector on a cylinder's end caps
    #[test]
    fn normal_closed() {
        for (point, normal) in vec![
            (point(0, 1, 0), vector(0, -1, 0)),
            (point(0.5, 1, 0), vector(0, -1, 0)),
            (point(0, 1, 0.5), vector(0, -1, 0)),
            (point(0, 2, 0), vector(0, 1, 0)),
            (point(0.5, 2, 0), vector(0, 1, 0)),
            (point(0, 2, 0.5), vector(0, 1, 0)),
        ] {
            let geometry = Cylinder::new(1.0, 2.0, true);
            assert_almost_eq!(geometry.normal_at(point), normal);
        }
    }
}
