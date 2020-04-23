use crate::approx_eq::EPSILON;
use crate::math::Squared;
use crate::ray::{Intersection, Ray};
use crate::shapes::{Geometry, Shape};
use crate::tuple::{vector, Point, Vector};
use std::f64::INFINITY;

pub fn cone() -> Shape {
    Shape::new(Cone::default())
}

#[derive(Debug)]
pub struct Cone {
    minimum: f64,
    maximum: f64,
    closed: bool,
}

impl Default for Cone {
    fn default() -> Self {
        Cone::new(-INFINITY, INFINITY, false)
    }
}

impl Cone {
    pub fn new(minimum: f64, maximum: f64, closed: bool) -> Self {
        Cone {
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
            if check_cap(ray, t_min, self.minimum) {
                xs.push(Intersection::new(t_min, obj));
            }

            let t_max = (self.maximum - ray.origin().y()) / ray.direction().y();
            if check_cap(ray, t_max, self.maximum) {
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

impl Geometry for Cone {
    fn is_similar(&self, other: &dyn Geometry) -> bool {
        other.as_any().downcast_ref::<Cone>().is_some()
    }

    fn intersect<'a>(&self, obj: &'a Shape, ray: &Ray) -> Vec<Intersection<'a>> {
        let a = ray.direction().x().squared() - ray.direction().y().squared()
            + ray.direction().z().squared();
        let b = 2.0
            * (ray.origin().x() * ray.direction().x() - ray.origin().y() * ray.direction().y()
                + ray.origin().z() * ray.direction().z());
        let c =
            ray.origin().x().squared() - ray.origin().y().squared() + ray.origin().z().squared();

        if a.abs() < EPSILON {
            if b.abs() < EPSILON {
                self.intersect_caps(obj, ray, intersections![])
            } else {
                self.intersect_caps(
                    obj,
                    ray,
                    intersections![Intersection::new(c / (-2.0 * b), obj)],
                )
            }
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

        vector(
            p.x(),
            if p.y() < 0.0 {
                dist.sqrt()
            } else {
                -dist.sqrt()
            },
            p.z(),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::approx_eq::ApproximateEq;
    use crate::tuple::{point, vector};
    use std::f64::consts::SQRT_2;

    /// Intersecting a cone with a ray
    #[test]
    fn hit() {
        for (origin, direction, t0, t1) in vec![
            (point(0, 0, -5), vector(0, 0, 1), 5.0, 5.0),
            (point(0, 0, -5), vector(1, 1, 1), 8.66025, 8.66025),
            (point(1, 1, -5), vector(-0.5, -1, 1), 4.55006, 49.44994),
        ] {
            let geometry = Cone::default();
            let r = Ray::new(origin, direction.normalized());
            let dummy_shape = cone();
            let xs = geometry.intersect(&dummy_shape, &r);
            assert_eq!(xs.len(), 2);
            assert_almost_eq!(xs[0].t, t0);
            assert_almost_eq!(xs[1].t, t1);
        }
    }

    /// Intersecting a cone with a ray parallel to one of its halves
    #[test]
    fn hit_parallel() {
        let geometry = Cone::default();
        let r = Ray::new(point(0, 0, -1), vector(0, 1, 1).normalized());
        let dummy_shape = cone();
        let xs = geometry.intersect(&dummy_shape, &r);
        assert_eq!(xs.len(), 1);
        assert_almost_eq!(xs[0].t, 0.35355);
    }

    /// Intersecting a cone's end caps
    #[test]
    fn hit_caps() {
        for (origin, direction, count) in vec![
            (point(0, 0, -5), vector(0, 1, 0), 0),
            (point(0, 0, -0.25), vector(0, 1, 1), 2),
            (point(0, 0, -0.25), vector(0, 1, 0), 4),
        ] {
            let geometry = Cone::new(-0.5, 0.5, true);
            let r = Ray::new(origin, direction.normalized());
            let dummy_shape = cone();
            let xs = geometry.intersect(&dummy_shape, &r);
            assert_eq!(xs.len(), count);
        }
    }

    /// Normal vector on a cone
    #[test]
    fn normals() {
        for (point, normal) in vec![
            (point(0, 0, 0), vector(0, 0, 0)),
            (point(1, 1, 1), vector(1, -SQRT_2, 1)),
            (point(-1, -1, 0), vector(-1, 1, 0)),
        ] {
            let geometry = Cone::default();
            assert_almost_eq!(geometry.normal_at(point), normal);
        }
    }
}
