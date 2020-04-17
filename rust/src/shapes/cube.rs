use crate::ray::{Intersection, Ray};
use crate::shapes::{Geometry, Shape};
use crate::tuple::{vector, Point, Vector};

pub fn cube() -> Shape {
    Shape::new(Cube::new())
}

#[derive(Debug)]
pub struct Cube;

impl Cube {
    pub fn new() -> Self {
        Cube
    }
}

impl Geometry for Cube {
    fn is_similar(&self, other: &dyn Geometry) -> bool {
        other.as_any().downcast_ref::<Cube>().is_some()
    }

    fn intersect<'a>(&self, obj: &'a Shape, local_ray: &Ray) -> Vec<Intersection<'a>> {
        check_axis(local_ray.origin().x(), local_ray.direction().x())
            .and_then(|(xtmin, xtmax)| {
                check_axis(local_ray.origin().y(), local_ray.direction().y())
                    .map(|(ytmin, ytmax)| (xtmin.max(ytmin), xtmax.min(ytmax)))
            })
            .and_then(|(tmin, tmax)| {
                check_axis(local_ray.origin().z(), local_ray.direction().z())
                    .map(|(ztmin, ztmax)| (tmin.max(ztmin), tmax.min(ztmax)))
            })
            .filter(|(tmin, tmax)| tmin <= tmax)
            .map(|(tmin, tmax)| vec![Intersection::new(tmin, obj), Intersection::new(tmax, obj)])
            .unwrap_or(vec![])
    }

    fn normal_at(&self, local_point: Point) -> Vector {
        let (ax, ay, az) = (
            local_point.x().abs(),
            local_point.y().abs(),
            local_point.z().abs(),
        );
        if ax >= ay {
            if ax >= az {
                vector(local_point.x(), 0, 0)
            } else {
                vector(0, 0, local_point.z())
            }
        } else {
            if ay >= az {
                vector(0, local_point.y(), 0)
            } else {
                vector(0, 0, local_point.z())
            }
        }
    }
}

fn check_axis(origin: f64, direction: f64) -> Option<(f64, f64)> {
    if direction == 0.0 && (origin < -1.0 || origin > 1.0) {
        return None;
    }
    let t_min = (-1.0 - origin) / direction;
    let t_max = (1.0 - origin) / direction;
    if t_min > t_max {
        Some((t_max, t_min))
    } else {
        Some((t_min, t_max))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::approx_eq::ApproximateEq;
    use crate::tuple::{point, vector};

    /// A ray intersects a cube
    #[test]
    fn intersect() {
        for (origin, direction, t1, t2) in vec![
            (point(5, 0.5, 0), vector(-1, 0, 0), 4.0, 6.0),
            (point(-5, 0.5, 0), vector(1, 0, 0), 4.0, 6.0),
            (point(0.5, 5, 0), vector(0, -1, 0), 4.0, 6.0),
            (point(0.5, -5, 0), vector(0, 1, 0), 4.0, 6.0),
            (point(0.5, 0, 5), vector(0, 0, -1), 4.0, 6.0),
            (point(0.5, 0, -5), vector(0, 0, 1), 4.0, 6.0),
            (point(0, 0.5, 0), vector(0, 0, 1), -1.0, 1.0),
        ] {
            let c = cube();
            let r = Ray::new(origin, direction);
            let xs = c.intersect(&r);
            assert_eq!(xs.len(), 2);
            assert_almost_eq!(xs[0].t, t1);
            assert_almost_eq!(xs[1].t, t2);
        }
    }

    /// A ray misses a cube
    #[test]
    fn miss() {
        for (origin, direction) in vec![
            (point(-2, 0, 0), vector(0.2673, 0.5345, 0.8018)),
            (point(0, -2, 0), vector(0.8018, 0.2673, 0.5345)),
            (point(0, 0, -2), vector(0.5345, 0.8018, 0.2673)),
            (point(2, 0, 2), vector(0, 0, -1)),
            (point(0, 2, 2), vector(0, -1, 0)),
            (point(2, 2, 0), vector(-1, 0, 0)),
        ] {
            let c = cube();
            let r = Ray::new(origin, direction);
            let xs = c.intersect(&r);
            assert!(xs.is_empty());
        }
    }

    /// Normals on the surfaces of a cube
    #[test]
    fn normals() {
        assert_almost_eq!(cube().normal_at(point(1.0, 0.5, -0.8)), vector(1, 0, 0));
        assert_almost_eq!(cube().normal_at(point(-1.0, -0.2, 0.9)), vector(-1, 0, 0));
        assert_almost_eq!(cube().normal_at(point(-0.4, 1.0, -0.1)), vector(0, 1, 0));
        assert_almost_eq!(cube().normal_at(point(0.3, -1.0, -0.7)), vector(0, -1, 0));
        assert_almost_eq!(cube().normal_at(point(-0.6, 0.3, 1.0)), vector(0, 0, 1));
        assert_almost_eq!(cube().normal_at(point(0.4, 0.4, -1.0)), vector(0, 0, -1));
        assert_almost_eq!(cube().normal_at(point(1, 1, 1)), vector(1, 0, 0));
        assert_almost_eq!(cube().normal_at(point(-1, -1, -1)), vector(-1, 0, 0));
    }
}
