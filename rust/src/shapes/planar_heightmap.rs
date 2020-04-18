use crate::approx_eq::EPSILON;
use crate::ray::{Intersection, Ray};
use crate::shapes::{Geometry, Shape};
use crate::tuple::{point, vector, Point, Vector};

pub fn planar_heightmap(
    xmin: f64,
    xmax: f64,
    ymin: f64,
    ymax: f64,
    zmin: f64,
    zmax: f64,
    func: impl 'static + Sync + Send + Fn(f64, f64) -> f64,
) -> Shape {
    Shape::new(
        PlanarHeightmap::new(func)
            .with_xrange(xmin, xmax)
            .with_yrange(ymin, ymax)
            .with_zrange(zmin, zmax),
    )
}

pub struct PlanarHeightmap {
    height_func: Box<dyn Sync + Send + Fn(f64, f64) -> f64>,
    detail_scale: f64,
    min_bound: Point,
    max_bound: Point,
}

impl PlanarHeightmap {
    pub fn new(func: impl 'static + Sync + Send + Fn(f64, f64) -> f64) -> Self {
        PlanarHeightmap {
            height_func: Box::new(func),
            detail_scale: 0.1,
            min_bound: point(-1, -1, -1),
            max_bound: point(1, 1, 1),
        }
    }

    pub fn with_xrange(mut self, xmin: f64, xmax: f64) -> Self {
        self.min_bound.set_x(xmin);
        self.max_bound.set_x(xmax);
        self
    }

    pub fn with_yrange(mut self, ymin: f64, ymax: f64) -> Self {
        self.min_bound.set_y(ymin);
        self.max_bound.set_y(ymax);
        self
    }

    pub fn with_zrange(mut self, zmin: f64, zmax: f64) -> Self {
        self.min_bound.set_z(zmin);
        self.max_bound.set_z(zmax);
        self
    }

    fn intersect_aabb(&self, r: &Ray) -> Option<(f64, f64)> {
        let (xtmin, xtmax) = check_axis(
            r.origin().x(),
            r.direction().x(),
            self.min_bound.x(),
            self.max_bound.x(),
        )?;
        let (ytmin, ytmax) = check_axis(
            r.origin().y(),
            r.direction().y(),
            self.min_bound.y(),
            self.max_bound.y(),
        )?;
        let (ztmin, ztmax) = check_axis(
            r.origin().z(),
            r.direction().z(),
            self.min_bound.z(),
            self.max_bound.z(),
        )?;
        let tmin = xtmin.max(ytmin).max(ztmin);
        let tmax = xtmax.min(ytmax).min(ztmax);
        if tmin > tmax {
            None
        } else {
            Some((tmin, tmax))
        }
    }

    fn find_intersection(&self, r: &Ray, t1: f64, h1: f64, t2: f64, h2: f64) -> f64 {
        let t = (t1 + t2) / 2.0;
        if (t2 - t1).abs() < EPSILON {
            return t;
        }
        let p = r.position(t);
        let h = (self.height_func)(p.x(), p.z());
        if h > p.y() {
            self.find_intersection(r, t, h, t2, h2)
        } else {
            self.find_intersection(r, t1, h1, t, h)
        }
    }
}

impl std::fmt::Debug for PlanarHeightmap {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "PlanarHeightMap({:p})", self.height_func)
    }
}

impl Geometry for PlanarHeightmap {
    fn is_similar(&self, other: &dyn Geometry) -> bool {
        other.as_any().downcast_ref::<PlanarHeightmap>().is_some()
    }

    fn intersect<'a>(&self, obj: &'a Shape, local_ray: &Ray) -> Vec<Intersection<'a>> {
        let (tmin, tmax) = match self.intersect_aabb(local_ray) {
            None => return vec![],
            Some(range) => range,
        };

        let dt = (self.detail_scale
            / f64::max(local_ray.direction().x(), local_ray.direction().z()))
        .abs();

        let mut intersections = vec![];

        let mut t = tmin;
        let mut last_t = tmin;
        let p = local_ray.position(last_t);
        let mut last_h = (self.height_func)(p.x(), p.z());
        let mut last_inside = false;
        loop {
            let p = local_ray.position(t);
            let h = (self.height_func)(p.x(), p.z());
            let is_inside = h > p.y();

            match (is_inside, last_inside) {
                (true, false) => intersections.push(Intersection::new(
                    self.find_intersection(local_ray, t, h, last_t, last_h),
                    obj,
                )),
                (false, true) => intersections.push(Intersection::new(
                    self.find_intersection(local_ray, last_t, last_h, t, h),
                    obj,
                )),
                _ => {}
            }

            last_t = t;
            last_h = h;
            last_inside = is_inside;

            if t == tmax {
                break;
            }

            t += dt;

            if t > tmax {
                t = tmax;
            }
        }

        if last_inside {
            intersections.push(Intersection::new(tmax, obj));
        }

        intersections
    }

    fn normal_at(&self, p: Point) -> Vector {
        if p.y() >= self.max_bound.y() {
            return vector(0, 1, 0);
        }

        let dx = self.detail_scale * 0.01;
        let dz = self.detail_scale * 0.01;

        let y0 = (self.height_func)(p.x(), p.z());
        let yx = (self.height_func)(p.x() + dx, p.z());
        let yz = (self.height_func)(p.x(), p.z() + dz);

        let vx = vector(dx, yx - y0, 0);
        let vz = vector(0, yz - y0, dz);

        vz.cross(&vx).normalized()
    }
}

fn check_axis(origin: f64, direction: f64, min: f64, max: f64) -> Option<(f64, f64)> {
    if direction == 0.0 && (origin < min || origin > max) {
        return None;
    }
    let t_min = (min - origin) / direction;
    let t_max = (max - origin) / direction;
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
    use std::f64::consts::FRAC_1_SQRT_2;

    /// The normal of a flat map
    #[test]
    fn normal() {
        let p = PlanarHeightmap::new(|_, _| 0.0);
        let n1 = p.normal_at(point(0, 0, 0));
        let n2 = p.normal_at(point(10, 0, -10));
        let n3 = p.normal_at(point(-5, 0, 150));
        assert_almost_eq!(n1, vector(0, 1, 0));
        assert_almost_eq!(n2, vector(0, 1, 0));
        assert_almost_eq!(n3, vector(0, 1, 0));
    }

    /// Intersect with a ray parallel to the plane above the surface
    #[test]
    fn intersect_parallel_above() {
        //let dummy_shape = planar_heightmap();
        let p = planar_heightmap(-1.0, 1.0, -1.0, 1.0, -1.0, 1.0, |_, _| 0.0);
        let r = Ray::new(point(0, 0.5, 0), vector(0, 0, 1));
        let xs = p.intersect(&r);
        assert!(xs.is_empty());
    }

    /// Intersect with a ray parallel to the plane below the surface
    #[test]
    fn intersect_parallel_below() {
        let p = planar_heightmap(-1.0, 1.0, -1.0, 1.0, -1.0, 1.0, |_, _| 0.0);
        let r = Ray::new(point(0, -0.5, 0), vector(0, 0, 1));
        let xs = p.intersect(&r);
        assert_eq!(xs.len(), 2);
        assert_almost_eq!(xs[0].t, -1.0);
        assert_almost_eq!(xs[1].t, 1.0);
    }

    /// Intersect surface with a hole
    #[test]
    fn intersect_parallel_hole() {
        let p = planar_heightmap(-1.0, 1.0, -1.0, 1.0, -1.0, 1.0, |x, y| x * x + y * y);
        let r = Ray::new(point(0, 0.5, 0), vector(0, 0, 1));
        let xs = p.intersect(&r);
        assert_eq!(xs.len(), 4);
        assert_almost_eq!(xs[0].t, -1.0);
        assert_almost_eq!(xs[1].t, -FRAC_1_SQRT_2);
        assert_almost_eq!(xs[2].t, FRAC_1_SQRT_2);
        assert_almost_eq!(xs[3].t, 1.0);
    }

    /// Intersect with a vertical ray from above
    #[test]
    fn intersect_straight_above() {
        let p = planar_heightmap(-1.0, 1.0, -1.0, 1.0, -1.0, 1.0, |_, _| 0.0);
        let r = Ray::new(point(0, 2, 0), vector(0, -1, 0));
        let xs = p.intersect(&r);
        assert_eq!(xs.len(), 2);
        assert_almost_eq!(xs[0].t, 2.0);
        assert_almost_eq!(xs[1].t, 3.0);
    }

    /// Intersect with a vertical ray from below
    #[test]
    fn intersect_straight_below() {
        let p = planar_heightmap(-1.0, 1.0, -1.0, 1.0, -1.0, 1.0, |_, _| 0.0);
        let r = Ray::new(point(0, -2, 0), vector(0, 1, 0));
        let xs = p.intersect(&r);
        assert_eq!(xs.len(), 2);
        assert_almost_eq!(xs[0].t, 1.0);
        assert_almost_eq!(xs[1].t, 2.0);
    }
}
