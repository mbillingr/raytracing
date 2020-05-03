use crate::aabb::Aabb;
use crate::approx_eq::EPSILON;
use crate::ray::{Intersection, Ray};
use crate::shapes::{Geometry, Shape};
use crate::tuple::{vector, Point, Vector};
use std::sync::Arc;

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

#[derive(Clone)]
pub struct PlanarHeightmap {
    height_func: Arc<dyn Sync + Send + Fn(f64, f64) -> f64>,
    detail_scale: f64,
    aabb: Aabb,
}

impl PlanarHeightmap {
    pub fn new(func: impl 'static + Sync + Send + Fn(f64, f64) -> f64) -> Self {
        PlanarHeightmap {
            height_func: Arc::new(func),
            detail_scale: 0.1,
            aabb: Aabb::new(-1.0, 1.0, -1.0, 1.0, -1.0, 1.0),
        }
    }

    pub fn with_xrange(mut self, xmin: f64, xmax: f64) -> Self {
        self.aabb.min_p.set_x(xmin);
        self.aabb.max_p.set_x(xmax);
        self
    }

    pub fn with_yrange(mut self, ymin: f64, ymax: f64) -> Self {
        self.aabb.min_p.set_y(ymin);
        self.aabb.max_p.set_y(ymax);
        self
    }

    pub fn with_zrange(mut self, zmin: f64, zmax: f64) -> Self {
        self.aabb.min_p.set_z(zmin);
        self.aabb.max_p.set_z(zmax);
        self
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
    fn duplicate(&self) -> Box<dyn Geometry> {
        Box::new(self.clone())
    }

    fn is_similar(&self, other: &dyn Geometry) -> bool {
        other.as_any().downcast_ref::<PlanarHeightmap>().is_some()
    }

    fn intersect<'a>(&self, obj: &'a Shape, local_ray: &Ray) -> Vec<Intersection<'a>> {
        let (tmin, tmax) = match self.aabb.intersect(local_ray) {
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

    fn normal_at(&self, p: Point, _: &Intersection) -> Vector {
        if p.y() >= self.aabb.max_p.y() {
            return vector(0, 1, 0);
        }

        if p.y() <= self.aabb.min_p.y() {
            return vector(0, -1, 0);
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

    fn aabb(&self) -> Aabb {
        self.aabb.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::approx_eq::ApproximateEq;
    use crate::shapes::sphere;
    use crate::tuple::{point, vector};
    use std::f64::consts::FRAC_1_SQRT_2;

    /// The normal of a flat map
    #[test]
    fn normal() {
        let p = PlanarHeightmap::new(|_, _| 0.0);
        let dummy_shape = sphere();
        let dummy_intersection = Intersection::new(0.0, &dummy_shape);
        let n1 = p.normal_at(point(0, 0, 0), &dummy_intersection);
        let n2 = p.normal_at(point(10, 0, -10), &dummy_intersection);
        let n3 = p.normal_at(point(-5, 0, 150), &dummy_intersection);
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
