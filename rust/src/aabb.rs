use crate::matrix::Matrix;
use crate::ray::Ray;
use crate::tuple::{Point, Vector};
use std::f64::INFINITY;

#[derive(Debug, Clone)]
pub struct Aabb {
    pub(crate) min_p: Point,
    pub(crate) max_p: Point,
}

impl Default for Aabb {
    fn default() -> Self {
        Aabb::empty()
    }
}

impl Aabb {
    pub fn new(xmin: f64, xmax: f64, ymin: f64, ymax: f64, zmin: f64, zmax: f64) -> Self {
        Aabb {
            min_p: Point::new(xmin, ymin, zmin),
            max_p: Point::new(xmax, ymax, zmax),
        }
    }

    pub fn empty() -> Self {
        Aabb {
            min_p: Point::new(INFINITY, INFINITY, INFINITY),
            max_p: Point::new(-INFINITY, -INFINITY, -INFINITY),
        }
    }

    pub fn empty_at(p: Point) -> Self {
        Aabb { min_p: p, max_p: p }
    }

    pub fn center(&self) -> Point {
        self.min_p + (self.max_p - self.min_p) / 2
    }

    pub fn size(&self) -> Vector {
        self.max_p - self.min_p
    }

    pub fn intersect(&self, r: &Ray) -> Option<(f64, f64)> {
        check_axis(
            self.min_p.x(),
            self.max_p.x(),
            r.origin().x(),
            r.direction().x(),
        )
        .and_then(|(xtmin, xtmax)| {
            check_axis(
                self.min_p.y(),
                self.max_p.y(),
                r.origin().y(),
                r.direction().y(),
            )
            .and_then(|(ytmin, ytmax)| {
                check_axis(
                    self.min_p.z(),
                    self.max_p.z(),
                    r.origin().z(),
                    r.direction().z(),
                )
                .map(|(ztmin, ztmax)| (xtmin.max(ytmin).max(ztmin), xtmax.min(ytmax).min(ztmax)))
                .filter(|(tmin, tmax)| tmin <= tmax)
            })
        })
    }

    pub fn merge(&self, other: &Self) -> Self {
        Aabb::new(
            f64::min(self.min_p.x(), other.min_p.x()),
            f64::max(self.max_p.x(), other.max_p.x()),
            f64::min(self.min_p.y(), other.min_p.y()),
            f64::max(self.max_p.y(), other.max_p.y()),
            f64::min(self.min_p.z(), other.min_p.z()),
            f64::max(self.max_p.z(), other.max_p.z()),
        )
    }

    pub fn extend(&self, p: Point) -> Self {
        Aabb::new(
            f64::min(self.min_p.x(), p.x()),
            f64::max(self.max_p.x(), p.x()),
            f64::min(self.min_p.y(), p.y()),
            f64::max(self.max_p.y(), p.y()),
            f64::min(self.min_p.z(), p.z()),
            f64::max(self.max_p.z(), p.z()),
        )
    }

    pub fn transform(&self, t: Matrix) -> Self {
        Aabb::empty_at(t * self.min_p)
            .extend(t * Point::new(self.min_p.x(), self.min_p.y(), self.max_p.z()))
            .extend(t * Point::new(self.min_p.x(), self.max_p.y(), self.min_p.z()))
            .extend(t * Point::new(self.min_p.x(), self.max_p.y(), self.max_p.z()))
            .extend(t * Point::new(self.max_p.x(), self.min_p.y(), self.min_p.z()))
            .extend(t * Point::new(self.max_p.x(), self.min_p.y(), self.max_p.z()))
            .extend(t * Point::new(self.max_p.x(), self.max_p.y(), self.min_p.z()))
            .extend(t * self.max_p)
    }

    pub fn split_x(self, x: f32) -> (Self, Self) {
        let mut left = self.clone();
        left.max_p.set_x(x as f64);
        let mut right = self;
        right.min_p.set_x(x as f64);
        (left, right)
    }

    pub fn split_y(self, y: f32) -> (Self, Self) {
        let mut left = self.clone();
        left.max_p.set_y(y as f64);
        let mut right = self;
        right.min_p.set_y(y as f64);
        (left, right)
    }

    pub fn split_z(self, z: f32) -> (Self, Self) {
        let mut left = self.clone();
        left.max_p.set_z(z as f64);
        let mut right = self;
        right.min_p.set_z(z as f64);
        (left, right)
    }
}

fn check_axis(min: f64, max: f64, origin: f64, direction: f64) -> Option<(f64, f64)> {
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
