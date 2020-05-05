use crate::approx_eq::ApproximateEq;
use crate::color::Color;
use crate::tuple::{vector, Point, Vector};
use rand::distributions::Distribution;
use rand::thread_rng;
use rand_distr::UnitSphere;
use std::any::Any;

pub trait Light: Sync + 'static {
    fn as_any(&self) -> &dyn Any;
    fn is_similar(&self, other: &dyn Light) -> bool;
    fn incoming_at(&self, point: Point) -> IncomingLight;
}

#[derive(Debug, Copy, Clone)]
pub enum IncomingLight {
    Ray(LightRay),
    Omni(Color),
}

#[derive(Debug, Copy, Clone)]
pub struct LightRay {
    pub origin: Point,
    pub direction: Vector,
    pub color: Color,
}

#[derive(Debug)]
pub struct PointLight {
    position: Point,
    intensity: Color,
}

impl PointLight {
    pub fn new(position: Point, intensity: Color) -> Self {
        PointLight {
            position,
            intensity,
        }
    }

    pub fn position(&self) -> Point {
        self.position
    }

    pub fn intensity(&self) -> Color {
        self.intensity
    }
}

impl Light for PointLight {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn is_similar(&self, other: &dyn Light) -> bool {
        other
            .as_any()
            .downcast_ref::<Self>()
            .map(|other| {
                self.position().approx_eq(&other.position())
                    && self.intensity().approx_eq(&other.intensity())
            })
            .unwrap_or(false)
    }

    fn incoming_at(&self, point: Point) -> IncomingLight {
        IncomingLight::Ray(LightRay {
            origin: self.position,
            direction: (self.position - point).normalized(),
            color: self.intensity,
        })
    }
}

#[derive(Debug)]
pub struct AmbientLight {
    intensity: Color,
}

impl AmbientLight {
    pub fn new(intensity: Color) -> Self {
        AmbientLight { intensity }
    }

    pub fn intensity(&self) -> Color {
        self.intensity
    }
}

impl Light for AmbientLight {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn is_similar(&self, other: &dyn Light) -> bool {
        other
            .as_any()
            .downcast_ref::<Self>()
            .map(|other| self.intensity().approx_eq(&other.intensity()))
            .unwrap_or(false)
    }

    fn incoming_at(&self, _: Point) -> IncomingLight {
        IncomingLight::Omni(self.intensity())
    }
}

#[derive(Debug)]
pub struct SphereLight {
    position: Point,
    intensity: Color,
    radius: f64,
}

impl SphereLight {
    pub fn new(position: Point, radius: f64, intensity: Color) -> Self {
        SphereLight {
            position,
            intensity,
            radius,
        }
    }

    pub fn position(&self) -> Point {
        self.position
    }

    pub fn intensity(&self) -> Color {
        self.intensity
    }

    pub fn radius(&self) -> f64 {
        self.radius
    }
}

impl Light for SphereLight {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn is_similar(&self, other: &dyn Light) -> bool {
        other
            .as_any()
            .downcast_ref::<Self>()
            .map(|other| {
                self.position().approx_eq(&other.position())
                    && self.intensity().approx_eq(&other.intensity())
                    && self.radius().approx_eq(&other.radius())
            })
            .unwrap_or(false)
    }

    fn incoming_at(&self, point: Point) -> IncomingLight {
        let p: [f64; 3] = UnitSphere.sample(&mut thread_rng());
        let origin = self.position + self.radius * vector(p[0], p[1], p[2]);

        IncomingLight::Ray(LightRay {
            origin,
            direction: (origin - point).normalized(),
            color: self.intensity,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::color::color;
    use crate::tuple::point;

    /// A point light has a position and intensity
    #[test]
    fn point_attrs() {
        let intensity = color(1, 1, 1);
        let position = point(0, 0, 0);
        let light = PointLight::new(position, intensity);
        assert_eq!(light.position(), position);
        assert_eq!(light.intensity(), intensity);
    }
}
