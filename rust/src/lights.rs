use crate::approx_eq::ApproximateEq;
use crate::color::{color, Color};
use crate::cosine_distribution::CosineDistribution;
use crate::tuple::{vector, Point, Vector};
use rand::distributions::Distribution;
use rand::thread_rng;
use rand_distr::{StandardNormal, UnitDisc, UnitSphere};
use std::any::Any;
use std::f64::consts::PI;

// TODO: make sure the photon emitting behavior of light sources is consistent with ray tracing.

pub trait Light: Sync + 'static {
    fn as_any(&self) -> &dyn Any;
    fn is_similar(&self, other: &dyn Light) -> bool;
    fn incoming_at(&self, point: Point) -> IncomingLight;

    fn power(&self) -> f64;
    fn emit_photon(&self) -> LightRay;
}

#[derive(Debug, Copy, Clone)]
pub enum IncomingLight {
    Ray(LightRay),
    Omni(Color),
    NoLight,
}

impl IncomingLight {
    pub fn intensity(&self) -> Color {
        match self {
            IncomingLight::Ray(lr) => lr.color,
            IncomingLight::Omni(c) => *c,
            IncomingLight::NoLight => color(0, 0, 0),
        }
    }
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

    pub fn compute_power(intensity: Color) -> f64 {
        intensity.sum() / 3.0
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

    /// The point light is the reference light source: it has a scaling factor of 1.
    /// All other light sources need to specify a scaling factor relative to that of
    /// the point light.
    fn power(&self) -> f64 {
        PointLight::compute_power(self.intensity) * 1.0
    }

    fn emit_photon(&self) -> LightRay {
        let p: [f64; 3] = UnitSphere.sample(&mut thread_rng());
        LightRay {
            origin: self.position,
            direction: p.into(),
            color: self.intensity,
        }
    }
}

#[derive(Debug)]
pub struct RealisticPointLight {
    position: Point,
    intensity: Color,
}

impl RealisticPointLight {
    pub fn new(position: Point, intensity: Color) -> Self {
        RealisticPointLight {
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

impl Light for RealisticPointLight {
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
        let distance2 = (self.position - point).square_len();
        IncomingLight::Ray(LightRay {
            origin: self.position,
            direction: (self.position - point).normalized(),
            color: self.intensity / (4.0 * PI * distance2),
        })
    }

    /// The point light is the reference light source: it has a scaling factor of 1.
    /// All other light sources need to specify a scaling factor relative to that of
    /// the point light.
    fn power(&self) -> f64 {
        PointLight::compute_power(self.intensity) * 1.0
    }

    fn emit_photon(&self) -> LightRay {
        let p: [f64; 3] = UnitSphere.sample(&mut thread_rng());
        LightRay {
            origin: self.position,
            direction: p.into(),
            color: self.intensity * 2.0 / PI, // I do not know where the factor 2/pi comes from, but empirically it gives the same brightness as incoming_at...
        }
    }
}

#[derive(Debug)]
pub struct Beam {
    position: Point,
    up: Vector,
    right: Vector,
    direction: Vector,
    intensity: Color,
}

impl Beam {
    pub fn new(position: Point, up: Vector, right: Vector, intensity: Color) -> Self {
        assert!(
            up.dot(&right).approx_eq(&0.0),
            "*up* and *right* must be orthogonal"
        );
        Beam {
            position,
            up,
            right,
            direction: right.cross(&up).normalized(),
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

impl Light for Beam {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn is_similar(&self, other: &dyn Light) -> bool {
        other
            .as_any()
            .downcast_ref::<Self>()
            .map(|other| {
                self.position().approx_eq(&other.position())
                    && self.up.approx_eq(&other.up)
                    && self.right.approx_eq(&other.right)
                    && self.intensity().approx_eq(&other.intensity())
            })
            .unwrap_or(false)
    }

    fn incoming_at(&self, point: Point) -> IncomingLight {
        let direction = point - self.position;
        if direction.dot(&self.direction) < 0.0 {
            return IncomingLight::NoLight;
        }
        let u = direction.dot(&self.up) / self.up.square_len();
        let v = direction.dot(&self.right) / self.right.square_len();
        let radius_squared = u * u + v * v;
        IncomingLight::Ray(LightRay {
            origin: self.position + u * self.up + v * self.right,
            direction: -self.direction,
            color: self.intensity * f64::exp(-radius_squared),
        })
    }

    /// The point light is the reference light source: it has a scaling factor of 1.
    /// All other light sources need to specify a scaling factor relative to that of
    /// the point light.
    fn power(&self) -> f64 {
        PointLight::compute_power(self.intensity) * 1.0
    }

    fn emit_photon(&self) -> LightRay {
        let rng = &mut thread_rng();
        let u: f64 = StandardNormal.sample(rng);
        let v: f64 = StandardNormal.sample(rng);
        LightRay {
            origin: self.position + u * self.up + v * self.right,
            direction: self.direction,
            color: self.intensity * 2.0 / PI,
        }
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

    /// The Ambient light has a power of 0. This is mainly for practical reasons; it should never
    /// emit photons.
    fn power(&self) -> f64 {
        0.0
    }

    fn emit_photon(&self) -> LightRay {
        unimplemented!()
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
            color: self.intensity, // TODO: should the intensity be scaled with the cosine of the angle between sphere normal and outgoing direction?
        })
    }

    /// Assume that the spherical light is a point light with a diffusing sphere around it.
    /// Thus, it has the same power as a point light. I think this implies that larger radii
    /// lead to apparently dimmer light sources.
    fn power(&self) -> f64 {
        PointLight::compute_power(self.intensity) * 1.0
    }

    fn emit_photon(&self) -> LightRay {
        let rng = &mut thread_rng();
        let p: Vector = UnitSphere.sample(rng).into();
        let d = CosineDistribution::new(p).sample(rng);
        LightRay {
            origin: self.position + p * self.radius,
            direction: d,
            color: self.intensity,
        }
    }
}

#[derive(Debug)]
pub struct DiscLight {
    position: Point,
    normal: Vector,
    radius: f64,
    width: Vector,
    height: Vector,
    intensity: Color,
}

impl DiscLight {
    pub fn new(position: Point, normal: Vector, radius: f64, intensity: Color) -> Self {
        let mut helper = vector(1, 0, 0);
        if helper.dot(&normal).approx_eq(&1.0) {
            helper = vector(0, 1, 0);
        }
        let width = normal.cross(&helper);
        let height = width.cross(&normal);
        DiscLight {
            position,
            normal,
            radius,
            width: width * radius,
            height: height * radius,
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

impl Light for DiscLight {
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
                    && self.normal.approx_eq(&other.normal)
                    && self.radius.approx_eq(&other.radius)
            })
            .unwrap_or(false)
    }

    fn incoming_at(&self, point: Point) -> IncomingLight {
        let rng = &mut thread_rng();
        let [u, v]: [f64; 2] = UnitDisc.sample(rng);
        let origin = self.position + self.width * u + self.height * v;

        let direction = (origin - point).normalized();

        let distance2 = (origin - point).square_len();

        IncomingLight::Ray(LightRay {
            origin,
            direction,
            color: self.intensity * (-direction).dot(&self.normal).max(0.0)
                / (4.0 * PI * distance2),
        })
    }

    /// Assume that the spherical light is a point light with a diffusing sphere around it.
    /// Thus, it has the same power as a point light. I think this implies that larger radii
    /// lead to apparently dimmer light sources.
    fn power(&self) -> f64 {
        PointLight::compute_power(self.intensity) * 1.0
    }

    fn emit_photon(&self) -> LightRay {
        let rng = &mut thread_rng();
        let [u, v]: [f64; 2] = UnitDisc.sample(rng);
        let origin = self.position + self.width * u + self.height * v;

        let d = CosineDistribution::new(self.normal).sample(rng);
        LightRay {
            origin,
            direction: d,
            color: self.intensity * 0.25 * 2.0 / PI, // I do not know where the factor 2/pi comes from, but empirically it gives the same brightness as incoming_at...
        }
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

    /// Constructing a beam light
    #[test]
    fn beam_light() {
        let beam = Beam::new(
            point(0, 0, 0),
            vector(0, 1, 0),
            vector(1, 0, 0),
            color(1, 1, 1),
        );
        assert_almost_eq!(beam.position, point(0, 0, 0));
        assert_almost_eq!(beam.up, vector(0, 1, 0));
        assert_almost_eq!(beam.right, vector(1, 0, 0));
        assert_almost_eq!(beam.direction, vector(0, 0, 1));
        assert_almost_eq!(beam.intensity, color(1, 1, 1));
    }

    /// beam profile
    #[test]
    fn beam_no_backlight() {
        let beam = Beam::new(
            point(0, 0, 0),
            vector(0, 1, 0),
            vector(1, 0, 0),
            color(1, 1, 1),
        );
        assert_almost_eq!(
            beam.incoming_at(point(0, 0, -10)).intensity(),
            color(0, 0, 0)
        );
    }

    /// beam profile
    #[test]
    fn beam_profile_circular() {
        let beam = Beam::new(
            point(0, 0, 0),
            vector(0, 1, 0),
            vector(1, 0, 0),
            color(1, 1, 1),
        );
        assert_almost_eq!(
            beam.incoming_at(point(0, 0, 10)).intensity(),
            color(1, 1, 1)
        );
        assert_almost_eq!(
            beam.incoming_at(point(0.83255, 0, 10)).intensity(),
            color(0.5, 0.5, 0.5)
        );
        assert_almost_eq!(
            beam.incoming_at(point(0, -0.83255, 10)).intensity(),
            color(0.5, 0.5, 0.5)
        );
        assert_almost_eq!(
            beam.incoming_at(point(-0.58871, 0.58871, 10)).intensity(),
            color(0.5, 0.5, 0.5)
        );
    }
}
