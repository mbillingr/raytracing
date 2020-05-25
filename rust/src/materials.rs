use crate::approx_eq::{ApproximateEq, EPSILON};
use crate::color::{color, Color, BLACK};
use crate::lights::IncomingLight;
use crate::pattern::Pattern;
use crate::photon_map::TravellingPhoton;
use crate::ray::{IntersectionState, Ray};
use crate::tuple::Vector;
use crate::world::World;
use rand::distributions::{Distribution, WeightedIndex};
use rand::thread_rng;
use std::any::Any;
use std::f64::consts::PI;

pub trait Material: 'static + std::fmt::Debug + Sync {
    fn as_any(&self) -> &dyn Any;
    fn box_clone(&self) -> Box<dyn Material>;
    fn is_similar(&self, other: &dyn Material) -> bool;

    fn color_at(&self, comps: &IntersectionState) -> Color;
    fn lighting(&self, light: IncomingLight, comps: &IntersectionState, in_shadow: bool) -> Color;
    fn shade_hit(&self, world: &World, comps: &IntersectionState, remaining_bounces: u32) -> Color;

    fn photon_hit(
        &self,
        photon: TravellingPhoton,
        comps: &IntersectionState,
        enable_diffuse: bool,
    ) -> (Option<TravellingPhoton>, Option<TravellingPhoton>);

    fn refractive_index(&self) -> f64;
}

impl Clone for Box<dyn Material> {
    fn clone(&self) -> Self {
        self.box_clone()
    }
}

#[derive(Debug, Clone)]
pub enum SurfaceColor {
    Flat(Color),
    Pattern(Pattern),
}

#[derive(Debug, Clone)]
pub struct Phong {
    color: SurfaceColor,
    emissive: f64,
    diffuse: f64,
    specular: f64,
    shininess: f64,
    reflective: f64,
    transparency: f64,
    refractive_index: f64,
}

impl Default for Phong {
    fn default() -> Self {
        Self::new(color(1, 1, 1), 0.0, 0.9, 0.9, 200.0, 0.0, 0.0, 1.0)
    }
}

impl Phong {
    pub fn new(
        color: Color,
        emissive: f64,
        diffuse: f64,
        specular: f64,
        shininess: f64,
        reflective: f64,
        transparency: f64,
        refractive_index: f64,
    ) -> Self {
        Phong {
            color: SurfaceColor::Flat(color),
            emissive,
            diffuse,
            specular,
            shininess,
            reflective,
            transparency,
            refractive_index,
        }
    }
    pub fn new_pattern(
        pattern: Pattern,
        emissive: f64,
        diffuse: f64,
        specular: f64,
        shininess: f64,
        reflective: f64,
        transparency: f64,
        refractive_index: f64,
    ) -> Self {
        Phong {
            color: SurfaceColor::Pattern(pattern),
            emissive,
            diffuse,
            specular,
            shininess,
            reflective,
            transparency,
            refractive_index,
        }
    }

    pub fn with_color(self, color: Color) -> Self {
        Phong {
            color: SurfaceColor::Flat(color),
            ..self
        }
    }

    pub fn set_color(&mut self, c: Color) {
        self.color = SurfaceColor::Flat(c);
    }

    pub fn with_pattern(self, pattern: Pattern) -> Self {
        Phong {
            color: SurfaceColor::Pattern(pattern),
            ..self
        }
    }

    pub fn set_pattern(&mut self, p: Pattern) {
        self.color = SurfaceColor::Pattern(p);
    }

    pub fn with_rgb(self, r: f64, g: f64, b: f64) -> Self {
        self.with_color(color(r, g, b))
    }

    pub fn with_hsv(self, h: f64, s: f64, v: f64) -> Self {
        self.with_color(Color::from_hsv(h, s, v))
    }

    pub fn with_emissive(self, emissive: f64) -> Self {
        Phong { emissive, ..self }
    }

    pub fn with_diffuse(self, diffuse: f64) -> Self {
        Phong { diffuse, ..self }
    }

    pub fn with_specular(self, specular: f64) -> Self {
        Phong { specular, ..self }
    }

    pub fn with_shininess(self, shininess: f64) -> Self {
        Phong { shininess, ..self }
    }

    pub fn with_reflective(self, reflective: f64) -> Self {
        Phong { reflective, ..self }
    }

    pub fn color(&self) -> &SurfaceColor {
        &self.color
    }

    pub fn emissive(&self) -> f64 {
        self.emissive
    }

    pub fn set_emissive(&mut self, emissive: f64) {
        self.emissive = emissive
    }

    pub fn diffuse(&self) -> f64 {
        self.diffuse
    }

    pub fn set_diffuse(&mut self, diffuse: f64) {
        self.diffuse = diffuse
    }

    pub fn specular(&self) -> f64 {
        self.specular
    }

    pub fn set_specular(&mut self, specular: f64) {
        self.specular = specular
    }

    pub fn shininess(&self) -> f64 {
        self.shininess
    }

    pub fn set_shininess(&mut self, shininess: f64) {
        self.shininess = shininess
    }

    pub fn reflective(&self) -> f64 {
        self.reflective
    }

    pub fn set_reflective(&mut self, reflective: f64) {
        self.reflective = reflective
    }

    pub fn transparency(&self) -> f64 {
        self.transparency
    }

    pub fn set_transparency(&mut self, t: f64) {
        self.transparency = t;
    }

    pub fn with_transparency(self, transparency: f64) -> Self {
        Phong {
            transparency,
            ..self
        }
    }

    pub fn refractive_index(&self) -> f64 {
        self.refractive_index
    }

    pub fn set_refractive_index(&mut self, n: f64) {
        self.refractive_index = n;
    }

    pub fn with_refractive_index(self, refractive_index: f64) -> Self {
        Phong {
            refractive_index,
            ..self
        }
    }

    pub fn color_at(&self, comps: &IntersectionState) -> Color {
        match &self.color {
            SurfaceColor::Flat(c) => *c,
            SurfaceColor::Pattern(p) => comps.obj.pattern_at(p, comps.point),
        }
    }

    pub fn lighting(
        &self,
        surface_color: Color,
        light: IncomingLight,
        eyev: Vector,
        normalv: Vector,
        in_shadow: bool,
    ) -> Color {
        match light {
            IncomingLight::NoLight => BLACK,
            IncomingLight::Omni(intensity) => surface_color * intensity,
            IncomingLight::Ray(lightray) => {
                let effective_color = surface_color * lightray.color;

                if in_shadow {
                    return BLACK;
                }

                let light_dot_normal = lightray.direction.dot(&normalv);

                let diffuse;
                let specular;

                if light_dot_normal <= 0.0 {
                    diffuse = BLACK;
                    specular = BLACK;
                } else {
                    diffuse = effective_color * (self.diffuse() * light_dot_normal);

                    let reflectv = (-lightray.direction).reflect(&normalv);
                    let reflect_dot_eye = reflectv.dot(&eyev);

                    specular = if reflect_dot_eye <= 0.0 {
                        BLACK
                    } else {
                        lightray.color * (self.specular() * reflect_dot_eye.powf(self.shininess()))
                    };
                }

                diffuse + specular
            }
        }
    }

    pub fn reflected_color(
        &self,
        world: &World,
        comps: &IntersectionState,
        remaining_bounces: u32,
    ) -> Color {
        let r = self.reflective();
        if r == 0.0 || remaining_bounces == 0 {
            BLACK
        } else {
            world
                .color_at(
                    &Ray::new(comps.over_point, comps.reflectv),
                    remaining_bounces - 1,
                )
                .map(|c| c * r)
                .unwrap_or(BLACK)
        }
    }

    pub fn refracted_color(
        &self,
        world: &World,
        comps: &IntersectionState,
        remaining_bounces: u32,
    ) -> Color {
        if remaining_bounces == 0 || self.transparency() == 0.0 {
            color(0, 0, 0)
        } else {
            let n_ratio = comps.n1 / comps.n2;
            let cos_i = comps.eyev.dot(&comps.normalv);
            let sin2_t = n_ratio * n_ratio * (1.0 - cos_i * cos_i);
            if sin2_t > 1.0 {
                color(0, 0, 0)
            } else {
                let cos_t = (1.0 - sin2_t).sqrt();
                let direction = comps.normalv * (n_ratio * cos_i - cos_t) - comps.eyev * n_ratio;
                world
                    .color_at(
                        &Ray::new(comps.under_point, direction),
                        remaining_bounces - 1,
                    )
                    .map(|c| c * self.transparency())
                    .unwrap_or(BLACK)
            }
        }
    }
}

impl Material for Phong {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn box_clone(&self) -> Box<dyn Material> {
        Box::new(self.clone())
    }

    fn is_similar(&self, other: &dyn Material) -> bool {
        other
            .as_any()
            .downcast_ref::<Self>()
            .map(|o| self.approx_eq(o))
            .unwrap_or(false)
    }

    fn color_at(&self, comps: &IntersectionState) -> Color {
        Phong::color_at(self, comps)
    }

    fn lighting(&self, light: IncomingLight, comps: &IntersectionState, in_shadow: bool) -> Color {
        Phong::lighting(
            self,
            self.color_at(comps),
            light,
            comps.eyev,
            comps.normalv,
            in_shadow,
        )
    }

    fn shade_hit(&self, world: &World, comps: &IntersectionState, remaining_bounces: u32) -> Color {
        let surface_color = self.color_at(&comps);

        let mut surface = BLACK;

        if world.photon_map_enabled() {
            if let Some((pm, n_nearest)) = world.get_photon_map() {
                let (photons, square_radius) = pm.find_nearest(n_nearest, comps.point);
                let total_light = photons.iter().fold(BLACK, |color, photon| {
                    color + comps.normalv.dot(&photon.direction()).max(0.0) * photon.power()
                });
                surface = surface + surface_color * total_light / (PI * square_radius);
            }
        }

        if world.direct_illumination_enabled() {
            surface = surface
                + world.lights().iter().fold(BLACK, |color, light| {
                    let incoming_light = light.incoming_at(comps.over_point);
                    let in_shadow = world.is_shadowed(&incoming_light, comps.over_point);
                    color
                        + comps
                            .obj
                            .material()
                            .lighting(incoming_light, &comps, in_shadow)
                });
        }

        surface = surface.clip(0.0, 1.0);

        let emissive = surface_color * self.emissive;

        let reflected = self.reflected_color(world, &comps, remaining_bounces);
        let refracted = self.refracted_color(world, &comps, remaining_bounces);

        if self.reflective() > 0.0 && self.transparency() > 0.0 {
            let reflectance = comps.schlick();
            surface + reflected * reflectance + refracted * (1.0 - reflectance) + emissive
        } else {
            surface + reflected + refracted + emissive
        }
    }

    fn photon_hit(
        &self,
        photon: TravellingPhoton,
        comps: &IntersectionState,
        enable_diffuse: bool,
    ) -> (Option<TravellingPhoton>, Option<TravellingPhoton>) {
        let mut stored_photon = None;
        let next_photon;

        let diffuse_reflectance = self.diffuse() * self.color_at(comps);
        let mut specular_reflectance = self.specular().max(self.reflective()); // TODO: Is this correct?
        let mut transmittance = self.transparency();

        let mut pd_avg = diffuse_reflectance.sum() / 3.0;

        if pd_avg > EPSILON {
            stored_photon = Some(photon);
        }

        if !enable_diffuse {
            pd_avg = 0.0;
        }

        if self.reflective() > 0.0 && self.transparency() > 0.0 {
            let r = comps.schlick();
            specular_reflectance *= r;
            transmittance *= 1.0 - r;
        }

        let p_absorb = 1.0 - pd_avg - specular_reflectance - transmittance;
        assert!(p_absorb >= 0.0);
        assert!(p_absorb <= 1.0);

        let dist =
            WeightedIndex::new(&[p_absorb, pd_avg, specular_reflectance, transmittance]).unwrap();
        match dist.sample(&mut thread_rng()) {
            0 => next_photon = None,
            1 => {
                next_photon =
                    Some(photon.scatter(comps.over_point, comps.normalv, diffuse_reflectance))
            }
            2 => next_photon = Some(photon.reflect(comps.over_point, comps.normalv)),
            3 => {
                next_photon =
                    Some(photon.refract(comps.under_point, comps.normalv, comps.n1, comps.n2))
            }
            _ => unreachable!(),
        };

        (stored_photon, next_photon)
    }

    fn refractive_index(&self) -> f64 {
        Phong::refractive_index(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::approx_eq::ApproximateEq;
    use crate::color::color;
    use crate::lights::Light;
    use crate::lights::PointLight;
    use crate::tuple::{point, vector};

    /// The default material
    #[test]
    fn point_attrs() {
        let m = Phong::default();
        assert_almost_eq!(m.color(), &SurfaceColor::Flat(color(1, 1, 1)));
        assert_eq!(m.emissive(), 0.0);
        assert_eq!(m.diffuse(), 0.9);
        assert_eq!(m.specular(), 0.9);
        assert_eq!(m.shininess(), 200.0);
        assert_eq!(m.reflective(), 0.0);
        assert_eq!(m.transparency(), 0.0);
        assert_eq!(m.refractive_index(), 1.0);
    }

    /// Lighting with the eye between light and surface
    #[test]
    fn lighting1() {
        let m = Phong::default();
        let pos = point(0, 0, 0);
        let eyev = vector(0, 0, -1);
        let normalv = vector(0, 0, -1);
        let light = PointLight::new(point(0, 0, -10), color(1, 1, 1));
        let result = m.lighting(
            color(1.0, 1.0, 1.0),
            light.incoming_at(pos),
            eyev,
            normalv,
            false,
        );
        assert_almost_eq!(result, color(1.8, 1.8, 1.8));
    }

    /// Lighting with the eye between light and surface, eye offset 45 deg
    #[test]
    fn lighting2() {
        let m = Phong::default();
        let pos = point(0, 0, 0);
        let eyev = vector(
            0,
            std::f64::consts::FRAC_1_SQRT_2,
            -std::f64::consts::FRAC_1_SQRT_2,
        );
        let normalv = vector(0, 0, -1);
        let light = PointLight::new(point(0, 0, -10), color(1, 1, 1));
        let result = m.lighting(
            color(1.0, 1.0, 1.0),
            light.incoming_at(pos),
            eyev,
            normalv,
            false,
        );
        assert_almost_eq!(result, color(0.9, 0.9, 0.9));
    }

    /// Lighting with eye opposite surface, light offset 45 deg
    #[test]
    fn lighting3() {
        let m = Phong::default();
        let pos = point(0, 0, 0);
        let eyev = vector(0, 0, -1);
        let normalv = vector(0, 0, -1);
        let light = PointLight::new(point(0, 10, -10), color(1, 1, 1));
        let result = m.lighting(
            color(1.0, 1.0, 1.0),
            light.incoming_at(pos),
            eyev,
            normalv,
            false,
        );
        assert_almost_eq!(result, color(0.6364, 0.6364, 0.6364));
    }

    /// Lighting with eye in the path of the reflection vector
    #[test]
    fn lighting4() {
        let m = Phong::default();
        let pos = point(0, 0, 0);
        let eyev = vector(
            0,
            -std::f64::consts::FRAC_1_SQRT_2,
            -std::f64::consts::FRAC_1_SQRT_2,
        );
        let normalv = vector(0, 0, -1);
        let light = PointLight::new(point(0, 10, -10), color(1, 1, 1));
        let result = m.lighting(
            color(1.0, 1.0, 1.0),
            light.incoming_at(pos),
            eyev,
            normalv,
            false,
        );
        assert_almost_eq!(result, color(1.5364, 1.5364, 1.5364));
    }

    /// Lighting with the light behind the surface
    #[test]
    fn lighting5() {
        let m = Phong::default();
        let pos = point(0, 0, 0);
        let eyev = vector(0, 0, -1);
        let normalv = vector(0, 0, -1);
        let light = PointLight::new(point(0, 0, 10), color(1, 1, 1));
        let result = m.lighting(
            color(1.0, 1.0, 1.0),
            light.incoming_at(pos),
            eyev,
            normalv,
            false,
        );
        assert_almost_eq!(result, BLACK);
    }

    /// Lighting with the surface in shadow
    #[test]
    fn shadow() {
        let m = Phong::default();
        let pos = point(0, 0, 0);
        let eyev = vector(0, 0, -1);
        let normalv = vector(0, 0, -1);
        let light = PointLight::new(point(0, 0, -10), color(1, 1, 1));
        let result = m.lighting(
            color(1.0, 1.0, 1.0),
            light.incoming_at(pos),
            eyev,
            normalv,
            true,
        );
        assert_almost_eq!(result, BLACK);
    }
}
