use crate::color::{color, Color, BLACK};
use crate::lights::LightRay;
use crate::pattern::Pattern;
use crate::shapes::Shape;
use crate::tuple::{Point, Vector};

#[derive(Debug, Clone)]
pub enum SurfaceColor {
    Flat(Color),
    Pattern(Pattern),
}

#[derive(Debug, Clone)]
pub struct Phong {
    color: SurfaceColor,
    ambient: f64,
    diffuse: f64,
    specular: f64,
    shininess: f64,
    reflective: f64,
    transparency: f64,
    refractive_index: f64,
}

impl Default for Phong {
    fn default() -> Self {
        Self::new(color(1, 1, 1), 0.1, 0.9, 0.9, 200.0, 0.0, 0.0, 1.0)
    }
}

impl Phong {
    pub fn new(
        color: Color,
        ambient: f64,
        diffuse: f64,
        specular: f64,
        shininess: f64,
        reflective: f64,
        transparency: f64,
        refractive_index: f64,
    ) -> Self {
        Phong {
            color: SurfaceColor::Flat(color),
            ambient,
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
        ambient: f64,
        diffuse: f64,
        specular: f64,
        shininess: f64,
        reflective: f64,
        transparency: f64,
        refractive_index: f64,
    ) -> Self {
        Phong {
            color: SurfaceColor::Pattern(pattern),
            ambient,
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

    pub fn with_ambient(self, ambient: f64) -> Self {
        Phong { ambient, ..self }
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

    pub fn ambient(&self) -> f64 {
        self.ambient
    }

    pub fn set_ambient(&mut self, ambient: f64) {
        self.ambient = ambient
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

    pub fn lighting(
        &self,
        obj: &Shape,
        light: LightRay,
        point: Point,
        eyev: Vector,
        normalv: Vector,
        in_shadow: bool,
    ) -> Color {
        let color = match &self.color {
            SurfaceColor::Flat(c) => *c,
            SurfaceColor::Pattern(p) => obj.pattern_at(p, point),
        };

        let effective_color = color * light.color;
        let ambient = effective_color * self.ambient();

        if in_shadow {
            return ambient;
        }

        let light_dot_normal = light.direction.dot(&normalv);

        let diffuse;
        let specular;

        if light_dot_normal <= 0.0 {
            diffuse = BLACK;
            specular = BLACK;
        } else {
            diffuse = effective_color * (self.diffuse() * light_dot_normal);

            let reflectv = (-light.direction).reflect(&normalv);
            let reflect_dot_eye = reflectv.dot(&eyev);

            specular = if reflect_dot_eye <= 0.0 {
                BLACK
            } else {
                light.color * (self.specular() * reflect_dot_eye.powf(self.shininess()))
            };
        }

        ambient + diffuse + specular
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::approx_eq::ApproximateEq;
    use crate::color::color;
    use crate::lights::Light;
    use crate::lights::PointLight;
    use crate::shapes::sphere;
    use crate::tuple::{point, vector};

    /// The default material
    #[test]
    fn point_attrs() {
        let m = Phong::default();
        assert_almost_eq!(m.color(), &SurfaceColor::Flat(color(1, 1, 1)));
        assert_eq!(m.ambient(), 0.1);
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
        let result = m.lighting(&sphere(), light.incoming_at(pos), pos, eyev, normalv, false);
        assert_almost_eq!(result, color(1.9, 1.9, 1.9));
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
        let result = m.lighting(&sphere(), light.incoming_at(pos), pos, eyev, normalv, false);
        assert_almost_eq!(result, color(1.0, 1.0, 1.0));
    }

    /// Lighting with eye opposite surface, light offset 45 deg
    #[test]
    fn lighting3() {
        let m = Phong::default();
        let pos = point(0, 0, 0);
        let eyev = vector(0, 0, -1);
        let normalv = vector(0, 0, -1);
        let light = PointLight::new(point(0, 10, -10), color(1, 1, 1));
        let result = m.lighting(&sphere(), light.incoming_at(pos), pos, eyev, normalv, false);
        assert_almost_eq!(result, color(0.7364, 0.7364, 0.7364));
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
        let result = m.lighting(&sphere(), light.incoming_at(pos), pos, eyev, normalv, false);
        assert_almost_eq!(result, color(1.6364, 1.6364, 1.6364));
    }

    /// Lighting with the light behind the surface
    #[test]
    fn lighting5() {
        let m = Phong::default();
        let pos = point(0, 0, 0);
        let eyev = vector(0, 0, -1);
        let normalv = vector(0, 0, -1);
        let light = PointLight::new(point(0, 0, 10), color(1, 1, 1));
        let result = m.lighting(&sphere(), light.incoming_at(pos), pos, eyev, normalv, false);
        assert_almost_eq!(result, color(0.1, 0.1, 0.1));
    }

    /// Lighting with the surface in shadow
    #[test]
    fn shadow() {
        let m = Phong::default();
        let pos = point(0, 0, 0);
        let eyev = vector(0, 0, -1);
        let normalv = vector(0, 0, -1);
        let light = PointLight::new(point(0, 0, -10), color(1, 1, 1));
        let result = m.lighting(&sphere(), light.incoming_at(pos), pos, eyev, normalv, true);
        assert_almost_eq!(result, color(0.1, 0.1, 0.1));
    }
}
