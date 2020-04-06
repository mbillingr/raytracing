use crate::color::{color, Color};
use crate::lights::PointLight;
use crate::tuple::{Point, Vector};

#[derive(Debug, Clone)]
pub struct Phong {
    color: Color,
    ambient: f64,
    diffuse: f64,
    specular: f64,
    shininess: f64,
}

impl Default for Phong {
    fn default() -> Self {
        Self::new(color(1, 1, 1), 0.1, 0.9, 0.9, 200.0)
    }
}

impl Phong {
    pub fn new(color: Color, ambient: f64, diffuse: f64, specular: f64, shininess: f64) -> Self {
        Phong {
            color,
            ambient,
            diffuse,
            specular,
            shininess,
        }
    }

    pub fn with_color(self, color: Color) -> Self {
        Phong { color, ..self }
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

    pub fn color(&self) -> Color {
        self.color
    }

    pub fn ambient(&self) -> f64 {
        self.ambient
    }

    pub fn diffuse(&self) -> f64 {
        self.diffuse
    }

    pub fn specular(&self) -> f64 {
        self.specular
    }

    pub fn shininess(&self) -> f64 {
        self.shininess
    }

    pub fn lighting(
        &self,
        light: &PointLight,
        point: Point,
        eyev: Vector,
        normalv: Vector,
        in_shadow: bool,
    ) -> Color {
        let effective_color = self.color() * light.intensity();
        let ambient = effective_color * self.ambient();

        if in_shadow {
            return ambient;
        }

        let lightv = (light.position() - point).normalized();
        let light_dot_normal = lightv.dot(&normalv);

        let diffuse;
        let specular;

        if light_dot_normal <= 0.0 {
            diffuse = Color::BLACK;
            specular = Color::BLACK;
        } else {
            diffuse = effective_color * (self.diffuse() * light_dot_normal);

            let reflectv = (-lightv).reflect(&normalv);
            let reflect_dot_eye = reflectv.dot(&eyev);

            specular = if reflect_dot_eye <= 0.0 {
                Color::BLACK
            } else {
                light.intensity() * (self.specular() * reflect_dot_eye.powf(self.shininess()))
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
    use crate::lights::PointLight;
    use crate::tuple::{point, vector};

    /// The default material
    #[test]
    fn point_attrs() {
        let m = Phong::default();
        assert_eq!(m.color(), color(1, 1, 1));
        assert_eq!(m.ambient(), 0.1);
        assert_eq!(m.diffuse(), 0.9);
        assert_eq!(m.specular(), 0.9);
        assert_eq!(m.shininess(), 200.0);
    }

    /// Lighting with the eye between light and surface
    #[test]
    fn lighting1() {
        let m = Phong::default();
        let pos = point(0, 0, 0);
        let eyev = vector(0, 0, -1);
        let normalv = vector(0, 0, -1);
        let light = PointLight::new(point(0, 0, -10), color(1, 1, 1));
        let result = m.lighting(&light, pos, eyev, normalv, false);
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
        let result = m.lighting(&light, pos, eyev, normalv, false);
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
        let result = m.lighting(&light, pos, eyev, normalv, false);
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
        let result = m.lighting(&light, pos, eyev, normalv, false);
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
        let result = m.lighting(&light, pos, eyev, normalv, false);
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
        let result = m.lighting(&light, pos, eyev, normalv, true);
        assert_almost_eq!(result, color(0.1, 0.1, 0.1));
    }
}
