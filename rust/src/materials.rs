use crate::color::{color, Color};

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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::color::color;

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
}
