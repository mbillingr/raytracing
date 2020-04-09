use crate::color::Color;
use crate::matrix::Matrix;
use crate::tuple::Point;
use std::rc::Rc;

pub fn stripe_pattern(a: Color, b: Color) -> Pattern {
    Pattern::new(move |p| if p.x().floor() % 2.0 == 0.0 { a } else { b })
}

#[derive(Clone)]
pub struct Pattern {
    func: Rc<dyn Fn(Point) -> Color>,
    inv_transform: Matrix,
}

impl Pattern {
    pub fn new(f: impl 'static + Fn(Point) -> Color) -> Self {
        Pattern {
            func: Rc::new(f),
            inv_transform: Matrix::identity(),
        }
    }

    pub fn at(&self, obj_point: Point) -> Color {
        (self.func)(self.inv_transform * obj_point)
    }

    pub fn set_transform(&mut self, t: Matrix) {
        self.inv_transform = t.inverse();
    }

    pub fn with_transform(mut self, t: Matrix) -> Self {
        self.set_transform(t);
        self
    }
}

impl std::fmt::Debug for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Pattern")
    }
}

impl PartialEq for Pattern {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.func, &other.func)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::approx_eq::ApproximateEq;
    use crate::color::{BLACK, WHITE};
    use crate::lights::PointLight;
    use crate::materials::Phong;
    use crate::matrix::{scaling, translation};
    use crate::shapes::sphere;
    use crate::tuple::{point, vector};

    /// A stripe pattern is constant in y
    #[test]
    fn stripe_y() {
        let pattern = stripe_pattern(WHITE, BLACK);
        assert_almost_eq!(pattern.at(point(0, 0, 0)), WHITE);
        assert_almost_eq!(pattern.at(point(0, 1, 0)), WHITE);
        assert_almost_eq!(pattern.at(point(0, 2, 0)), WHITE);
    }

    /// A stripe pattern is constant in z
    #[test]
    fn stripe_z() {
        let pattern = stripe_pattern(WHITE, BLACK);
        assert_almost_eq!(pattern.at(point(0, 0, 0)), WHITE);
        assert_almost_eq!(pattern.at(point(0, 0, 1)), WHITE);
        assert_almost_eq!(pattern.at(point(0, 0, 2)), WHITE);
    }

    /// A stripe pattern alternates in x
    #[test]
    fn stripe_x() {
        let pattern = stripe_pattern(WHITE, BLACK);
        assert_almost_eq!(pattern.at(point(0, 0, 0)), WHITE);
        assert_almost_eq!(pattern.at(point(0.9, 0, 0)), WHITE);
        assert_almost_eq!(pattern.at(point(1.0, 0, 0)), BLACK);
        assert_almost_eq!(pattern.at(point(-0.1, 0, 0)), BLACK);
        assert_almost_eq!(pattern.at(point(-1.0, 0, 0)), BLACK);
        assert_almost_eq!(pattern.at(point(-1.1, 0, 0)), WHITE);
    }

    /// Lighting with a pattern applied
    #[test]
    fn stripe_light() {
        let m = Phong::new_pattern(stripe_pattern(WHITE, BLACK), 1.0, 0.0, 0.0, 1.0);
        let eyev = vector(0, 0, -1);
        let normalv = vector(0, 0, -1);
        let light = PointLight::new(point(0, 0, -10), WHITE);
        let c1 = m.lighting(&sphere(), &light, point(0.9, 0, 0), eyev, normalv, false);
        let c2 = m.lighting(&sphere(), &light, point(1.1, 0, 0), eyev, normalv, false);
        assert_almost_eq!(c1, WHITE);
        assert_almost_eq!(c2, BLACK);
    }

    /// Stripes with an object transformation
    #[test]
    fn stripe_otrans() {
        let mut object = sphere();
        object.set_transform(scaling(2, 2, 2));
        let pattern = stripe_pattern(WHITE, BLACK);
        let c = object.pattern_at(&pattern, point(1.5, 0, 0));
        assert_almost_eq!(c, WHITE);
    }

    /// Stripes with a pattern transformation
    #[test]
    fn stripe_ptrans() {
        let object = sphere();
        let mut pattern = stripe_pattern(WHITE, BLACK);
        pattern.set_transform(scaling(2, 2, 2));
        let c = object.pattern_at(&pattern, point(1.5, 0, 0));
        assert_almost_eq!(c, WHITE);
    }

    /// Stripes with both, a pattern and an object transformation
    #[test]
    fn stripe_optrans() {
        let mut object = sphere();
        object.set_transform(scaling(2, 2, 2));
        let mut pattern = stripe_pattern(WHITE, BLACK);
        pattern.set_transform(translation(0.5, 0, 0));
        let c = object.pattern_at(&pattern, point(2.5, 0, 0));
        assert_almost_eq!(c, WHITE);
    }
}
