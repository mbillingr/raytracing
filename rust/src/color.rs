use std::ops::{Add, Mul, Sub};

pub fn color(r: impl Into<f64>, g: impl Into<f64>, b: impl Into<f64>) -> Color {
    Color::new(r.into(), g.into(), b.into())
}

#[derive(Debug, Copy, Clone)]
pub struct Color {
    red: f64,
    green: f64,
    blue: f64,
}

impl Color {
    pub fn new(red: f64, green: f64, blue: f64) -> Self {
        Color { red, green, blue }
    }

    pub fn red(&self) -> f64 {
        self.red
    }

    pub fn green(&self) -> f64 {
        self.green
    }

    pub fn blue(&self) -> f64 {
        self.blue
    }

    pub fn to_u8(self) -> (u8, u8, u8) {
        let c = self.clip(0.0, 1.0);
        (
            (c.red * 255.0).round() as u8,
            (c.green * 255.0).round() as u8,
            (c.blue * 255.0).round() as u8,
        )
    }

    pub fn clip(self, lo: f64, hi: f64) -> Self {
        Color::new(
            self.red.max(lo).min(hi),
            self.green.max(lo).min(hi),
            self.blue.max(lo).min(hi),
        )
    }

    pub fn add(&self, other: &Self) -> Self {
        Color::new(
            self.red + other.red,
            self.green + other.green,
            self.blue + other.blue,
        )
    }

    pub fn sub(&self, other: &Self) -> Self {
        Color::new(
            self.red - other.red,
            self.green - other.green,
            self.blue - other.blue,
        )
    }

    pub fn mul(&self, other: &Self) -> Self {
        Color::new(
            self.red * other.red,
            self.green * other.green,
            self.blue * other.blue,
        )
    }

    pub fn scale(&self, s: f64) -> Self {
        Color::new(self.red * s, self.green * s, self.blue * s)
    }
}

impl Add for Color {
    type Output = Color;
    fn add(self, other: Self) -> Self::Output {
        Color::add(&self, &other)
    }
}

impl Sub for Color {
    type Output = Color;
    fn sub(self, other: Self) -> Self::Output {
        Color::sub(&self, &other)
    }
}

impl Mul for Color {
    type Output = Color;
    fn mul(self, other: Self) -> Self::Output {
        Color::mul(&self, &other)
    }
}

impl<T: Into<f64>> Mul<T> for Color {
    type Output = Color;
    fn mul(self, rhs: T) -> Self::Output {
        Color::scale(&self, rhs.into())
    }
}

impl Mul<Color> for f64 {
    type Output = Color;
    fn mul(self, rhs: Color) -> Self::Output {
        Color::scale(&rhs, self)
    }
}

impl Mul<Color> for i32 {
    type Output = Color;
    fn mul(self, rhs: Color) -> Self::Output {
        Color::scale(&rhs, self as f64)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::approx_eq::ApproximateEq;

    /// Only for testing, implement an inaccurate PartialEq
    impl PartialEq for Color {
        fn eq(&self, other: &Self) -> bool {
            self.approx_eq(other)
        }
    }

    /// Colors are (red, green, blue) tuples
    #[test]
    fn colors() {
        let c = color(-0.5, 0.4, 1.7);
        assert_eq!(c.red(), -0.5);
        assert_eq!(c.green(), 0.4);
        assert_eq!(c.blue(), 1.7);
    }

    /// Adding colors
    #[test]
    fn add_colors() {
        let c1 = color(0.9, 0.6, 0.75);
        let c2 = color(0.7, 0.1, 0.25);
        assert_eq!(c1 + c2, color(1.6, 0.7, 1.0));
    }

    /// Subtracting colors
    #[test]
    fn sub_colors() {
        let c1 = color(0.9, 0.6, 0.75);
        let c2 = color(0.7, 0.1, 0.25);
        assert_eq!(c1 - c2, color(0.2, 0.5, 0.5));
    }

    /// Multiplying a color by a scalar
    #[test]
    fn scale_color() {
        let c = color(0.2, 0.3, 0.4);
        assert_eq!(c * 2, color(0.4, 0.6, 0.8));
        assert_eq!(2 * c, color(0.4, 0.6, 0.8));
    }

    /// Multiplying colors
    #[test]
    fn mul_colors() {
        let c1 = color(1.0, 0.2, 0.4);
        let c2 = color(0.9, 1, 0.1);
        assert_eq!(c1 * c2, color(0.9, 0.2, 0.04));
    }
}
