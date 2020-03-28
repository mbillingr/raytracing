use crate::color::{Color, color};

pub fn canvas(w: usize, h: usize) -> Canvas {
    Canvas::new(w, h)
}

pub struct Canvas {
    width: usize,
    height: usize,
    data: Vec<Color>,
}

impl Canvas {
    pub fn new(width: usize, height: usize) -> Self {
        Canvas {
            width, height,
            data: vec![color(0, 0, 0); width * height],
        }
    }

    pub fn width(&self) -> usize {
        self.width
    }

    pub fn height(&self) -> usize {
        self.height
    }

    pub fn set_pixel(&mut self, x: usize, y: usize, c: Color) {
        self.rows_mut().skip(y).next().unwrap()[x] = c;
    }

    pub fn get_pixel(&self, x: usize, y: usize) -> Color {
        self.rows().skip(y).next().unwrap()[x]
    }

    pub fn rows(&self) -> impl Iterator<Item=&[Color]> + '_ {
        self.data.chunks_exact(self.width)
    }

    pub fn rows_mut(&mut self) -> impl Iterator<Item=&mut [Color]> + '_ {
        self.data.chunks_exact_mut(self.width)
    }

    pub fn flat(&self) -> impl Iterator<Item=Color> + '_ {
        self.data.iter().copied()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Creating a canvas
    #[test]
    fn test_canvas() {
        let c = canvas(10, 20);
        assert_eq!(c.width(), 10);
        assert_eq!(c.height(), 20);
        assert!(c.flat().all(|pixel| pixel == color(0, 0, 0)));
    }

    /// Writing pixels to a canvas
    #[test]
    fn set_pixel() {
        let mut c = canvas(10, 20);
        let red = color(1, 0, 0);
        c.set_pixel(2, 3, red);
        assert_eq!(c.get_pixel(2, 3), red);
    }
}