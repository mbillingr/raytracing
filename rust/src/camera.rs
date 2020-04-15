use crate::canvas::Canvas;
use crate::color::Color;
use crate::live_preview::{live_preview, Message};
use crate::matrix::Matrix;
use crate::ray::Ray;
use crate::tuple::{point, Point, Vector};
use crate::world::World;
use rand::seq::SliceRandom;
use rand::{thread_rng, Rng};
use rayon::prelude::*;
use std::sync::Mutex;

#[derive(Debug, Clone)]
pub struct Camera {
    hsize: u32,
    vsize: u32,
    field_of_view: f64,

    transform: Matrix,
    inv_transform: Matrix,

    pixel_size: f64,
    half_width: f64,
    half_height: f64,

    multisampling: u16,
}

impl Camera {
    pub fn new(hsize: u32, vsize: u32, field_of_view: f64) -> Self {
        let half_view = f64::tan(field_of_view / 2.0);
        let aspect = hsize as f64 / vsize as f64;

        let half_width;
        let half_height;
        if aspect >= 1.0 {
            half_width = half_view;
            half_height = half_view / aspect;
        } else {
            half_width = half_view * aspect;
            half_height = half_view;
        }
        let pixel_size = half_width * 2.0 / hsize as f64;

        Camera {
            hsize,
            vsize,
            field_of_view,
            transform: Matrix::Identity,
            inv_transform: Matrix::Identity,
            pixel_size,
            half_width,
            half_height,
            multisampling: 1,
        }
    }

    pub fn hsize(&self) -> u32 {
        self.hsize
    }

    pub fn vsize(&self) -> u32 {
        self.vsize
    }

    pub fn field_of_view(&self) -> f64 {
        self.field_of_view
    }

    pub fn transform(&self) -> &Matrix {
        &self.transform
    }

    pub fn inv_transform(&self) -> &Matrix {
        &self.inv_transform
    }

    pub fn set_transform(&mut self, t: Matrix) {
        self.transform = t;
        self.inv_transform = t.inverse();
    }

    pub fn with_view_transform(self, from: Point, to: Point, up: Vector) -> Self {
        self.with_transform(Matrix::view(from, to, up))
    }

    pub fn with_transform(mut self, t: Matrix) -> Self {
        self.set_transform(t);
        self
    }

    pub fn set_multisampling(&mut self, n: u16) {
        self.multisampling = n;
    }

    pub fn pixel_size(&self) -> f64 {
        self.pixel_size
    }

    pub fn ray_for_pixel(&self, px: u32, py: u32, randomize: bool) -> Ray {
        let x_offset;
        let y_offset;
        if randomize {
            let mut rnd = thread_rng();
            x_offset = (px as f64 + rnd.gen::<f64>()) * self.pixel_size;
            y_offset = (py as f64 + rnd.gen::<f64>()) * self.pixel_size;
        } else {
            x_offset = (px as f64 + 0.5) * self.pixel_size;
            y_offset = (py as f64 + 0.5) * self.pixel_size;
        }
        let world_x = self.half_width - x_offset;
        let world_y = self.half_height - y_offset;
        let pixel = self.inv_transform * point(world_x, world_y, -1.0);
        let origin = self.inv_transform * point(0.0, 0.0, 0.0);
        Ray::new(origin, (pixel - origin).normalized())
    }

    pub fn render(&self, world: &World) -> Canvas {
        let mut canvas = Canvas::new(self.hsize, self.vsize);
        for (x, y, c) in self.trace_pixels(world, |_, _, _| ()) {
            canvas.add_to_pixel(x, y, c);
        }
        canvas
    }

    pub fn render_live(&self, world: &World, window_name: &'static str) -> Canvas {
        let mut canvas = Canvas::new(self.hsize, self.vsize);
        let (h, tx) = live_preview(self.hsize, self.vsize, window_name);
        // It sucks to wrap the Sender in a Mutex. Ideally, each thread would have one copy
        // of tx, but there does not seem to be an easy way to accomplish this with Rayon.
        let tx = Mutex::new(tx);
        for (x, y, c) in self.trace_pixels(world, move |x, y, c| {
            tx.lock()
                .unwrap()
                .send(Message::set_pixel(x, y, c))
                .unwrap();
        }) {
            canvas.add_to_pixel(x, y, c);
        }
        h.join().unwrap();
        canvas
    }

    pub fn trace_pixels(
        &self,
        world: &World,
        pixel_callback: impl Sync + Fn(u32, u32, Color),
    ) -> Vec<(u32, u32, Color)> {
        let mut coordinates: Vec<_> = (0..self.vsize)
            .flat_map(|y| (0..self.hsize).map(move |x| (x, y)))
            .collect();
        coordinates.shuffle(&mut thread_rng());

        coordinates
            .par_iter()
            .filter_map(|&(x, y)| {
                let mut color = None;
                for n in 0..self.multisampling {
                    let ray = self.ray_for_pixel(x, y, n > 0);
                    color = match (color, world.trace(&ray)) {
                        (None, None) => None,
                        (Some(c), None) => Some(c),
                        (None, Some(c)) => Some(c * (1.0 / self.multisampling as f64)),
                        (Some(c1), Some(c2)) => Some(c1 + c2 * (1.0 / self.multisampling as f64)),
                    };
                }
                color.map(|c| (x, y, c))
            })
            .inspect(|&(x, y, color)| pixel_callback(x, y, color))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::approx_eq::ApproximateEq;
    use crate::color::color;
    use crate::matrix::{rotation_y, translation};
    use crate::tuple::{point, vector};
    use crate::world::World;
    use std::f64::consts::{FRAC_1_SQRT_2, PI};

    /// Constructing a camera
    #[test]
    fn make_camera() {
        let hsize = 160;
        let vsize = 120;
        let field_of_view = PI / 2.0;
        let c = Camera::new(hsize, vsize, field_of_view);
        assert_eq!(c.hsize(), hsize);
        assert_eq!(c.vsize(), vsize);
        assert_eq!(c.field_of_view(), field_of_view);
        assert_eq!(*c.transform(), Matrix::Identity);
    }

    /// The pixel size for a horizontal canvas
    #[test]
    fn pixel_horizontal() {
        let c = Camera::new(200, 125, PI / 2.0);
        assert_almost_eq!(c.pixel_size(), 0.01);
    }

    /// The pixel size for a vertical canvas
    #[test]
    fn pixel_vertical() {
        let c = Camera::new(125, 200, PI / 2.0);
        assert_almost_eq!(c.pixel_size(), 0.01);
    }

    /// Constructing a ray through the center of the canvas
    #[test]
    fn ray_center() {
        let c = Camera::new(201, 101, PI / 2.0);
        let r = c.ray_for_pixel(100, 50, false);
        assert_almost_eq!(r.origin(), point(0, 0, 0));
        assert_almost_eq!(r.direction(), vector(0, 0, -1));
    }

    /// Constructing a ray through a corner of the canvas
    #[test]
    fn ray_corner() {
        let c = Camera::new(201, 101, PI / 2.0);
        let r = c.ray_for_pixel(0, 0, false);
        assert_almost_eq!(r.origin(), point(0, 0, 0));
        assert_almost_eq!(r.direction(), vector(0.66519, 0.33259, -0.66851));
    }

    /// Constructing a ray when the camera is transformed
    #[test]
    fn ray_transformed() {
        let mut c = Camera::new(201, 101, PI / 2.0);
        c.set_transform(rotation_y(PI / 4.0) * translation(0, -2, 5));
        let r = c.ray_for_pixel(100, 50, false);
        assert_almost_eq!(r.origin(), point(0, 2, -5));
        assert_almost_eq!(r.direction(), vector(FRAC_1_SQRT_2, 0, -FRAC_1_SQRT_2));
    }

    /// Rendering a world with the camera
    #[test]
    fn render() {
        let c = Camera::new(11, 11, PI / 2.0).with_view_transform(
            point(0, 0, -5),
            point(0, 0, 0),
            vector(0, 1, 0),
        );
        let image = c.render(&World::default());
        assert_almost_eq!(image.get_pixel(5, 5), color(0.38066, 0.47583, 0.2855))
    }
}
