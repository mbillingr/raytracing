use crate::canvas::Canvas;
use crate::color::{Color, BLACK};
use crate::live_preview::{live_preview, Event, Message};
use crate::matrix::{rotation, translation, Matrix};
use crate::ray::Ray;
use crate::tuple::{point, vector, Point, Vector};
use crate::world::World;
use rand::seq::SliceRandom;
use rand::{thread_rng, Rng};
use rayon::prelude::*;
use std::sync::mpsc::{RecvError, TryRecvError};
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

    pixel_allowed_standard_error: f64,
    pixel_min_samples: u16,

    focal_distance: f64,
    aperture_size: f64,
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
            pixel_allowed_standard_error: 1e-1,
            pixel_min_samples: 5,
            focal_distance: 3e100,
            aperture_size: 0.0,
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

    pub fn set_allowed_standard_error(&mut self, se: f64) {
        self.pixel_allowed_standard_error = se;
    }

    pub fn set_min_samples(&mut self, n: u16) {
        self.pixel_min_samples = n;
    }

    pub fn pixel_size(&self) -> f64 {
        self.pixel_size
    }

    pub fn set_aperture_size(&mut self, s: f64) {
        self.aperture_size = s;
    }

    pub fn set_focal_distance(&mut self, d: f64) {
        self.focal_distance = d;
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
        let primary_ray = Ray::new(origin, (pixel - origin).normalized());

        if self.aperture_size == 0.0 {
            primary_ray
        } else {
            let focal_point = primary_ray.position(self.focal_distance);

            let mut rnd = thread_rng();
            let ap_pixel = pixel
                + vector(
                    (rnd.gen::<f64>() - 0.5) * self.aperture_size,
                    (rnd.gen::<f64>() - 0.5) * self.aperture_size,
                    0.0,
                );
            let secondary_ray = Ray::new(ap_pixel, (focal_point - ap_pixel).normalized());

            secondary_ray
        }
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
        let (h, tx, _) = live_preview(self.hsize as usize, self.vsize as usize, window_name);
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

    pub fn render_interactive(&mut self, world: &World, window_name: &'static str) -> Canvas {
        self.set_min_samples(1);

        let mut canvas = Canvas::new(self.hsize, self.vsize);
        let (h, tx, event_receiver) =
            live_preview(self.hsize as usize, self.vsize as usize, window_name);
        let tx = Mutex::new(tx);

        loop {
            let mut coordinates: Vec<_> = (0..self.vsize)
                .flat_map(|y| (0..self.hsize).map(move |x| (x, y)))
                .collect();
            coordinates.shuffle(&mut thread_rng());

            canvas.clear(BLACK);
            tx.lock()
                .unwrap()
                .send(Message::Clear(128, 128, 128))
                .unwrap();

            let mut event = None;

            for chunk in coordinates.chunks(16) {
                let buffer: Vec<_> = chunk
                    .to_vec()
                    .into_par_iter()
                    .map(|(x, y)| {
                        let c = self.multisample(x, y, world);
                        (x, y, c)
                    })
                    .collect();

                for (x, y, c) in buffer {
                    tx.lock()
                        .unwrap()
                        .send(Message::set_pixel(x, y, c))
                        .unwrap();
                    canvas.set_pixel(x, y, c);
                }

                match event_receiver.try_recv() {
                    Ok(e) => {
                        event = Some(e);
                        break;
                    }
                    Err(TryRecvError::Empty) => {}
                    Err(TryRecvError::Disconnected) => return canvas,
                }
            }

            if event.is_none() {
                match event_receiver.recv() {
                    Ok(e) => event = Some(e),
                    Err(_) => return canvas,
                }
            }

            match event.unwrap() {
                Event::Forward(t) => self.set_transform(translation(0.0, 0.0, t) * self.transform),
                Event::Side(t) => self.set_transform(translation(t, 0.0, 0.0) * self.transform),
                Event::Up(t) => self.set_transform(translation(0.0, t, 0.0) * self.transform),
                Event::Yaw(phi) => self.set_transform(self.transform.rotate(phi, vector(0, 1, 0))),
            }
        }

        h.join().unwrap();
        canvas
    }

    pub fn trace_pixels(
        &self,
        world: &World,
        pixel_callback: impl Sync + Fn(u32, u32, Color),
    ) -> Vec<(u32, u32, Color)> {
        let coordinates: Vec<_> = (0..self.vsize)
            .flat_map(|y| (0..self.hsize).map(move |x| (x, y)))
            .collect();
        //coordinates.shuffle(&mut thread_rng());

        coordinates
            .into_par_iter()
            .map(|(x, y)| (x, y, self.multisample(x, y, world)))
            .inspect(|&(x, y, color)| pixel_callback(x, y, color))
            .collect()
    }

    /*fn simple_sample(&self, x: u32, y: u32, world: &World) -> Color {
        world
            .trace(&self.ray_for_pixel(x, y, false))
            .unwrap_or(BLACK)
    }*/

    fn multisample(&self, x: u32, y: u32, world: &World) -> Color {
        let c = world
            .trace(&self.ray_for_pixel(x, y, false))
            .unwrap_or(BLACK);
        let mut color_sum_of_squares = c;
        let mut color_sum = c;
        let mut n = 1.0;

        while n < self.pixel_min_samples as f64 {
            let c = world
                .trace(&self.ray_for_pixel(x, y, true))
                .unwrap_or(BLACK);
            color_sum = color_sum + c;
            color_sum_of_squares = color_sum_of_squares + c * c;
            n += 1.0;
        }

        while color_variance_of_mean(n, color_sum, color_sum_of_squares)
            > self.pixel_allowed_standard_error * self.pixel_allowed_standard_error
        {
            let c = world
                .trace(&self.ray_for_pixel(x, y, true))
                .unwrap_or(BLACK);
            color_sum = color_sum + c;
            color_sum_of_squares = color_sum_of_squares + c * c;
            n += 1.0;
        }

        color_sum / n
    }
}

fn color_variance_of_mean(n: f64, sum: Color, sos: Color) -> f64 {
    let mean_rgb = sum / n;
    let variance_rgb = sos / n - mean_rgb * mean_rgb;
    let variance = (variance_rgb.red() + variance_rgb.green() + variance_rgb.blue()) / 3.0;
    variance / n
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
        let mut c = Camera::new(11, 11, PI / 2.0).with_view_transform(
            point(0, 0, -5),
            point(0, 0, 0),
            vector(0, 1, 0),
        );
        c.set_min_samples(1);
        c.set_allowed_standard_error(1.0);
        let image = c.render(&World::default());
        assert_almost_eq!(image.get_pixel(5, 5), color(0.38066, 0.47583, 0.2855))
    }
}
