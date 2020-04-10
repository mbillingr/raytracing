use crate::color::{color, Color, BLACK};
use crate::lights::PointLight;
use crate::materials::Phong;
use crate::matrix::scaling;
use crate::ray::{hit, Intersection, IntersectionState, Ray};
use crate::shapes::{sphere, Shape};
use crate::tuple::{point, Point};

pub struct World {
    lights: Vec<PointLight>,
    objects: Vec<Shape>,
    max_reflection_depth: u32,
}

impl Default for World {
    fn default() -> Self {
        World::new(
            vec![PointLight::new(point(-10, 10, -10), color(1, 1, 1))],
            vec![
                sphere().with_material(Phong::new(color(0.8, 1.0, 0.6), 0.1, 0.7, 0.2, 200.0, 0.0)),
                sphere().with_transform(scaling(0.5, 0.5, 0.5)),
            ],
        )
    }
}

impl World {
    pub fn new(lights: Vec<PointLight>, objects: Vec<Shape>) -> Self {
        World {
            lights,
            objects,
            max_reflection_depth: 10,
        }
    }

    pub fn empty() -> Self {
        World::new(vec![], vec![])
    }

    pub fn add_light(&mut self, light: PointLight) {
        self.lights.push(light);
    }

    pub fn add_shape(&mut self, shape: Shape) {
        self.objects.push(shape);
    }

    pub fn trace(&self, ray: &Ray) -> Option<Color> {
        self.color_at(ray, self.max_reflection_depth)
    }

    fn color_at(&self, ray: &Ray, remaining_bounces: u32) -> Option<Color> {
        hit(&self.intersect(ray))
            .map(|i| self.shade_hit(i.prepare_computations(&ray), remaining_bounces))
    }

    fn shade_hit(&self, comps: IntersectionState, remaining_bounces: u32) -> Color {
        self.lights.iter().fold(BLACK, |color, light| {
            color
                + comps.obj.material().lighting(
                    &comps.obj,
                    &light,
                    comps.point,
                    comps.eyev,
                    comps.normalv,
                    self.is_shadowed(&light, comps.over_point),
                )
                + self.reflected_color(&comps, remaining_bounces)
        })
    }

    pub fn intersect(&self, ray: &Ray) -> Vec<Intersection> {
        let mut xs = vec![];
        for obj in &self.objects {
            xs.extend(obj.intersect(ray));
        }
        xs.sort_unstable_by(|a, b| {
            a.t.partial_cmp(&b.t)
                .expect("Unable to compare intersection distances")
        });
        xs
    }

    pub fn is_shadowed(&self, light: &PointLight, p: Point) -> bool {
        let direction = light.position() - p;
        hit(&self.intersect(&Ray::new(p, direction.normalized())))
            .map(|i| i.t < direction.len())
            .unwrap_or(false)
    }

    fn reflected_color(&self, comps: &IntersectionState, remaining_bounces: u32) -> Color {
        let r = comps.obj.material().reflective();
        if r == 0.0 || remaining_bounces == 0 {
            BLACK
        } else {
            self.color_at(
                &Ray::new(comps.over_point, comps.reflectv),
                remaining_bounces - 1,
            )
            .map(|c| c * r)
            .unwrap_or(BLACK)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::approx_eq::ApproximateEq;
    use crate::approx_eq::FindSimilar;
    use crate::color::color;
    use crate::materials::{Phong, SurfaceColor};
    use crate::matrix::{scaling, translation};
    use crate::shapes::{plane, sphere};
    use crate::tuple::{point, vector};
    use std::f64::consts::{FRAC_1_SQRT_2, SQRT_2};

    /// Creating a world
    #[test]
    fn empty() {
        let w = World::empty();
        assert!(w.lights.is_empty());
        assert!(w.objects.is_empty());
    }

    /// The default world
    #[test]
    fn default() {
        let light = PointLight::new(point(-10, 10, -10), color(1, 1, 1));
        let s1 = sphere().with_material(
            Phong::default()
                .with_color(color(0.8, 1.0, 0.6))
                .with_diffuse(0.7)
                .with_specular(0.2),
        );
        let s2 = sphere().with_transform(scaling(0.5, 0.5, 0.5));
        let w = World::default();
        assert_eq!(w.lights.len(), 1);
        assert!(w.lights.contains_similar(&light));

        assert_eq!(w.objects.len(), 2);
        assert!(w.objects.contains_similar(&s1));
        assert!(w.objects.contains_similar(&s2));
    }

    /// Intersect a world with a ray
    #[test]
    fn intersect() {
        let w = World::default();
        let r = Ray::new(point(0, 0, -5), vector(0, 0, 1));
        let xs = w.intersect(&r);
        assert_eq!(xs.len(), 4);
        assert_almost_eq!(xs[0].t, 4.0);
        assert_almost_eq!(xs[1].t, 4.5);
        assert_almost_eq!(xs[2].t, 5.5);
        assert_almost_eq!(xs[3].t, 6.0);
    }

    /// Shading an intersection
    #[test]
    fn shade_intersection() {
        let w = World::default();
        let r = Ray::new(point(0, 0, -5), vector(0, 0, 1));
        let shape = &w.objects[0];
        let i = Intersection::new(4.0, shape);
        let comps = i.prepare_computations(&r);
        let c = w.shade_hit(comps, 0);
        assert_almost_eq!(c, color(0.38066, 0.47583, 0.2855));
    }

    /// Shading an intersection from the inside
    #[test]
    fn shade_inner_intersection() {
        let mut w = World::default();
        let r = Ray::new(point(0, 0, 0), vector(0, 0, 1));
        let shape = &w.objects[1];
        let i = Intersection::new(0.5, shape);
        w.lights = vec![PointLight::new(point(0, 0.25, 0), color(1, 1, 1))];
        let comps = i.prepare_computations(&r);
        let c = w.shade_hit(comps, 0);
        assert_almost_eq!(c, color(0.90498, 0.90498, 0.90498));
    }

    /// The color when a ray misses
    #[test]
    fn miss() {
        let w = World::default();
        let r = Ray::new(point(0, 0, -5), vector(0, 1, 0));
        let c = w.color_at(&r, 0);
        assert_eq!(c, None);
    }

    /// The color when a ray hits
    #[test]
    fn hit() {
        let w = World::default();
        let r = Ray::new(point(0, 0, -5), vector(0, 0, 1));
        let c = w.color_at(&r, 0);
        assert_eq!(c, Some(color(0.38066, 0.47583, 0.2855)));
    }

    /// The color with an intersection behind the ray
    #[test]
    fn behind() {
        let mut w = World::default();
        let r = Ray::new(point(0, 0, 0.75), vector(0, 0, -1));
        let m0 = w.objects[0].material().clone().with_ambient(1.0);
        w.objects[0].set_material(m0);
        let m1 = w.objects[1].material().clone().with_ambient(1.0);
        w.objects[1].set_material(m1.clone());
        let c = w.color_at(&r, 0).unwrap();
        assert_almost_eq!(SurfaceColor::Flat(c), m1.color());
    }

    /// There is no shadow if nothing is collinear with point and light
    #[test]
    fn shadow1() {
        let w = World::default();
        let p = point(0, 10, 0);
        assert!(!w.is_shadowed(&w.lights[0], p))
    }

    /// There is shadow when an object is between point and light
    #[test]
    fn shadow2() {
        let w = World::default();
        let p = point(10, -10, 10);
        assert!(w.is_shadowed(&w.lights[0], p))
    }

    /// There is no shadow if the object is behind the light
    #[test]
    fn shadow3() {
        let w = World::default();
        let p = point(-20, 20, -20);
        assert!(!w.is_shadowed(&w.lights[0], p))
    }

    /// There is no shadow if the object is behind the point
    #[test]
    fn shadow4() {
        let w = World::default();
        let p = point(-2, 2, -2);
        assert!(!w.is_shadowed(&w.lights[0], p))
    }

    /// Shading an intersection in shadow
    #[test]
    fn shadow5() {
        let mut w = World::empty();
        w.add_light(PointLight::new(point(0, 0, -10), color(1, 1, 1)));
        w.add_shape(sphere());
        w.add_shape(sphere().with_transform(translation(0, 0, 10)));

        let r = Ray::new(point(0, 0, 5), vector(0, 0, 1));
        let i = Intersection::new(4.0, &w.objects[1]);
        let comps = i.prepare_computations(&r);
        let c = w.shade_hit(comps, 0);
        assert_almost_eq!(c, color(0.1, 0.1, 0.1));
    }

    /// The reflected color of a nonreflective material
    #[test]
    fn reflect_nothing() {
        let mut w = World::default();
        let r = Ray::new(point(0, 0, 0), vector(0, 0, 1));
        w.objects[1].material_mut().set_ambient(1.0);
        let i = Intersection::new(1.0, &w.objects[1]);
        let comps = i.prepare_computations(&r);
        let c = w.reflected_color(&comps, 1);
        assert_almost_eq!(c, color(0, 0, 0));
    }

    /// The reflected color for a reflective material
    #[test]
    fn reflection() {
        let mut w = World::default();
        let mut shape = plane();
        shape.material_mut().set_reflective(0.5);
        shape.set_transform(translation(0, -1, 0));
        w.add_shape(shape);
        let r = Ray::new(point(0, 0, -3), vector(0, -FRAC_1_SQRT_2, FRAC_1_SQRT_2));
        let i = Intersection::new(SQRT_2, &w.objects[2]);
        let comps = i.prepare_computations(&r);
        let c = w.reflected_color(&comps, 1);
        assert_almost_eq!(c, color(0.19033, 0.23792, 0.14274));
    }

    /// shade_hit() with a reflective material
    #[test]
    fn shade_reflection() {
        let mut w = World::default();
        let mut shape = plane();
        shape.material_mut().set_reflective(0.5);
        shape.set_transform(translation(0, -1, 0));
        w.add_shape(shape);
        let r = Ray::new(point(0, 0, -3), vector(0, -FRAC_1_SQRT_2, FRAC_1_SQRT_2));
        let i = Intersection::new(SQRT_2, &w.objects[2]);
        let comps = i.prepare_computations(&r);
        let c = w.shade_hit(comps, 1);
        assert_almost_eq!(c, color(0.87676, 0.92434, 0.82917));
    }

    /// The reflected color at maximum recursive depth
    #[test]
    fn max_reflect() {
        let mut w = World::default();
        let mut shape = plane();
        shape.material_mut().set_reflective(0.5);
        shape.set_transform(translation(0, -1, 0));
        w.add_shape(shape);
        let r = Ray::new(point(0, 0, -3), vector(0, -FRAC_1_SQRT_2, FRAC_1_SQRT_2));
        let i = Intersection::new(SQRT_2, &w.objects[2]);
        let comps = i.prepare_computations(&r);
        let c = w.reflected_color(&comps, 0);
        assert_almost_eq!(c, color(0, 0, 0));
    }

    /// color-at() with mutually reflective surfaces
    #[test]
    fn infinite_reflection() {
        let mut w = World::empty();
        w.add_shape(
            plane()
                .with_transform(translation(0, -1, 0))
                .with_material(Phong::new(color(0, 0, 0), 1.0, 0.0, 0.0, 0.0, 1.0)),
        );
        w.add_shape(
            plane()
                .with_transform(translation(0, 1, 0))
                .with_material(Phong::new(color(0, 0, 0), 1.0, 0.0, 0.0, 0.0, 1.0)),
        );
        w.add_light(PointLight::new(point(0, 0, 0), color(1, 1, 1)));
        w.trace(&Ray::new(point(0, 0, 0), vector(0, 1, 0))).unwrap(); // just make sure the function returns
    }
}
