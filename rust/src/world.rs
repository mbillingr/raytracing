use crate::color::{color, Color, BLACK};
use crate::lights::{Light, LightRay, PointLight};
use crate::materials::Phong;
use crate::matrix::scaling;
use crate::ray::{hit, Intersection, IntersectionState, Ray};
use crate::shapes::{sphere, Shape};
use crate::tuple::{point, Point};

pub struct World {
    lights: Vec<Box<dyn Light>>,
    objects: Vec<Shape>,
    max_reflection_depth: u32,
}

impl Default for World {
    fn default() -> Self {
        World::new(
            vec![Box::new(PointLight::new(
                point(-10, 10, -10),
                color(1, 1, 1),
            ))],
            vec![
                sphere().with_material(
                    Phong::default()
                        .with_color(color(0.8, 1.0, 0.6))
                        .with_diffuse(0.7)
                        .with_specular(0.2),
                ),
                sphere().with_transform(scaling(0.5, 0.5, 0.5)),
            ],
        )
    }
}

impl World {
    pub fn new(lights: Vec<Box<dyn Light>>, objects: Vec<Shape>) -> Self {
        World {
            lights,
            objects,
            max_reflection_depth: 10,
        }
    }

    pub fn empty() -> Self {
        World::new(vec![], vec![])
    }

    pub fn add_light(&mut self, light: impl Light) {
        self.lights.push(Box::new(light));
    }

    pub fn add_shape(&mut self, shape: Shape) {
        self.objects.push(shape);
    }

    pub fn trace(&self, ray: &Ray) -> Option<Color> {
        self.color_at(ray, self.max_reflection_depth)
    }

    fn color_at(&self, ray: &Ray, remaining_bounces: u32) -> Option<Color> {
        let xs = self.intersect(ray);
        hit(&xs).map(|i| self.shade_hit(i.prepare_computations(&ray, &xs), remaining_bounces))
    }

    fn shade_hit(&self, comps: IntersectionState, remaining_bounces: u32) -> Color {
        let material = comps.obj.material();
        let surface = self.lights.iter().fold(BLACK, |color, light| {
            let lighting = light.incoming_at(comps.over_point);
            color
                + comps.obj.material().lighting(
                    &comps.obj,
                    lighting,
                    comps.point,
                    comps.eyev,
                    comps.normalv,
                    self.is_shadowed(&lighting, comps.over_point),
                )
        });

        let reflected = self.reflected_color(&comps, remaining_bounces);
        let refracted = self.refracted_color(&comps, remaining_bounces);

        if material.reflective() > 0.0 && material.transparency() > 0.0 {
            let reflectance = comps.schlick();
            surface + reflected * reflectance + refracted * (1.0 - reflectance)
        } else {
            surface + reflected + refracted
        }
    }

    pub fn intersect(&self, ray: &Ray) -> Vec<Intersection> {
        let mut xs: Vec<_> = self
            .objects
            .iter()
            .flat_map(|obj| obj.intersect(ray))
            .collect();
        xs.sort_unstable_by(|a, b| {
            a.t.partial_cmp(&b.t)
                .expect("Unable to compare intersection distances")
        });
        xs
    }

    pub fn intersect_shadow(&self, ray: &Ray) -> Vec<Intersection> {
        let mut xs: Vec<_> = self
            .objects
            .iter()
            .filter(|obj| obj.cast_shadow())
            .flat_map(|obj| obj.intersect(ray))
            .collect();
        xs.sort_unstable_by(|a, b| {
            a.t.partial_cmp(&b.t)
                .expect("Unable to compare intersection distances")
        });
        xs
    }

    pub fn is_shadowed(&self, light: &LightRay, p: Point) -> bool {
        hit(&self.intersect_shadow(&Ray::new(p, light.direction)))
            .map(|i| i.t < (light.origin - p).len())
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

    fn refracted_color(&self, comps: &IntersectionState, remaining_bounces: u32) -> Color {
        if remaining_bounces == 0 || comps.obj.material().transparency() == 0.0 {
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
                self.color_at(
                    &Ray::new(comps.under_point, direction),
                    remaining_bounces - 1,
                )
                .map(|c| c * comps.obj.material().transparency())
                .unwrap_or(BLACK)
            }
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
    use crate::pattern::Pattern;
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
        let light: &dyn Light = &PointLight::new(point(-10, 10, -10), color(1, 1, 1));
        let s1 = sphere().with_material(
            Phong::default()
                .with_color(color(0.8, 1.0, 0.6))
                .with_diffuse(0.7)
                .with_specular(0.2),
        );
        let s2 = sphere().with_transform(scaling(0.5, 0.5, 0.5));
        let w = World::default();
        assert_eq!(w.lights.len(), 1);
        assert!(w.lights.contains_similar(light));

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
        let comps = i.prepare_computations(&r, &[i]);
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
        w.lights = vec![Box::new(PointLight::new(point(0, 0.25, 0), color(1, 1, 1)))];
        let comps = i.prepare_computations(&r, &[i]);
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
        assert!(!w.is_shadowed(&w.lights[0].incoming_at(p), p))
    }

    /// There is shadow when an object is between point and light
    #[test]
    fn shadow2() {
        let w = World::default();
        let p = point(10, -10, 10);
        assert!(w.is_shadowed(&w.lights[0].incoming_at(p), p))
    }

    /// There is no shadow if the object is behind the light
    #[test]
    fn shadow3() {
        let w = World::default();
        let p = point(-20, 20, -20);
        assert!(!w.is_shadowed(&w.lights[0].incoming_at(p), p))
    }

    /// There is no shadow if the object is behind the point
    #[test]
    fn shadow4() {
        let w = World::default();
        let p = point(-2, 2, -2);
        assert!(!w.is_shadowed(&w.lights[0].incoming_at(p), p))
    }

    /// There is no shadow when the object between point and light does not cast shadows
    #[test]
    fn shadow6() {
        let mut w = World::default();
        w.objects[0].set_cast_shadow(false);
        w.objects[1].set_cast_shadow(false);
        let p = point(10, -10, 10);
        assert!(!w.is_shadowed(&w.lights[0].incoming_at(p), p))
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
        let comps = i.prepare_computations(&r, &[i]);
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
        let comps = i.prepare_computations(&r, &[i]);
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
        let comps = i.prepare_computations(&r, &[i]);
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
        let comps = i.prepare_computations(&r, &[i]);
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
        let comps = i.prepare_computations(&r, &[i]);
        let c = w.reflected_color(&comps, 0);
        assert_almost_eq!(c, color(0, 0, 0));
    }

    /// color-at() with mutually reflective surfaces
    #[test]
    fn infinite_reflection() {
        let mut w = World::empty();
        w.add_shape(
            plane().with_transform(translation(0, -1, 0)).with_material(
                Phong::default()
                    .with_color(color(0, 0, 0))
                    .with_ambient(1.0)
                    .with_diffuse(0.0)
                    .with_specular(0.0)
                    .with_reflective(1.0),
            ),
        );
        w.add_shape(
            plane().with_transform(translation(0, 1, 0)).with_material(
                Phong::default()
                    .with_color(color(0, 0, 0))
                    .with_ambient(1.0)
                    .with_diffuse(0.0)
                    .with_specular(0.0)
                    .with_reflective(1.0),
            ),
        );
        w.add_light(PointLight::new(point(0, 0, 0), color(1, 1, 1)));
        w.trace(&Ray::new(point(0, 0, 0), vector(0, 1, 0))).unwrap(); // just make sure the function returns
    }

    /// The refracted color with an opaque surface
    #[test]
    fn refract_opaque() {
        let w = World::default();
        let shape = &w.objects[0];
        let r = Ray::new(point(0, 0, -5), vector(0, 0, 1));
        let xs = intersections![Intersection::new(4.0, shape), Intersection::new(6.0, shape)];
        let comps = xs[0].prepare_computations(&r, &xs);
        let c = w.refracted_color(&comps, 5);
        assert_almost_eq!(c, color(0, 0, 0));
    }

    /// The refracted color at maximum recursion depth
    #[test]
    fn refract_recursion_limit() {
        let mut w = World::default();
        w.objects[0].material_mut().set_transparency(1.0);
        w.objects[0].material_mut().set_refractive_index(1.5);
        let shape = &w.objects[0];
        let r = Ray::new(point(0, 0, -5), vector(0, 0, 1));
        let xs = intersections![Intersection::new(4.0, shape), Intersection::new(6.0, shape)];
        let comps = xs[0].prepare_computations(&r, &xs);
        let c = w.refracted_color(&comps, 0);
        assert_almost_eq!(c, color(0, 0, 0));
    }

    /// The refracted color under total internal reflection
    #[test]
    fn refract_total_internal_reflection() {
        let mut w = World::default();
        w.objects[0].material_mut().set_transparency(1.0);
        w.objects[0].material_mut().set_refractive_index(1.5);
        let shape = &w.objects[0];
        let r = Ray::new(point(0, 0, FRAC_1_SQRT_2), vector(0, 1, 0));
        let xs = intersections![
            Intersection::new(-FRAC_1_SQRT_2, shape),
            Intersection::new(FRAC_1_SQRT_2, shape)
        ];
        let comps = xs[1].prepare_computations(&r, &xs);
        let c = w.refracted_color(&comps, 5);
        assert_almost_eq!(c, color(0, 0, 0));
    }

    /// Finding the refracted color
    #[test]
    fn refracted_color() {
        fn test_pattern() -> Pattern {
            Pattern::new(|p| color(p.x(), p.y(), p.z()))
        }

        let mut w = World::default();
        w.objects[0].material_mut().set_ambient(1.0);
        w.objects[0].material_mut().set_pattern(test_pattern());
        w.objects[1].material_mut().set_transparency(1.0);
        w.objects[1].material_mut().set_refractive_index(1.5);
        let a = &w.objects[0];
        let b = &w.objects[1];

        let r = Ray::new(point(0, 0, 0.1), vector(0, 1, 0));
        let xs = intersections![
            Intersection::new(-0.9899, a),
            Intersection::new(-0.4899, b),
            Intersection::new(0.4899, b),
            Intersection::new(0.9899, a)
        ];
        let comps = xs[2].prepare_computations(&r, &xs);
        let c = w.refracted_color(&comps, 5);
        assert_almost_eq!(c, color(0, 0.99888, 0.04722));
    }

    /// shade-hit() with a transparent material
    #[test]
    fn refracted_shading() {
        let mut w = World::default();
        w.add_shape(
            plane().with_transform(translation(0, -1, 0)).with_material(
                Phong::default()
                    .with_transparency(0.5)
                    .with_refractive_index(1.5),
            ),
        );
        w.add_shape(
            sphere()
                .with_transform(translation(0, -3.5, -0.5))
                .with_material(Phong::default().with_rgb(1.0, 0.0, 0.0).with_ambient(0.5)),
        );

        let floor = &w.objects[2];

        let r = Ray::new(point(0, 0, -3), vector(0, -FRAC_1_SQRT_2, FRAC_1_SQRT_2));
        let xs = intersections![Intersection::new(SQRT_2, floor),];
        let comps = xs[0].prepare_computations(&r, &xs);
        let c = w.shade_hit(comps, 5);
        assert_almost_eq!(c, color(0.93642, 0.68642, 0.68642));
    }

    /// shade-hit() with a reflective, transparent material
    #[test]
    fn schlick_shading() {
        let mut w = World::default();
        w.add_shape(
            plane().with_transform(translation(0, -1, 0)).with_material(
                Phong::default()
                    .with_reflective(0.5)
                    .with_transparency(0.5)
                    .with_refractive_index(1.5),
            ),
        );
        w.add_shape(
            sphere()
                .with_transform(translation(0, -3.5, -0.5))
                .with_material(Phong::default().with_rgb(1.0, 0.0, 0.0).with_ambient(0.5)),
        );

        let floor = &w.objects[2];

        let r = Ray::new(point(0, 0, -3), vector(0, -FRAC_1_SQRT_2, FRAC_1_SQRT_2));
        let xs = intersections![Intersection::new(SQRT_2, floor),];
        let comps = xs[0].prepare_computations(&r, &xs);
        let c = w.shade_hit(comps, 5);
        assert_almost_eq!(c, color(0.93391, 0.69643, 0.69243));
    }
}
