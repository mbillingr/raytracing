use crate::approx_eq::EPSILON;
use crate::materials::Material;
use crate::matrix::Matrix;
use crate::shapes::Shape;
use crate::tuple::{Point, Vector};

#[derive(Debug, Copy, Clone)]
pub struct Ray {
    origin: Point,
    direction: Vector,
}

impl Ray {
    pub fn new(origin: Point, direction: Vector) -> Self {
        Ray { origin, direction }
    }

    pub fn origin(&self) -> Point {
        self.origin
    }

    pub fn direction(&self) -> Vector {
        self.direction
    }

    pub fn position(&self, t: f64) -> Point {
        self.origin + self.direction * t
    }

    pub fn transform(&self, m: Matrix) -> Self {
        Ray {
            origin: m * self.origin,
            direction: m * self.direction,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Intersection<'a> {
    pub t: f64,
    pub obj: &'a Shape,
    pub u: f64,
    pub v: f64,
    pub i: usize,
}

impl PartialEq for Intersection<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.t == other.t && std::ptr::eq(self.obj, other.obj)
    }
}

impl<'a> Intersection<'a> {
    pub fn new(t: f64, obj: &'a Shape) -> Self {
        Intersection {
            t,
            obj,
            u: 0.0,
            v: 0.0,
            i: 0,
        }
    }

    pub fn new_uv(t: f64, u: f64, v: f64, obj: &'a Shape) -> Self {
        Intersection { t, obj, u, v, i: 0 }
    }

    pub fn with_i(self, i: usize) -> Self {
        Intersection { i, ..self }
    }

    pub fn prepare_computations<'b: 'a>(
        &self,
        ray: &Ray,
        xs: &'b [Intersection<'a>],
    ) -> IntersectionState {
        let point = ray.position(self.t);
        let eyev = -ray.direction();
        let normalv = self.obj.normal_at(point, self);
        let inside = normalv.dot(&eyev) < 0.0;
        let normalv = if inside { -normalv } else { normalv };
        let over_point = point + normalv * EPSILON;
        let under_point = point - normalv * EPSILON;
        let reflectv = ray.direction().reflect(&normalv);
        let (mat1, mat2) = self.compute_materials(xs);
        let (n1, n2) = self.compute_refractive_indices(xs);
        IntersectionState {
            t: self.t,
            obj: self.obj,
            inside,
            point,
            over_point,
            under_point,
            eyev,
            normalv,
            reflectv,
            n1,
            n2,
            mat1,
            mat2,
        }
    }

    pub fn compute_refractive_indices(&self, xs: &[Intersection]) -> (f64, f64) {
        let (mat1, mat2) = self.compute_materials(xs);
        let n1 = mat1.map(Material::refractive_index).unwrap_or(1.0);
        let n2 = mat2.map(Material::refractive_index).unwrap_or(1.0);
        (n1, n2)
    }

    pub fn compute_materials(
        &self,
        xs: &'a [Intersection],
    ) -> (Option<&dyn Material>, Option<&dyn Material>) {
        let mut containers: Vec<&Shape> = vec![];

        let mut mat1 = None;

        for i in xs {
            if i == self {
                mat1 = containers.last().map(|shape| shape.material());
            }

            if let Some(idx) = containers
                .iter()
                .position(|&shape| std::ptr::eq(shape, i.obj))
            {
                containers.remove(idx);
            } else {
                containers.push(i.obj);
            }

            if i == self {
                let mat2 = containers.last().map(|shape| shape.material());
                return (mat1, mat2);
            }
        }

        panic!("hit not found in intersections")
    }
}

pub fn hit<'a>(xs: &[Intersection<'a>]) -> Option<Intersection<'a>> {
    /*xs.iter()
    .fold(None, |h, i| match (h, i.t) {
        (_, t) if t < 0.0 => h,
        (None, _) => Some(i),
        (Some(h), t) if t < h.t => Some(i),
        _ => h,
    })
    .copied()*/
    // optimization: assume sorted list, so return first non-negative hit
    xs.iter().filter(|i| i.t >= 0.0).next().copied()
}

pub fn origin_object<'a>(xs: &[Intersection<'a>]) -> Option<&'a Shape> {
    let mut containers: Vec<&Shape> = vec![];

    for i in xs {
        if i.t > 0.0 {
            return containers.pop();
        }

        if let Some(idx) = containers
            .iter()
            .position(|&shape| std::ptr::eq(shape, i.obj))
        {
            containers.remove(idx);
        } else {
            containers.push(i.obj);
        }
    }

    None
}

pub struct IntersectionState<'a> {
    pub t: f64,
    pub obj: &'a Shape,
    pub inside: bool,
    pub point: Point,
    pub over_point: Point,
    pub under_point: Point,
    pub eyev: Vector,
    pub normalv: Vector,
    pub reflectv: Vector,
    pub n1: f64,
    pub n2: f64,
    pub mat1: Option<&'a dyn Material>,
    pub mat2: Option<&'a dyn Material>,
}

impl IntersectionState<'_> {
    pub fn schlick(&self) -> f64 {
        schlick(self.eyev, self.normalv, self.n1, self.n2)
    }
}

pub fn schlick(eyev: Vector, normalv: Vector, n1: f64, n2: f64) -> f64 {
    let mut cos_en = eyev.dot(&normalv);

    if n1 > n2 {
        let n = n1 / n2;
        let sin2_t = n * n * (1.0 - cos_en * cos_en);
        if sin2_t > 1.0 {
            return 1.0;
        }

        let cos_t = (1.0 - sin2_t).sqrt();
        cos_en = cos_t
    }

    let r0 = sqr((n1 - n2) / (n1 + n2));

    let tmp = 1.0 - cos_en;
    let tmp_pow_2 = tmp * tmp;
    let tmp_pow_5 = tmp_pow_2 * tmp_pow_2 * tmp;
    r0 + (1.0 - r0) * tmp_pow_5
}

fn sqr(x: f64) -> f64 {
    x * x
}

pub fn sort_intersections(mut xs: Vec<Intersection>) -> Vec<Intersection> {
    xs.sort_unstable_by(|a, b| {
        a.t.partial_cmp(&b.t)
            .expect("Unable to compare intersection distances")
    });
    xs
}

#[macro_export]
macro_rules! intersections {
    ($($x:expr),* $(,)?) => {{
        crate::ray::sort_intersections(vec![$($x),*])
    }}
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::approx_eq::ApproximateEq;
    use crate::materials::Phong;
    use crate::matrix::{rotation_x, scaling, translation};
    use crate::shapes::{cube, glass_sphere, plane, sphere, triangle};
    use crate::tuple::{point, vector};
    use std::f32::consts::PI;
    use std::f64::consts::{FRAC_1_SQRT_2, SQRT_2};

    /// Creating and querying a ray
    #[test]
    fn rays() {
        let origin = point(1, 2, 3);
        let direction = vector(4, 5, 6);
        let r = Ray::new(origin, direction);
        assert_eq!(r.origin(), origin);
        assert_eq!(r.direction(), direction);
    }

    /// Computing a point from a distance
    #[test]
    fn ray_position() {
        let r = Ray::new(point(2, 3, 4), vector(1, 0, 0));
        assert_eq!(r.position(0.0), point(2, 3, 4));
        assert_eq!(r.position(1.0), point(3, 3, 4));
        assert_eq!(r.position(-1.0), point(1, 3, 4));
        assert_eq!(r.position(2.5), point(4.5, 3, 4));
    }

    /// An intersection encapsulates t and object
    #[test]
    fn intersection() {
        let s = sphere();
        let i = Intersection::new(3.5, &s);
        assert_eq!(i.t, 3.5);
        assert_eq!(i.obj as *const _, &s as *const _);
    }

    /// Aggregating intersections
    #[test]
    fn intersections() {
        let s = sphere();
        let i1 = Intersection::new(1.0, &s);
        let i2 = Intersection::new(2.0, &s);
        let xs = intersections![i1, i2];
        assert_eq!(xs.len(), 2);
        assert_eq!(xs[0].t, 1.0);
        assert_eq!(xs[1].t, 2.0);
    }

    /// The hit, when all intersections have positive t
    #[test]
    fn hit_positive() {
        let s = sphere();
        let i1 = Intersection::new(1.0, &s);
        let i2 = Intersection::new(2.0, &s);
        let xs = intersections![i2, i1];
        let i = hit(&xs);
        assert_eq!(i, Some(i1));
    }

    /// The hit, when some intersections have negative t
    #[test]
    fn hit_part_negative() {
        let s = sphere();
        let i1 = Intersection::new(-1.0, &s);
        let i2 = Intersection::new(1.0, &s);
        let xs = intersections![i2, i1];
        let i = hit(&xs);
        assert_eq!(i, Some(i2));
    }

    /// The hit, when all intersections have negative t
    #[test]
    fn hit_negative() {
        let s = sphere();
        let i1 = Intersection::new(-2.0, &s);
        let i2 = Intersection::new(-1.0, &s);
        let xs = intersections![i2, i1];
        let i = hit(&xs);
        assert_eq!(i, None);
    }

    /// The hit is always the lowest nonnegative intersection
    #[test]
    fn hit_lowest_positive() {
        let s = sphere();
        let i1 = Intersection::new(5.0, &s);
        let i2 = Intersection::new(7.0, &s);
        let i3 = Intersection::new(-3.0, &s);
        let i4 = Intersection::new(2.0, &s);
        let xs = intersections![i1, i2, i3, i4];
        let i = hit(&xs);
        assert_eq!(i, Some(i4));
    }

    /// Translating a ray
    #[test]
    fn translate() {
        let r = Ray::new(point(1, 2, 3), vector(0, 1, 0));
        let m = translation(3, 4, 5);
        let r2 = r.transform(m);
        assert_eq!(r2.origin(), point(4, 6, 8));
        assert_eq!(r2.direction(), vector(0, 1, 0));
    }

    /// Scaling a ray
    #[test]
    fn scale() {
        let r = Ray::new(point(1, 2, 3), vector(0, 1, 0));
        let m = scaling(2, 3, 4);
        let r2 = r.transform(m);
        assert_eq!(r2.origin(), point(2, 6, 12));
        assert_eq!(r2.direction(), vector(0, 3, 0));
    }

    /// Rotating a ray
    #[test]
    fn rotate() {
        let r = Ray::new(point(1, 2, 3), vector(0, 1, 0));
        let m = rotation_x(PI / 2.0);
        let r2 = r.transform(m);
        assert_eq!(r2.origin(), point(1, -3, 2));
        assert_eq!(r2.direction(), vector(0, 0, 1));
    }

    /// Precomputing the state of an intersection
    #[test]
    fn precompute_outside() {
        let r = Ray::new(point(0, 0, -5), vector(0, 0, 1));
        let shape = sphere();
        let i = Intersection::new(4.0, &shape);
        let xs = [i];
        let comps = i.prepare_computations(&r, &xs);
        assert_eq!(comps.t, i.t);
        assert_eq!(comps.obj as *const _, i.obj as *const _);
        assert!(!comps.inside);
        assert_almost_eq!(comps.point, point(0, 0, -1));
        assert_almost_eq!(comps.eyev, vector(0, 0, -1));
        assert_almost_eq!(comps.normalv, vector(0, 0, -1));
    }

    /// The hit, when an intersection occurs on the inside
    #[test]
    fn precompute_inside() {
        let r = Ray::new(point(0, 0, 0), vector(0, 0, 1));
        let shape = sphere();
        let i = Intersection::new(1.0, &shape);
        let xs = [i];
        let comps = i.prepare_computations(&r, &xs);
        assert!(comps.inside);
        assert_almost_eq!(comps.point, point(0, 0, 1));
        assert_almost_eq!(comps.normalv, vector(0, 0, -1)); // inverted!
    }

    /// The hit should offset the point
    #[test]
    fn offset() {
        let r = Ray::new(point(0, 0, -5), vector(0, 0, 1));
        let shape = sphere().with_transform(translation(0, 0, 1));
        let i = Intersection::new(5.0, &shape);
        let xs = [i];
        let comps = i.prepare_computations(&r, &xs);
        assert!(comps.over_point.z() < -EPSILON / 2.0);
        assert!(comps.over_point.z() < comps.point.z());
    }

    /// Precomputing the reflection vector
    #[test]
    fn reflection_vector() {
        let shape = plane();
        let r = Ray::new(point(0, 1, -1), vector(0, -FRAC_1_SQRT_2, FRAC_1_SQRT_2));
        let i = Intersection::new(SQRT_2, &shape);
        let xs = [i];
        let comps = i.prepare_computations(&r, &xs);
        assert_almost_eq!(comps.reflectv, vector(0, FRAC_1_SQRT_2, FRAC_1_SQRT_2));
    }

    /// Finding n1 and n2 at various intersections
    #[test]
    fn refractive_index_at_intersections() {
        let glass = glass_sphere()
            .material()
            .as_any()
            .downcast_ref::<Phong>()
            .unwrap()
            .clone();

        let mut a = glass_sphere().with_transform(scaling(2, 2, 2));
        a.set_material(glass.clone().with_refractive_index(1.5));

        let mut b = glass_sphere().with_transform(translation(0, 0, -0.25));
        b.set_material(glass.clone().with_refractive_index(2.0));

        let mut c = glass_sphere().with_transform(translation(0, 0, 0.25));
        c.set_material(glass.clone().with_refractive_index(2.5));

        let r = Ray::new(point(0, 0, -4), vector(0, 0, 1));
        let xs = intersections![
            Intersection::new(2.0, &a),
            Intersection::new(2.75, &b),
            Intersection::new(3.25, &c),
            Intersection::new(4.75, &b),
            Intersection::new(5.25, &c),
            Intersection::new(6.0, &a)
        ];

        for (index, n1, n2) in vec![
            (0, 1.0, 1.5),
            (1, 1.5, 2.0),
            (2, 2.0, 2.5),
            (3, 2.5, 2.5),
            (4, 2.5, 1.5),
            (5, 1.5, 1.0),
        ] {
            let comps = xs[index].prepare_computations(&r, &xs);

            assert_almost_eq!(comps.n1, n1);
            assert_almost_eq!(comps.n2, n2);
        }
    }

    /// The under point is offset below the surface
    #[test]
    fn under_point() {
        let r = Ray::new(point(0, 0, -5), vector(0, 0, 1));
        let shape = glass_sphere().with_transform(translation(0, 0, 1));
        let i = Intersection::new(5.0, &shape);
        let xs = [i];
        let comps = i.prepare_computations(&r, &xs);
        assert!(comps.under_point.z() > EPSILON / 2.0);
        assert!(comps.under_point.z() > comps.point.z());
    }

    /// The Schlick approximation under total internal reflection
    #[test]
    fn schlick_internal() {
        let shape = glass_sphere();
        let ray = Ray::new(point(0, 0, FRAC_1_SQRT_2), vector(0, 1, 0));
        let xs = intersections![
            Intersection::new(-FRAC_1_SQRT_2, &shape),
            Intersection::new(FRAC_1_SQRT_2, &shape)
        ];
        let comps = xs[1].prepare_computations(&ray, &xs);
        let reflectance = comps.schlick();
        assert_almost_eq!(reflectance, 1.0)
    }

    /// The Schlick approximation with a perpendicular viewing angle
    #[test]
    fn schlick_steep() {
        let shape = glass_sphere();
        let ray = Ray::new(point(0, 0, 0), vector(0, 1, 0));
        let xs = intersections![
            Intersection::new(-1.0, &shape),
            Intersection::new(1.0, &shape)
        ];
        let comps = xs[1].prepare_computations(&ray, &xs);
        let reflectance = comps.schlick();
        assert_almost_eq!(reflectance, 0.04)
    }

    /// The Schlick approximation with a small angle and n2 > n1
    #[test]
    fn schlick_flat() {
        let shape = glass_sphere();
        let ray = Ray::new(point(0, 0.99, -2), vector(0, 0, 1));
        let xs = intersections![Intersection::new(1.8589, &shape)];
        let comps = xs[0].prepare_computations(&ray, &xs);
        let reflectance = comps.schlick();
        assert_almost_eq!(reflectance, 0.48873)
    }

    /// An intersection can encapsulate u and v
    #[test]
    fn intersecion_with_uv() {
        let s = triangle(point(0, 1, 0), point(-1, 0, 0), point(1, 0, 0));
        let i = Intersection::new_uv(3.5, 0.2, 0.4, &s);
        assert_eq!(i.u, 0.2);
        assert_eq!(i.v, 0.4);
    }

    /// A ray that does not hit anything cannot start inside an object
    #[test]
    fn ray_hits_nothing_has_no_originating_object() {
        let xs = intersections![];
        assert!(origin_object(&xs).is_none());
    }

    /// A ray that starts outside an object
    #[test]
    fn ray_starts_outside_an_object() {
        let shape = sphere();
        let xs = intersections![
            Intersection::new(1.0, &shape),
            Intersection::new(3.0, &shape)
        ];
        assert!(origin_object(&xs).is_none());
    }

    /// A ray that starts inside an object
    #[test]
    fn ray_starts_inside_an_object() {
        let shape = sphere();
        let xs = intersections![
            Intersection::new(-1.0, &shape),
            Intersection::new(1.0, &shape)
        ];
        assert_almost_eq!(origin_object(&xs).unwrap(), &shape);
    }

    /// A ray that starts between two bjects
    #[test]
    fn ray_starts_between_objects() {
        let a = sphere();
        let b = sphere();
        let xs = intersections![
            Intersection::new(-3.0, &a),
            Intersection::new(-1.0, &a),
            Intersection::new(1.0, &b),
            Intersection::new(3.0, &b)
        ];
        assert!(origin_object(&xs).is_none());
    }

    /// A ray that starts inside two objects
    #[test]
    fn ray_starts_inside_two_objects() {
        let a = sphere();
        let b = cube();
        let xs = intersections![
            Intersection::new(-2.0, &a),
            Intersection::new(-1.0, &b),
            Intersection::new(1.0, &b),
            Intersection::new(2.0, &a)
        ];
        assert_almost_eq!(origin_object(&xs).unwrap(), &b);
    }

    /// A ray that starts inside two overlapping objects
    #[test]
    fn ray_starts_inside_overlapping_objects() {
        let a = sphere();
        let b = cube();
        let xs = intersections![
            Intersection::new(-2.0, &a),
            Intersection::new(-1.0, &b),
            Intersection::new(1.0, &a),
            Intersection::new(2.0, &b)
        ];
        assert_almost_eq!(origin_object(&xs).unwrap(), &b);
    }
}
