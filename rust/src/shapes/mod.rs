mod cone;
mod cube;
mod cylinder;
pub mod planar_heightmap;
mod plane;
mod sphere;
mod triangle;

pub use cone::cone;
pub use cube::cube;
pub use cylinder::{cylinder, Cylinder};
pub use planar_heightmap::planar_heightmap;
pub use plane::plane;
pub use sphere::{glass_sphere, sphere};
pub use triangle::{triangle, Triangle};

use crate::aabb::Aabb;
use crate::approx_eq::ApproximateEq;
use crate::color::Color;
use crate::materials::Phong;
use crate::matrix::Matrix;
use crate::pattern::Pattern;
use crate::ray::{sort_intersections, Intersection, Ray};
use crate::tuple::{Point, Vector};
use std::any::Any;

pub fn group() -> Group {
    Group::default()
}

pub fn bounding_group() -> BoundingGroup {
    BoundingGroup::default()
}

pub trait Geometry: 'static + AsAny + std::fmt::Debug + Sync {
    fn duplicate(&self) -> Box<dyn Geometry>;
    //fn as_any(&self) -> &dyn Any;// { self }
    fn is_similar(&self, other: &dyn Geometry) -> bool;
    fn intersect<'a>(&self, obj: &'a Shape, local_ray: &Ray) -> Vec<Intersection<'a>>;
    fn normal_at(&self, local_point: Point) -> Vector;
    fn aabb(&self) -> Aabb;
}

#[derive(Debug, Clone)]
pub enum SceneItem {
    Primitive(Shape),
    Compound(Group),
    Bounded(BoundingGroup),
}

impl SceneItem {
    pub fn as_shape(&self) -> Option<&Shape> {
        match self {
            SceneItem::Primitive(shape) => Some(shape),
            _ => None,
        }
    }

    pub fn as_shape_mut(&mut self) -> Option<&mut Shape> {
        match self {
            SceneItem::Primitive(shape) => Some(shape),
            _ => None,
        }
    }
    pub fn as_group(&self) -> Option<&Group> {
        match self {
            SceneItem::Compound(group) => Some(group),
            _ => None,
        }
    }

    pub fn as_group_shape_mut(&mut self) -> Option<&mut Group> {
        match self {
            SceneItem::Compound(group) => Some(group),
            _ => None,
        }
    }

    pub fn with_transform(self, t: Matrix) -> Self {
        use SceneItem::*;
        match self {
            Primitive(shape) => Primitive(shape.with_transform(t)),
            Compound(group) => Compound(group.with_transform(t)),
            Bounded(group) => Bounded(group.with_transform(t)),
        }
    }

    pub fn intersect(&self, world_ray: &Ray) -> Vec<Intersection> {
        match self {
            SceneItem::Primitive(shape) => shape.intersect(world_ray),
            SceneItem::Compound(group) => group.intersect(world_ray),
            SceneItem::Bounded(group) => group.intersect(world_ray),
        }
    }

    pub fn cast_shadow(&self) -> bool {
        match self {
            SceneItem::Primitive(shape) => shape.cast_shadow(),
            SceneItem::Compound(group) => group.cast_shadow,
            SceneItem::Bounded(bgroup) => bgroup.group.cast_shadow,
        }
    }

    pub fn update_transform(&mut self, t: Matrix) {
        match self {
            SceneItem::Primitive(shape) => shape.update_transform(t),
            SceneItem::Compound(group) => group.update_transform(t),
            SceneItem::Bounded(group) => group.group.update_transform(t),
        }
    }

    pub fn is_similar(&self, other: &Self) -> bool {
        use SceneItem::*;
        match (self, other) {
            (Primitive(a), Primitive(b)) => a.is_similar(b),
            (Compound(a), Compound(b)) => a.is_similar(b),
            (Compound(g), Primitive(s)) | (Primitive(s), Compound(g)) => {
                is_group_similar_to_shape(g, s)
            }
            (Bounded(a), Bounded(b)) => a.group.is_similar(&b.group),
            (Bounded(b), Compound(g)) | (Compound(g), Bounded(b)) => b.group.is_similar(g),
            (Bounded(b), Primitive(s)) | (Primitive(s), Bounded(b)) => {
                is_group_similar_to_shape(&b.group, s)
            }
        }
    }

    pub fn update_aabb(&mut self) -> Aabb {
        match self {
            SceneItem::Primitive(shape) => shape.aabb(),
            SceneItem::Compound(group) => group.aabb(),
            SceneItem::Bounded(bgroup) => bgroup.update_aabb().clone(),
        }
    }
}

pub fn is_group_similar_to_shape(g: &Group, s: &Shape) -> bool {
    g.items.len() == 1
        && g.transform.is_identity()
        && match &g.items[0] {
            SceneItem::Primitive(gs) => gs.is_similar(s),
            SceneItem::Compound(gg) => is_group_similar_to_shape(gg, s),
            SceneItem::Bounded(bg) => is_group_similar_to_shape(&bg.group, s),
        }
}

impl From<Shape> for SceneItem {
    fn from(shape: Shape) -> SceneItem {
        SceneItem::Primitive(shape)
    }
}

impl From<Group> for SceneItem {
    fn from(group: Group) -> SceneItem {
        SceneItem::Compound(group)
    }
}

impl From<BoundingGroup> for SceneItem {
    fn from(group: BoundingGroup) -> SceneItem {
        SceneItem::Bounded(group)
    }
}

#[derive(Debug)]
pub struct Shape {
    material: Phong,
    transform: Matrix,
    cumulative_transform: Matrix,
    inv_cumulative_transform: Matrix,
    cast_shadow: bool,
    geometry: Box<dyn Geometry>,
}

impl Clone for Shape {
    fn clone(&self) -> Self {
        Shape {
            material: self.material.clone(),
            transform: self.transform,
            cumulative_transform: self.cumulative_transform,
            inv_cumulative_transform: self.inv_cumulative_transform,
            cast_shadow: self.cast_shadow,
            geometry: self.geometry.duplicate(),
        }
    }
}

impl Shape {
    pub fn new(geometry: impl Geometry) -> Self {
        Shape {
            material: Phong::default(),
            transform: Matrix::identity(),
            cumulative_transform: Matrix::identity(),
            inv_cumulative_transform: Matrix::identity(),
            cast_shadow: true,
            geometry: Box::new(geometry),
        }
    }

    pub fn intersect(&self, world_ray: &Ray) -> Vec<Intersection> {
        self.geometry
            .intersect(&self, &world_ray.transform(*self.inv_transform()))
    }

    pub fn local_intersect(&self, local_ray: &Ray) -> Vec<Intersection> {
        self.geometry
            .intersect(&self, local_ray)
    }

    pub fn normal_at(&self, world_point: Point) -> Vector {
        let obj_point = self.world_to_object(world_point);
        let obj_normal = self.geometry.normal_at(obj_point);
        self.normal_to_world(obj_normal)
    }

    pub fn pattern_at(&self, pattern: &Pattern, world_point: Point) -> Color {
        pattern.at(self.world_to_object(world_point))
    }

    pub fn with_material(self, material: Phong) -> Self {
        Shape { material, ..self }
    }

    pub fn with_transform(self, transform: Matrix) -> Self {
        Shape {
            transform,
            cumulative_transform: transform,
            inv_cumulative_transform: transform.inverse(),
            ..self
        }
    }

    pub fn material(&self) -> &Phong {
        &self.material
    }

    pub fn material_mut(&mut self) -> &mut Phong {
        &mut self.material
    }

    pub fn set_material(&mut self, material: Phong) {
        self.material = material;
    }

    pub fn inv_transform(&self) -> &Matrix {
        &self.inv_cumulative_transform
    }

    pub fn set_transform(&mut self, t: Matrix) {
        self.transform = t;
        self.cumulative_transform = t;
        self.inv_cumulative_transform = t.inverse();
    }

    pub fn update_transform(&mut self, t: Matrix) {
        self.cumulative_transform = t * self.transform;
        self.inv_cumulative_transform = self.cumulative_transform.inverse();
    }

    pub fn world_to_object(&self, p: Point) -> Point {
        self.inv_cumulative_transform * p
    }

    pub fn normal_to_world(&self, obj_normal: Vector) -> Vector {
        let world_normal = self.inv_cumulative_transform.transpose() * obj_normal;
        Vector::new(world_normal.x(), world_normal.y(), world_normal.z()).normalized()
    }

    pub fn cast_shadow(&self) -> bool {
        self.cast_shadow
    }

    pub fn set_cast_shadow(&mut self, b: bool) {
        self.cast_shadow = b
    }

    pub fn with_cast_shadow(self, cast_shadow: bool) -> Self {
        Shape {
            cast_shadow,
            ..self
        }
    }

    pub fn is_similar(&self, other: &Self) -> bool {
        self.geometry.is_similar(&*other.geometry)
            && self.material.approx_eq(&other.material)
            && self.transform.approx_eq(&other.transform)
    }

    fn aabb(&self) -> Aabb {
        self.geometry.aabb().transform(self.cumulative_transform)
    }
}

#[derive(Debug, Clone)]
pub struct Group {
    transform: Matrix,
    cumulative_transform: Matrix,
    inv_cumulative_transform: Matrix,
    cast_shadow: bool,
    items: Vec<SceneItem>,
}

impl Default for Group {
    fn default() -> Self {
        Group {
            transform: Matrix::identity(),
            cumulative_transform: Matrix::identity(),
            inv_cumulative_transform: Matrix::identity(),
            cast_shadow: true,
            items: vec![],
        }
    }
}

impl Group {
    pub fn is_similar(&self, other: &Self) -> bool {
        self.transform.approx_eq(&other.transform)
            && self
                .items
                .iter()
                .zip(&other.items)
                .all(|(a, b)| a.is_similar(b))
    }

    pub fn aint_empty(&self) -> bool {
        !self.items.is_empty()
    }

    pub fn add_child(&mut self, child: impl Into<SceneItem>) {
        let child = child.into();
        self.items.push(child);
    }

    pub fn get_child(&self, idx: usize) -> &SceneItem {
        &self.items[idx]
    }

    pub fn intersect(&self, world_ray: &Ray) -> Vec<Intersection> {
        let mut xs = vec![];
        for obj in &self.items {
            xs.extend(obj.intersect(world_ray));
        }
        sort_intersections(xs)
    }

    pub fn with_transform(mut self, t: Matrix) -> Self {
        self.set_transform(t);
        self
    }

    pub fn set_transform(&mut self, t: Matrix) {
        self.transform = t;
        self.cumulative_transform = t;
        self.inv_cumulative_transform = t.inverse();
    }

    pub fn update_transform(&mut self, t: Matrix) {
        self.cumulative_transform = t * self.transform;
        self.inv_cumulative_transform = self.cumulative_transform.inverse();
        for child in &mut self.items {
            child.update_transform(self.cumulative_transform)
        }
    }

    pub fn aabb(&mut self) -> Aabb {
        let mut aabb = Aabb::empty();
        for child in &mut self.items {
            aabb = aabb.merge(&child.update_aabb());
        }
        aabb
    }
}

#[derive(Debug, Default, Clone)]
pub struct BoundingGroup {
    pub(crate) group: Group,
    pub(crate) aabb: Aabb,
}

impl BoundingGroup {
    pub fn add_child(&mut self, child: impl Into<SceneItem>) {
        self.group.add_child(child);
    }

    pub fn with_transform(self, transform: Matrix) -> Self {
        BoundingGroup {
            group: self.group.with_transform(transform),
            ..self
        }
    }

    pub fn update_aabb(&mut self) -> &Aabb {
        let mut aabb = Aabb::empty();
        for child in &mut self.group.items {
            aabb = aabb.merge(&child.update_aabb());
        }
        //aabb = aabb.transform(self.group.inv_cumulative_transform);
        self.aabb = aabb;
        &self.aabb
    }

    pub fn intersect(&self, world_ray: &Ray) -> Vec<Intersection> {
        if self.aabb.intersect(world_ray).is_some() {
            self.group.intersect(world_ray)
        } else {
            vec![]
        }
    }
}

impl From<Group> for BoundingGroup {
    fn from(group: Group) -> Self {
        BoundingGroup {
            group,
            aabb: Aabb::default(),
        }
    }
}

/*impl PartialEq for dyn Shape + '_ {
    fn eq(&self, other: &Self) -> bool {
        self as *const _ == other as *const _
    }
}*/

pub trait AsAny: Any {
    fn as_any(&self) -> &dyn Any;
}

impl<T: Any> AsAny for T {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::approx_eq::ApproximateEq;
    use crate::matrix::{rotation_x, rotation_y, rotation_z, scaling, translation};
    use crate::tuple::{point, vector};
    use std::f64::consts::{FRAC_1_SQRT_2, PI};
    use std::sync::{Arc, RwLock};

    fn test_shape() -> Shape {
        Shape::new(TestGeometry::new())
    }

    #[derive(Debug, Clone)]
    struct TestGeometry {
        last_ray: Arc<RwLock<Option<Ray>>>,
    }

    impl TestGeometry {
        fn new() -> Self {
            TestGeometry {
                last_ray: Arc::new(RwLock::new(None)),
            }
        }
    }

    impl Geometry for TestGeometry {
        fn duplicate(&self) -> Box<dyn Geometry> {
            Box::new(self.clone())
        }

        fn is_similar(&self, _: &dyn Geometry) -> bool {
            true
        }

        fn intersect<'a>(&self, _: &'a Shape, local_ray: &Ray) -> Vec<Intersection<'a>> {
            *self.last_ray.write().unwrap() = Some(local_ray.clone());
            vec![]
        }

        fn normal_at(&self, obj_point: Point) -> Vector {
            vector(obj_point.x(), obj_point.y(), obj_point.z())
        }

        fn aabb(&self) -> Aabb {
            unimplemented!()
        }
    }

    /// Intersecting a scaled shape with a ray
    #[test]
    fn intersect_scaled() {
        let r = Ray::new(point(0, 0, -5), vector(0, 0, 1));
        let mut s = test_shape();
        s.set_transform(scaling(2, 2, 2));
        s.intersect(&r);

        let g = (*s.geometry)
            .as_any()
            .downcast_ref::<TestGeometry>()
            .unwrap();
        assert_almost_eq!(
            g.last_ray.read().unwrap().as_ref().unwrap().origin(),
            point(0, 0, -2.5)
        );
        assert_almost_eq!(
            g.last_ray.read().unwrap().as_ref().unwrap().direction(),
            vector(0, 0, 0.5)
        );
    }

    /// Intersecting a translated shape with a ray
    #[test]
    fn intersect_translated() {
        let r = Ray::new(point(0, 0, -5), vector(0, 0, 1));
        let mut s = test_shape();
        s.set_transform(translation(5, 0, 0));
        s.intersect(&r);

        let g = (*s.geometry)
            .as_any()
            .downcast_ref::<TestGeometry>()
            .unwrap();
        assert_almost_eq!(
            g.last_ray.read().unwrap().as_ref().unwrap().origin(),
            point(-5, 0, -5)
        );
        assert_almost_eq!(
            g.last_ray.read().unwrap().as_ref().unwrap().direction(),
            vector(0, 0, 1)
        );
    }

    /// Intersecting a rotated shape with a ray
    #[test]
    fn intersect_rotated() {
        let r = Ray::new(point(0, 0, -5), vector(0, 0, 1));
        let mut s = test_shape();
        s.set_transform(rotation_x(1) * rotation_y(2) * rotation_z(3));
        s.intersect(&r);

        let g = (*s.geometry)
            .as_any()
            .downcast_ref::<TestGeometry>()
            .unwrap();
        assert_almost_eq!(
            g.last_ray.read().unwrap().as_ref().unwrap().origin(),
            point(-3.02564, 3.81859, 1.12423)
        );
        assert_almost_eq!(
            g.last_ray.read().unwrap().as_ref().unwrap().direction(),
            vector(0.60513, -0.76372, -0.22485)
        );
    }

    /// Computing the normal on a translated shape
    #[test]
    fn normal_translated() {
        let mut s = test_shape();
        s.set_transform(translation(0, 1, 0));
        let n = s.normal_at(point(0, 1.70711, 0.70711));
        assert_almost_eq!(n, vector(0, 0.70711, 0.70711));
    }

    /// Computing the normal on a transformed shape
    #[test]
    fn normal_transformed() {
        let mut s = test_shape();
        s.set_transform(scaling(1, 0.5, 1) * rotation_z(PI / 5.0));
        let n = s.normal_at(point(0, FRAC_1_SQRT_2, -FRAC_1_SQRT_2));
        assert_almost_eq!(n, vector(0, 0.97014, -0.24254));
    }

    /// Creating a new group
    #[test]
    fn new_group() {
        let g = group();
        assert_eq!(g.transform, Matrix::identity());
        assert!(g.items.is_empty());
    }

    /// Adding a child to a group
    #[test]
    fn add_child_to_group() {
        let mut g = group();
        let s = test_shape();
        g.add_child(s.clone());
        assert_almost_eq!(&g.items[0], s);
    }

    /// Intersecting a ray with an empty group
    #[test]
    fn intersect_empty_group() {
        let g = group();
        let r = Ray::new(point(0, 0, 0), vector(0, 0, 1));
        let xs = g.intersect(&r);
        assert!(xs.is_empty());
    }

    /// Intersecting a ray with a nonempty group
    #[test]
    fn intersect_nonempty_group() {
        let mut g = group();
        let s1 = sphere();
        let s2 = sphere().with_transform(translation(0, 0, -3));
        let s3 = sphere().with_transform(translation(5, 0, 0));
        g.add_child(s1.clone());
        g.add_child(s2.clone());
        g.add_child(s3.clone());
        let r = Ray::new(point(0, 0, 0), vector(0, 0, 1));
        let xs = g.intersect(&r);
        assert_eq!(xs.len(), 4);
        assert_almost_eq!(xs[0].obj, &s2);
        assert_almost_eq!(xs[1].obj, &s2);
        assert_almost_eq!(xs[2].obj, &s1);
        assert_almost_eq!(xs[3].obj, &s1);
    }

    /// Intersecting a transformed group
    #[test]
    fn intersect_transformed_group() {
        let mut g = group().with_transform(scaling(2, 2, 2));
        let s = sphere().with_transform(translation(5, 0, 0));
        g.add_child(s.clone());
        g.update_transform(Matrix::identity());
        let r = Ray::new(point(10, 0, -10), vector(0, 0, 1));
        let xs = g.intersect(&r);
        assert_eq!(xs.len(), 2);
    }

    /// Converting a point from world to object space
    #[test]
    fn world_point_to_object_point() {
        let mut g1 = group().with_transform(rotation_y(PI / 2.0));
        let mut g2 = group().with_transform(scaling(2, 2, 2));
        let s = sphere().with_transform(translation(5, 0, 0));
        g2.add_child(s);
        g1.add_child(g2);
        g1.update_transform(Matrix::identity());
        let s = g1.items[0].as_group().unwrap().items[0].as_shape().unwrap();
        assert_almost_eq!(s.world_to_object(point(-2, 0, -10)), point(0, 0, -1));
    }

    /// Converting a normal from object to world space
    #[test]
    fn object_normal_to_world_normal() {
        let mut g1 = group().with_transform(rotation_y(PI / 2.0));
        let mut g2 = group().with_transform(scaling(1, 2, 3));
        let s = sphere().with_transform(translation(5, 0, 0));
        g2.add_child(s);
        g1.add_child(g2);
        g1.update_transform(Matrix::identity());
        let s = g1.items[0].as_group().unwrap().items[0].as_shape().unwrap();
        let s33 = 3.0f64.sqrt() / 3.0;
        let n = s.normal_to_world(vector(s33, s33, s33));
        assert_almost_eq!(n, vector(0.28571, 0.42857, -0.85714));
    }

    /// Finding the normal on a child object
    #[test]
    fn normal_in_group() {
        let mut g1 = group().with_transform(rotation_y(PI / 2.0));
        let mut g2 = group().with_transform(scaling(1, 2, 3));
        let s = sphere().with_transform(translation(5, 0, 0));
        g2.add_child(s);
        g1.add_child(g2);
        g1.update_transform(Matrix::identity());
        let s = g1.items[0].as_group().unwrap().items[0].as_shape().unwrap();
        let n = s.normal_at(point(1.7321, 1.1547, -5.5774));
        assert_almost_eq!(n, vector(0.28570, 0.42854, -0.85716));
    }
}
