use crate::aabb::Aabb;
use crate::matrix::Matrix;
use crate::ray::{sort_intersections, Intersection, Ray};
use crate::shapes::{SceneItem, Shape};

pub fn csg_union(s1: impl Into<SceneItem>, s2: impl Into<SceneItem>) -> CsgPair {
    CsgPair::new(CsgOp::Union, s1, s2)
}

pub fn csg_difference(s1: impl Into<SceneItem>, s2: impl Into<SceneItem>) -> CsgPair {
    CsgPair::new(CsgOp::Difference, s1, s2)
}

pub fn csg_intersection(s1: impl Into<SceneItem>, s2: impl Into<SceneItem>) -> CsgPair {
    CsgPair::new(CsgOp::Intersection, s1, s2)
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum CsgOp {
    Union,
    Intersection,
    Difference,
}

impl CsgOp {
    fn eval(&self, lhit: bool, inl: bool, inr: bool) -> bool {
        match self {
            CsgOp::Union => lhit && !inr || !lhit && !inl,
            CsgOp::Intersection => lhit && inr || !lhit && inl,
            CsgOp::Difference => lhit && !inr || !lhit && inl,
        }
    }
}

#[derive(Debug, Clone)]
pub struct CsgPair {
    transform: Matrix,
    cumulative_transform: Matrix,
    inv_cumulative_transform: Matrix,
    cast_shadow: bool,
    op: CsgOp,
    items: Box<(SceneItem, SceneItem)>,
}

impl CsgPair {
    pub fn new(op: CsgOp, s1: impl Into<SceneItem>, s2: impl Into<SceneItem>) -> Self {
        CsgPair {
            transform: Matrix::identity(),
            cumulative_transform: Matrix::identity(),
            inv_cumulative_transform: Matrix::identity(),
            cast_shadow: true,
            op,
            items: Box::new((s1.into(), s2.into())),
        }
    }

    pub fn with_cast_shadow(self, cast_shadow: bool) -> Self {
        CsgPair {
            cast_shadow,
            ..self
        }
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
        self.items.0.update_transform(self.cumulative_transform);
        self.items.1.update_transform(self.cumulative_transform);
    }

    pub fn update_aabb(&mut self) -> Aabb {
        self.items
            .0
            .update_aabb()
            .merge(&self.items.1.update_aabb())
    }

    pub fn aabb(&self) -> Aabb {
        self.items.0.aabb().merge(&self.items.1.aabb())
    }

    pub fn cast_shadow(&self) -> bool {
        self.cast_shadow
    }

    pub fn filter_intersections<'a>(&self, xs: Vec<Intersection<'a>>) -> Vec<Intersection<'a>> {
        let mut inl = false;
        let mut inr = false;
        xs.into_iter()
            .filter(|i| {
                let lhit = self.items.0.contains(i.obj);
                let filt = self.op.eval(lhit, inl, inr);
                if lhit {
                    inl = !inl;
                } else {
                    inr = !inr;
                }
                filt
            })
            .collect()
    }

    pub fn is_similar(&self, other: &CsgPair) -> bool {
        self.op == other.op
            && self.items.0.is_similar(&other.items.0)
            && self.items.1.is_similar(&other.items.1)
    }

    pub fn intersect<'a>(&'a self, ray: &Ray) -> Vec<Intersection<'a>> {
        let mut xs = self.items.0.intersect(ray);
        xs.extend(self.items.1.intersect(ray));
        self.filter_intersections(sort_intersections(xs))
    }

    pub fn contains(&self, shape: &Shape) -> bool {
        self.items.0.contains(shape) || self.items.1.contains(shape)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::approx_eq::ApproximateEq;
    use crate::matrix::translation;
    use crate::shapes::{cube, group, sphere};
    use crate::tuple::{point, vector};

    // CSG is created with an operation and two shapes
    #[test]
    fn csg_object() {
        let s1 = sphere();
        let s2 = cube();
        let c = CsgPair::new(CsgOp::Union, s1.clone(), s2.clone());

        assert_eq!(c.op, CsgOp::Union);
        assert_almost_eq!(*c.items.0.as_shape().unwrap(), s1);
        assert_almost_eq!(*c.items.1.as_shape().unwrap(), s2);
    }

    /// Evaluating the rule for a csg operation
    #[test]
    fn csg_rules() {
        for (op, lhit, inl, inr, expected) in vec![
            (CsgOp::Union, true, true, true, false),
            (CsgOp::Union, true, true, false, true),
            (CsgOp::Union, true, false, true, false),
            (CsgOp::Union, true, false, false, true),
            (CsgOp::Union, false, true, true, false),
            (CsgOp::Union, false, true, false, false),
            (CsgOp::Union, false, false, true, true),
            (CsgOp::Union, false, false, false, true),
            (CsgOp::Intersection, true, true, true, true),
            (CsgOp::Intersection, true, true, false, false),
            (CsgOp::Intersection, true, false, true, true),
            (CsgOp::Intersection, true, false, false, false),
            (CsgOp::Intersection, false, true, true, true),
            (CsgOp::Intersection, false, true, false, true),
            (CsgOp::Intersection, false, false, true, false),
            (CsgOp::Intersection, false, false, false, false),
            (CsgOp::Difference, true, true, true, false),
            (CsgOp::Difference, true, true, false, true),
            (CsgOp::Difference, true, false, true, false),
            (CsgOp::Difference, true, false, false, true),
            (CsgOp::Difference, false, true, true, true),
            (CsgOp::Difference, false, true, false, true),
            (CsgOp::Difference, false, false, true, false),
            (CsgOp::Difference, false, false, false, false),
        ] {
            let result = op.eval(lhit, inl, inr);
            assert_eq!(result, expected);
        }
    }

    /// Filtering a list of csg-intersections
    #[test]
    fn filter_intersections() {
        for (op, x0, x1) in vec![
            (CsgOp::Union, 0, 3),
            (CsgOp::Intersection, 1, 2),
            (CsgOp::Difference, 0, 1),
        ] {
            let c = CsgPair::new(op, sphere(), cube());
            let s1 = c.items.0.as_shape().unwrap();
            let s2 = c.items.1.as_shape().unwrap();
            let xs = intersections![
                Intersection::new(1.0, s1),
                Intersection::new(2.0, s2),
                Intersection::new(3.0, s1),
                Intersection::new(4.0, s2),
            ];
            let result = c.filter_intersections(xs.clone());
            assert_eq!(result.len(), 2);
            assert_eq!(result[0], xs[x0]);
            assert_eq!(result[1], xs[x1]);
        }
    }

    /// Filtering a list of csg-intersections with groups
    #[test]
    fn filter_group_intersections() {
        for (op, x0, x1) in vec![
            (CsgOp::Union, 0, 3),
            (CsgOp::Intersection, 1, 2),
            (CsgOp::Difference, 0, 1),
        ] {
            let mut g1 = group();
            g1.add_child(sphere());
            let mut g2 = group();
            g2.add_child(cube());
            let c = CsgPair::new(op, g1, g2);
            let s1 = c
                .items
                .0
                .as_group()
                .unwrap()
                .get_child(0)
                .as_shape()
                .unwrap();
            let s2 = c
                .items
                .1
                .as_group()
                .unwrap()
                .get_child(0)
                .as_shape()
                .unwrap();
            let xs = intersections![
                Intersection::new(1.0, s1),
                Intersection::new(2.0, s2),
                Intersection::new(3.0, s1),
                Intersection::new(4.0, s2),
            ];
            let result = c.filter_intersections(xs.clone());
            assert_eq!(result.len(), 2);
            assert_eq!(result[0], xs[x0]);
            assert_eq!(result[1], xs[x1]);
        }
    }

    /// Filtering a list of csg-intersections with sub-csgs
    #[test]
    fn filter_sub_csg_intersections() {
        for (op, x0, x1) in vec![
            (CsgOp::Union, 0, 3),
            (CsgOp::Intersection, 1, 2),
            (CsgOp::Difference, 0, 1),
        ] {
            let c1 = csg_union(sphere(), sphere());
            let c2 = csg_union(sphere(), sphere());
            let c = CsgPair::new(op, c1, c2);
            let s1 = c.items.0.as_csg().unwrap().items.0.as_shape().unwrap();
            let s2 = c.items.1.as_csg().unwrap().items.1.as_shape().unwrap();
            let xs = intersections![
                Intersection::new(1.0, s1),
                Intersection::new(2.0, s2),
                Intersection::new(3.0, s1),
                Intersection::new(4.0, s2),
            ];
            let result = c.filter_intersections(xs.clone());
            assert_eq!(result.len(), 2);
            assert_eq!(result[0], xs[x0]);
            assert_eq!(result[1], xs[x1]);
        }
    }

    /// A ray misses a CSG object
    #[test]
    fn intersect_miss() {
        let c = csg_union(sphere(), cube());
        let r = Ray::new(point(0, 2, -5), vector(0, 0, 1));
        let xs = c.intersect(&r);
        assert!(xs.is_empty());
    }

    /// A ray hits a CSG object
    #[test]
    fn intersect_hit() {
        let s1 = sphere();
        let s2 = sphere().with_transform(translation(0, 0, 0.5));
        let c = CsgPair::new(CsgOp::Union, s1, s2);
        let r = Ray::new(point(0, 0, -5), vector(0, 0, 1));
        let xs = c.intersect(&r);
        assert_eq!(xs.len(), 2);
        assert_almost_eq!(xs[0].t, 4.0);
        assert!(std::ptr::eq(xs[0].obj, c.items.0.as_shape().unwrap()));
        assert_almost_eq!(xs[1].t, 6.5);
        assert!(std::ptr::eq(xs[1].obj, c.items.1.as_shape().unwrap()));
    }
}
