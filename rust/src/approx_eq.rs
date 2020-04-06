use crate::color::Color;
use crate::lights::PointLight;
use crate::materials::Phong;
use crate::matrix::Matrix;
use crate::shapes::Shape;
use crate::tuple::{Point, Vector};
use std::ops::Deref;

pub trait ApproximateEq<T: ?Sized = Self> {
    fn approx_eq(&self, other: &T) -> bool;
}

const EPSILON: f64 = 1e-5;

impl ApproximateEq for f64 {
    fn approx_eq(&self, other: &Self) -> bool {
        (self - other).abs() < EPSILON
    }
}

impl ApproximateEq for Point {
    fn approx_eq(&self, other: &Self) -> bool {
        self.x().approx_eq(&other.x())
            && self.y().approx_eq(&other.y())
            && self.z().approx_eq(&other.z())
            && self.w().approx_eq(&other.w())
    }
}

impl ApproximateEq for Vector {
    fn approx_eq(&self, other: &Self) -> bool {
        self.x().approx_eq(&other.x())
            && self.y().approx_eq(&other.y())
            && self.z().approx_eq(&other.z())
            && self.w().approx_eq(&other.w())
    }
}

impl ApproximateEq for Color {
    fn approx_eq(&self, other: &Self) -> bool {
        self.red().approx_eq(&other.red())
            && self.green().approx_eq(&other.green())
            && self.blue().approx_eq(&other.blue())
    }
}

impl ApproximateEq for Matrix {
    fn approx_eq(&self, other: &Self) -> bool {
        self.into_flat()
            .iter()
            .zip(other.into_flat().iter())
            .all(|(a, b)| a.approx_eq(b))
    }
}

impl ApproximateEq for Phong {
    fn approx_eq(&self, other: &Self) -> bool {
        self.color().approx_eq(&other.color())
            && self.ambient().approx_eq(&other.ambient())
            && self.diffuse().approx_eq(&other.diffuse())
            && self.specular().approx_eq(&other.specular())
    }
}

impl ApproximateEq for PointLight {
    fn approx_eq(&self, other: &Self) -> bool {
        self.position().approx_eq(&other.position())
            && self.intensity().approx_eq(&other.intensity())
    }
}

impl<T: Deref<Target = dyn Shape>> ApproximateEq<T> for dyn Shape {
    fn approx_eq(&self, other: &T) -> bool {
        self.is_similar(&**other)
    }
}

pub trait FindSimilar<T: ?Sized + ApproximateEq<Self::Item>> {
    type Item;

    fn find_similar(&self, item: &T) -> Option<&Self::Item>;

    fn contains_similar(&self, item: &T) -> bool {
        self.find_similar(item).is_some()
    }
}

impl<S, T> FindSimilar<S> for [T]
where
    S: ApproximateEq<T>,
    S: ?Sized,
{
    type Item = T;
    fn find_similar(&self, item: &S) -> Option<&T> {
        self.iter().find(|&x| item.approx_eq(x))
    }
}

/// Asserts that two expressions are equal to each other (using [`ApproximateEq`]).
///
/// On panic, this macro will print the values of the expressions with their
/// debug representations.
///
/// Like [`assert!`], this macro has a second form, where a custom
/// panic message can be provided.
///
/// [`PartialEq`]: cmp/trait.PartialEq.html
/// [`assert!`]: macro.assert.html
///
/// # Examples
///
/// ```
/// let a = 3;
/// let b = 1 + 2;
/// assert_eq!(a, b);
///
/// assert_eq!(a, b, "we are testing addition with {} and {}", a, b);
/// ```
#[macro_export]
macro_rules! assert_almost_eq {
    ($left:expr, $right:expr) => ({
        match (&$left, &$right) {
            (left_val, right_val) => {
                if !(left_val.approx_eq(right_val)) {
                    panic!(r#"assertion failed: `(left == right)`
  left: `{:?}`,
 right: `{:?}`"#, &*left_val, &*right_val)
                }
            }
        }
    });
    ($left:expr, $right:expr,) => ({
        $crate::assert_almost_eq!($left, $right)
    });
    ($left:expr, $right:expr, $($arg:tt)+) => ({
        match (&($left), &($right)) {
            (left_val, right_val) => {
                if !(left_val.approx_eq(right_val)) {
                    panic!(r#"assertion failed: `(left == right)`
  left: `{:?}`,
 right: `{:?}`: {}"#, &*left_val, &*right_val,
                           $crate::format_args!($($arg)+))
                }
            }
        }
    });
}
