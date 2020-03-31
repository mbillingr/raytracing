use crate::tuple::{tuple, vector, Tuple};
use std::ops::{Index, Mul};
use vecmath::{
    col_mat4_mul, col_mat4_transform, mat4_det, mat4_inv, mat4_transposed, row_mat4_mul,
    row_mat4_transform, vec3_add, vec3_mul, Matrix4, Vector3,
};

pub fn translation(dx: impl Into<f64>, dy: impl Into<f64>, dz: impl Into<f64>) -> Matrix {
    Matrix::translate(dx.into(), dy.into(), dz.into())
}

pub fn scaling(sx: impl Into<f64>, sy: impl Into<f64>, sz: impl Into<f64>) -> Matrix {
    Matrix::scale(sx.into(), sy.into(), sz.into())
}

#[macro_export]
macro_rules! matrix {
    ($a:expr, $b:expr, $c:expr, $d:expr;
     $e:expr, $f:expr, $g:expr, $h:expr;
     $i:expr, $j:expr, $k:expr, $l:expr;
     $m:expr, $n:expr, $o:expr, $p:expr$(;)?) => {
        Matrix::Full([
            [$a as f64, $b as f64, $c as f64, $d as f64],
            [$e as f64, $f as f64, $g as f64, $h as f64],
            [$i as f64, $j as f64, $k as f64, $l as f64],
            [$m as f64, $n as f64, $o as f64, $p as f64],
        ])
    };
}

#[derive(Debug, Copy, Clone)]
pub enum Matrix {
    Identity,
    Full(Matrix4<f64>),
    Transposed(Matrix4<f64>),
    Translate(Vector3<f64>),
    Scale(Vector3<f64>),
}

impl Matrix {
    pub fn identity() -> Self {
        Matrix::Identity
    }

    pub fn translate(dx: f64, dy: f64, dz: f64) -> Self {
        Matrix::Translate([dx, dy, dz])
    }

    pub fn scale(sx: f64, sy: f64, sz: f64) -> Self {
        Matrix::Scale([sx, sy, sz])
    }

    pub fn transpose(self) -> Self {
        match self {
            Matrix::Identity => Matrix::Identity,
            Matrix::Full(mat) => Matrix::Transposed(mat),
            Matrix::Transposed(mat) => Matrix::Full(mat),
            Matrix::Translate([dx, dy, dz]) => matrix![
                1.0, 0.0, 0.0, 0.0;
                0.0, 1.0, 0.0, 0.0;
                0.0, 0.0, 1.0, 0.0;
                 dx,  dy,  dz, 1.0;
            ],
            Matrix::Scale(s) => Matrix::Scale(s),
        }
    }

    pub fn is_invertible(&self) -> bool {
        match self {
            Matrix::Identity => true,
            Matrix::Full(m) => mat4_det(*m) != 0.0,
            Matrix::Transposed(m) => mat4_det(*m) != 0.0,
            Matrix::Translate(_) => true,
            Matrix::Scale([sx, sy, sz]) => *sx == 0.0 && *sy == 0.0 && *sz == 0.0,
        }
    }

    pub fn inverse(self) -> Self {
        match self {
            Matrix::Identity => Matrix::Identity,
            Matrix::Full(m) => Matrix::Full(mat4_inv(m)),
            Matrix::Transposed(m) => Matrix::Transposed(mat4_inv(m)),
            Matrix::Translate([dx, dy, dz]) => Matrix::Translate([-dx, -dy, -dz]),
            Matrix::Scale([sx, sy, sz]) => Matrix::Scale([1.0 / sx, 1.0 / sy, 1.0 / sz]),
        }
    }

    pub fn into_flat(self) -> [f64; 16] {
        match self {
            Matrix::Identity => [
                1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0,
            ],
            Matrix::Full(m) => [
                m[0][0], m[0][1], m[0][2], m[0][3], m[1][0], m[1][1], m[1][2], m[1][3], m[2][0],
                m[2][1], m[2][2], m[2][3], m[3][0], m[3][1], m[3][2], m[3][3],
            ],
            Matrix::Transposed(m) => [
                m[0][0], m[1][0], m[2][0], m[3][0], m[0][1], m[1][1], m[2][1], m[3][1], m[0][2],
                m[1][2], m[2][2], m[3][2], m[0][3], m[1][3], m[2][3], m[3][3],
            ],
            Matrix::Translate([dx, dy, dz]) => [
                1.0, 0.0, 0.0, dx, 0.0, 1.0, 0.0, dy, 0.0, 0.0, 1.0, dz, 0.0, 0.0, 0.0, 1.0,
            ],
            Matrix::Scale([sx, sy, sz]) => [
                sx, 0.0, 0.0, 0.0, 0.0, sy, 0.0, 0.0, 0.0, 0.0, sz, 0.0, 0.0, 0.0, 0.0, 1.0,
            ],
        }
    }
}

impl Index<(usize, usize)> for Matrix {
    type Output = f64;
    fn index(&self, (i, j): (usize, usize)) -> &f64 {
        match self {
            Matrix::Identity if i == j => &1.0,
            Matrix::Identity => &0.0,
            Matrix::Full(mat) => &mat[i][j],
            Matrix::Transposed(mat) => &mat[j][i],
            Matrix::Translate(_) if i == j => &1.0,
            Matrix::Translate(v) if j == 3 => &v[i],
            Matrix::Translate(_) => &0.0,
            Matrix::Scale(_) if i == 3 && j == 3 => &1.0,
            Matrix::Scale(s) if i == j => &s[i],
            Matrix::Scale(_) => &0.0,
        }
    }
}

impl Mul for Matrix {
    type Output = Matrix;
    fn mul(self, rhs: Matrix) -> Self::Output {
        use Matrix::*;
        match (self, rhs) {
            (Identity, any) | (any, Identity) => any,
            (Full(a), Full(b)) => Full(row_mat4_mul(a, b)),
            (Transposed(a), Transposed(b)) => Transposed(col_mat4_mul(a, b)),
            (Transposed(a), Full(b)) => Full(row_mat4_mul(mat4_transposed(a), b)),
            (Full(a), Transposed(b)) => Full(row_mat4_mul(a, mat4_transposed(b))),
            (Translate(a), Translate(b)) => Translate(vec3_add(a, b)),
            (Full(a), Translate([x, y, z])) => matrix![
                a[0][0], a[0][1], a[0][2], a[0][0] * x;
                a[1][0], a[1][1], a[1][2], a[1][0] * y;
                a[2][0], a[2][1], a[2][2], a[2][0] * z;
                a[3][0], a[3][1], a[3][2], a[3][0];
            ],
            (Transposed(a), Translate([x, y, z])) => matrix![
                a[0][0], a[1][0], a[2][0], a[0][0] * x;
                a[0][1], a[1][1], a[2][1], a[0][1] * y;
                a[0][2], a[1][2], a[2][2], a[0][2] * z;
                a[0][3], a[1][3], a[2][3], a[0][3];
            ],
            (Scale(a), Scale(b)) => Scale(vec3_mul(a, b)),
            (Full(a), Scale([x, y, z])) => matrix![
                a[0][0] * x, a[0][1], a[0][2], a[0][0];
                a[1][0], a[1][1] * y, a[1][2], a[1][0];
                a[2][0], a[2][1], a[2][2] * z, a[2][0];
                a[3][0], a[3][1], a[3][2], a[3][0];
            ],
            (Transposed(a), Scale([x, y, z])) => matrix![
                a[0][0] * x, a[1][0], a[2][0], a[0][0];
                a[0][1], a[1][1] * y, a[2][1], a[0][1];
                a[0][2], a[1][2], a[2][2] * z, a[0][2];
                a[0][3], a[1][3], a[2][3], a[0][3];
            ],
            (Translate([dx, dy, dz]), Scale([sx, sy, sz])) => matrix![
                sx, 0.0, 0.0, dx;
                0.0, sy, 0.0, dy;
                0.0, 0.0, sz, dz;
                0.0, 0.0, 0.0, 1.0;
            ],
            (Scale([sx, sy, sz]), Translate([dx, dy, dz])) => matrix![
                sx, 0.0, 0.0, sx * dx;
                0.0, sy, 0.0, sy * dy;
                0.0, 0.0, sz, sz * dz;
                0.0, 0.0, 0.0, 1.0;
            ],
            (a @ Translate(_), b @ Full(_)) => (b.transpose() * a.transpose()).transpose(),
            (a @ Translate(_), b @ Transposed(_)) => (b.transpose() * a.transpose()).transpose(),
            (a @ Scale(_), b @ Full(_)) => (b.transpose() * a.transpose()).transpose(),
            (a @ Scale(_), b @ Transposed(_)) => (b.transpose() * a.transpose()).transpose(),
        }
    }
}

impl Mul<Tuple> for Matrix {
    type Output = Tuple;
    fn mul(self, rhs: Tuple) -> Self::Output {
        use Matrix::*;
        match self {
            Identity => rhs,
            Full(mat) => Tuple(row_mat4_transform(mat, rhs.0)),
            Transposed(mat) => Tuple(col_mat4_transform(mat, rhs.0)),
            Translate([dx, dy, dz]) if rhs.is_point() => rhs + vector(dx, dy, dz),
            Translate(_) => rhs,
            Scale([sx, sy, sz]) => tuple(rhs.x() * sx, rhs.y() * sy, rhs.z() * sz, rhs.w()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::approx_eq::ApproximateEq;
    use crate::tuple::{point, tuple};

    /// Only for testing, implement an inaccurate PartialEq
    impl PartialEq for Matrix {
        fn eq(&self, other: &Self) -> bool {
            self.approx_eq(other)
        }
    }

    /// Constructing and inspecting a 4x4 matrix
    #[test]
    fn matrix4() {
        let m = matrix![
            1,     2,    3,    4;
            5.5,   6.5,  7.5,  8.5;
            9,    10,   11,   12;
            13.5, 14.5, 15.5, 16.5
        ];
        assert_eq!(m[(0, 0)], 1.0);
        assert_eq!(m[(0, 3)], 4.0);
        assert_eq!(m[(1, 0)], 5.5);
        assert_eq!(m[(1, 2)], 7.5);
        assert_eq!(m[(2, 2)], 11.0);
        assert_eq!(m[(3, 0)], 13.5);
        assert_eq!(m[(3, 2)], 15.5);
    }

    /// Matrix equality with identical matrices
    #[test]
    fn matrix_eq() {
        let a = matrix![
            1, 2, 3, 4;
            5, 6, 7, 8;
            9, 8, 7, 6;
            5, 4, 3, 2;
        ];
        let b = matrix![
            1, 2, 3, 4;
            5, 6, 7, 8;
            9, 8, 7, 6;
            5, 4, 3, 2;
        ];
        assert_eq!(a, b);
    }

    /// Matrix equality with different matrices
    #[test]
    fn matrix_neq() {
        let a = matrix![
            1, 2, 3, 4;
            5, 6, 7, 8;
            9, 8, 7, 6;
            5, 4, 3, 2;
        ];
        let b = matrix![
            2, 1, 3, 4;
            5, 6, 7, 8;
            9, 8, 7, 6;
            5, 4, 3, 2;
        ];
        assert_ne!(a, b);
    }

    /// Multiplying two matrices
    #[test]
    fn matrix_mul() {
        let a = matrix![
            1, 2, 3, 4;
            5, 6, 7, 8;
            9, 8, 7, 6;
            5, 4, 3, 2;
        ];
        let b = matrix![
            -2, 1, 2,  3;
             3, 2, 1, -1;
             4, 3, 6,  5;
             1, 2, 7,  8;
        ];
        assert_eq!(
            a * b,
            matrix![
                20, 22,  50,  48;
                44, 54, 114, 108;
                40, 58, 110, 102;
                16, 26,  46,  42;
            ]
        );
    }

    /// Multiplying two matrices
    #[test]
    fn matrix_mul_both_transposed() {
        let a = matrix![
            1, 2, 3, 4;
            5, 6, 7, 8;
            9, 8, 7, 6;
            5, 4, 3, 2;
        ];
        let b = matrix![
            -2, 1, 2,  3;
             3, 2, 1, -1;
             4, 3, 6,  5;
             1, 2, 7,  8;
        ];
        assert_eq!(
            (b.transpose() * a.transpose()).transpose(),
            matrix![
                20, 22,  50,  48;
                44, 54, 114, 108;
                40, 58, 110, 102;
                16, 26,  46,  42;
            ]
        );
    }

    /// Multiplying two matrices
    #[test]
    fn matrix_mul_a_transposed() {
        let a = matrix![
            1, 5, 9, 5;
            2, 6, 8, 4;
            3, 7, 7, 3;
            4, 8, 6, 2;
        ];
        let b = matrix![
            -2, 1, 2,  3;
             3, 2, 1, -1;
             4, 3, 6,  5;
             1, 2, 7,  8;
        ];
        assert_eq!(
            a.transpose() * b,
            matrix![
                20, 22,  50,  48;
                44, 54, 114, 108;
                40, 58, 110, 102;
                16, 26,  46,  42;
            ]
        );
    }

    /// Multiplying two matrices
    #[test]
    fn matrix_mul_b_transposed() {
        let a = matrix![
            1, 2, 3, 4;
            5, 6, 7, 8;
            9, 8, 7, 6;
            5, 4, 3, 2;
        ];
        let b = matrix![
            -2,  3, 4, 1;
             1,  2, 3, 2;
             2,  1, 6, 7;
             3, -1, 5, 8;
        ];
        assert_eq!(
            a * b.transpose(),
            matrix![
                20, 22,  50,  48;
                44, 54, 114, 108;
                40, 58, 110, 102;
                16, 26,  46,  42;
            ]
        );
    }

    /// A matrix multiplied by a tuple
    #[test]
    fn matrix_transform() {
        let a = matrix![
            1, 2, 3, 4;
            2, 4, 4, 2;
            8, 6, 4, 1;
            0, 0, 0, 1;
        ];
        let b = tuple(1, 2, 3, 1);
        assert_eq!(a * b, tuple(18, 24, 33, 1),);
    }

    /// Multiplying a matrix by the identity matrix
    #[test]
    fn matrix_mul_ident() {
        let a = matrix![
            0, 1, 2, 4;
            1, 2, 4, 8;
            2, 4, 8, 16;
            4, 8, 16, 32;
        ];
        assert_eq!(Matrix::identity() * a * Matrix::identity(), a);
    }

    /// Multiplying the identity matrix by a tuple
    #[test]
    fn matrix_transform_ident() {
        let a = tuple(1, 2, 4, 3);
        assert_eq!(Matrix::identity() * a, a);
    }

    /// Transposing a matrix
    #[test]
    fn matrix_transpose() {
        let a = matrix![
            1, 2, 3, 4;
            2, 4, 4, 2;
            8, 6, 4, 1;
            0, 0, 0, 1;
        ]
        .transpose();
        assert_eq!(a[(0, 0)], 1.0);
        assert_eq!(a[(1, 0)], 2.0);
        assert_eq!(a[(3, 0)], 4.0);
        assert_eq!(a[(2, 1)], 4.0);
        assert_eq!(
            a,
            matrix![
                1, 2, 8, 0;
                2, 4, 6, 0;
                3, 4, 4, 0;
                4, 2, 1, 1;
            ]
        );
    }

    /// Transposing the identity matrix
    #[test]
    fn matrix_transpose_identity() {
        assert_eq!(Matrix::identity().transpose(), Matrix::identity());
    }

    /// Testing an invertible matrix for invertibility
    #[test]
    fn matrix_invertible() {
        let a = matrix![
            6,  4, 4,  4;
            5,  5, 7,  6;
            4, -9, 3, -7;
            9,  1, 7, -6;
        ];
        assert!(a.is_invertible());
    }

    /// Testing a noninvertible matrix for invertibility
    #[test]
    fn matrix_noninvertible() {
        let a = matrix![
            6,  4, 4,  4;
            5,  5, 7,  6;
            4, -9, 3, -7;
            0, 0, 0, 0;
        ];
        assert!(!a.is_invertible());
    }

    /// Calculating the inverse of a matrix
    #[test]
    fn matrix_inverse() {
        let a = matrix![
            -5,  2,  6, -8;
             1, -5,  1,  8;
             7,  7, -6, -7;
             1, -3,  7,  4;
        ];
        assert_eq!(
            a.inverse(),
            matrix![
                 0.21805,  0.45113,  0.24060, -0.04511;
                -0.80827, -1.45677, -0.44361,  0.52068;
                -0.07895, -0.22368, -0.05263,  0.19737;
                -0.52256, -0.81391, -0.30075,  0.30639;
            ],
        );
    }

    /// The inverse of a transposed matrix is the same as the transposed inverse matrix
    #[test]
    fn matrix_invtrans() {
        let a = matrix![
            -5,  2,  6, -8;
             1, -5,  1,  8;
             7,  7, -6, -7;
             1, -3,  7,  4;
        ];
        assert_eq!(a.transpose().inverse(), a.inverse().transpose());
    }

    /// Multiplying a product by its inverse
    #[test]
    fn matrix_prodinv() {
        let a = matrix![
             3, -9,  7,  3;
             3, -8,  2, -9;
            -4,  4,  4,  1;
            -6,  5, -1,  1;
        ];
        let b = matrix![
            8,  2,  2,  2;
            3, -1,  7,  0;
            7,  0,  5,  4;
            6, -2,  0,  5;
        ];
        assert_eq!((a * b) * b.inverse(), a);
    }

    /// Multiplying by a translation matrix
    #[test]
    fn translate_point() {
        let transform = translation(5, -3, 2);
        let p = point(-3, 4, 5);
        assert_eq!(transform * p, point(2, 1, 7));
    }

    /// Multiplying by the inverse of a translation matrix
    #[test]
    fn translate_inv_point() {
        let transform = translation(5, -3, 2);
        let inv = transform.inverse();
        let p = point(-3, 4, 5);
        assert_eq!(inv * p, point(-8, 7, 3));
    }

    /// Translation does not affect vectors
    #[test]
    fn translate_vector() {
        let transform = translation(5, -3, 2);
        let v = vector(-3, 4, 5);
        assert_eq!(transform * v, v);
    }

    /// A scaling matrix applied to a point
    #[test]
    fn scale_point() {
        let transform = scaling(2, 3, 4);
        let p = point(-4, 6, 8);
        assert_eq!(transform * p, point(-8, 18, 32));
    }

    /// A scaling matrix applied to a vector
    #[test]
    fn scale_vector() {
        let transform = scaling(2, 3, 4);
        let v = vector(-4, 6, 8);
        assert_eq!(transform * v, vector(-8, 18, 32));
    }

    /// Multiplying by the inverse of a scaling matrix
    #[test]
    fn scale_inv_vector() {
        let transform = scaling(2, 3, 4);
        let inv = transform.inverse();
        let v = vector(-4, 6, 8);
        assert_eq!(inv * v, vector(-2, 2, 2));
    }

    /// Reflection is scaling by a negative value
    #[test]
    fn reflect_point() {
        let transform = scaling(-1, 1, 1);
        let p = point(2, 3, 4);
        assert_eq!(transform * p, point(-2, 3, 4));
    }
}
