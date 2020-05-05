use crate::approx_eq::ApproximateEq;
use crate::tuple::{point, vector, Point, Vector};
use quaternion::Quaternion;
use std::ops::{Index, Mul};
use vecmath::{
    col_mat4_mul, col_mat4_transform, mat4_det, mat4_inv, mat4_transposed, row_mat4_mul,
    row_mat4_transform, vec3_add, vec3_mul, Matrix4, Vector3,
};

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

pub fn translation(dx: impl Into<f64>, dy: impl Into<f64>, dz: impl Into<f64>) -> Matrix {
    Matrix::Identity.translate(dx.into(), dy.into(), dz.into())
}

pub fn scaling(sx: impl Into<f64>, sy: impl Into<f64>, sz: impl Into<f64>) -> Matrix {
    Matrix::Identity.scale(sx.into(), sy.into(), sz.into())
}

pub fn rotation_x(phi: impl Into<f64>) -> Matrix {
    Matrix::Identity.rotate(phi.into(), [1.0, 0.0, 0.0].into())
}

pub fn rotation_y(phi: impl Into<f64>) -> Matrix {
    Matrix::Identity.rotate(phi.into(), [0.0, 1.0, 0.0].into())
}

pub fn rotation_z(phi: impl Into<f64>) -> Matrix {
    Matrix::Identity.rotate(phi.into(), [0.0, 0.0, 1.0].into())
}

pub fn rotation(phi: impl Into<f64>, axis: Vector) -> Matrix {
    Matrix::Identity.rotate(phi.into(), [axis.x(), axis.y(), axis.z()].into())
}

pub fn view_transform(from: Point, to: Point, up: Vector) -> Matrix {
    Matrix::view(from, to, up)
}

pub fn shearing(
    xy: impl Into<f64>,
    xz: impl Into<f64>,
    yx: impl Into<f64>,
    yz: impl Into<f64>,
    zx: impl Into<f64>,
    zy: impl Into<f64>,
) -> Matrix {
    matrix![
        1, xy.into(), xz.into(), 0;
        yx.into(), 1, yz.into(), 0;
        zx.into(), zy.into(), 1, 0;
        0, 0, 0, 1;
    ]
}

#[derive(Debug, Copy, Clone)]
pub enum Matrix {
    Identity,
    Full(Matrix4<f64>),
    Transposed(Matrix4<f64>),
    Translate(Vector3<f64>),
    Scale(Vector3<f64>),
    Rotate(Quaternion<f64>),
}

impl Matrix {
    pub fn identity() -> Self {
        Matrix::Identity
    }

    pub fn translate(self, dx: f64, dy: f64, dz: f64) -> Self {
        Matrix::Translate([dx, dy, dz]) * self
    }

    pub fn scale(self, sx: f64, sy: f64, sz: f64) -> Self {
        Matrix::Scale([sx, sy, sz]) * self
    }

    pub fn rotate(self, phi: f64, axis: Vector) -> Self {
        Matrix::Rotate(quaternion::axis_angle(axis.as_vec3(), phi)) * self
    }

    pub fn view(from: Point, to: Point, up: Vector) -> Self {
        let forward = (to - from).normalized();
        let left = forward.cross(&up.normalized());
        let true_up = left.cross(&forward);
        let orientation = Matrix::Full([
            [left.x(), left.y(), left.z(), 0.0],
            [true_up.x(), true_up.y(), true_up.z(), 0.0],
            [-forward.x(), -forward.y(), -forward.z(), 0.0],
            [0.0, 0.0, 0.0, 1.0],
        ]);
        orientation * Matrix::Translate([-from.x(), -from.y(), -from.z()])
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
            Matrix::Rotate(q) => Matrix::Rotate(quaternion::conj(q)),
        }
    }

    pub fn is_identity(&self) -> bool {
        match self {
            Matrix::Identity => true,
            Matrix::Full(_) | Matrix::Transposed(_) => self.approx_eq(&matrix![
                1.0, 0.0, 0.0, 0.0;
                0.0, 1.0, 0.0, 0.0;
                0.0, 0.0, 1.0, 0.0;
                0.0, 0.0, 0.0, 1.0;
            ]),
            Matrix::Translate([x, y, z]) => {
                x.approx_eq(&0.0) && y.approx_eq(&0.0) && z.approx_eq(&0.0)
            }
            Matrix::Scale([x, y, z]) => x.approx_eq(&1.0) && y.approx_eq(&1.0) && z.approx_eq(&1.0),
            Matrix::Rotate((r, [i, j, k])) => {
                r.approx_eq(&1.0) && i.approx_eq(&0.0) && j.approx_eq(&0.0) && k.approx_eq(&0.0)
            }
        }
    }

    pub fn is_invertible(&self) -> bool {
        match self {
            Matrix::Identity => true,
            Matrix::Full(m) => mat4_det(*m) != 0.0,
            Matrix::Transposed(m) => mat4_det(*m) != 0.0,
            Matrix::Translate(_) => true,
            Matrix::Scale([sx, sy, sz]) => *sx == 0.0 && *sy == 0.0 && *sz == 0.0,
            Matrix::Rotate(_) => true,
        }
    }

    pub fn inverse(self) -> Self {
        match self {
            Matrix::Identity => Matrix::Identity,
            Matrix::Full(m) => Matrix::Full(mat4_inv(m)),
            Matrix::Transposed(m) => Matrix::Transposed(mat4_inv(m)),
            Matrix::Translate([dx, dy, dz]) => Matrix::Translate([-dx, -dy, -dz]),
            Matrix::Scale([sx, sy, sz]) => Matrix::Scale([1.0 / sx, 1.0 / sy, 1.0 / sz]),
            Matrix::Rotate(_) => self.transpose(),
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
            Matrix::Rotate(q) => Matrix::Full(quat_to_mat(q)).into_flat(),
        }
    }
}

fn quat_to_mat(q: Quaternion<f64>) -> Matrix4<f64> {
    [
        [
            quat_to_mat_element(q, 0, 0),
            quat_to_mat_element(q, 0, 1),
            quat_to_mat_element(q, 0, 2),
            0.0,
        ],
        [
            quat_to_mat_element(q, 1, 0),
            quat_to_mat_element(q, 1, 1),
            quat_to_mat_element(q, 1, 2),
            0.0,
        ],
        [
            quat_to_mat_element(q, 2, 0),
            quat_to_mat_element(q, 2, 1),
            quat_to_mat_element(q, 2, 2),
            0.0,
        ],
        [0.0, 0.0, 0.0, 1.0],
    ]
}

fn quat_to_mat_element((r, [i, j, k]): Quaternion<f64>, row: usize, col: usize) -> f64 {
    match (row, col) {
        (0, 0) => 1.0 - 2.0 * (j * j + k * k),
        (1, 1) => 1.0 - 2.0 * (i * i + k * k),
        (2, 2) => 1.0 - 2.0 * (i * i + j * j),
        (0, 1) => 2.0 * (i * j - k * r),
        (1, 0) => 2.0 * (i * j + k * r),
        (1, 2) => 2.0 * (j * k - i * r),
        (2, 1) => 2.0 * (j * k + i * r),
        (0, 2) => 2.0 * (i * k + j * r),
        (2, 0) => 2.0 * (i * k - j * r),
        (3, 3) => 1.0,
        (3, _) => 0.0,
        (_, 3) => 0.0,
        _ => panic!("Invalid matrix element"),
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
            //Matrix::Rotate(q) => &quat_to_mat_element(*q, i, j),
            Matrix::Rotate(_) => unimplemented!("Indexing into a rotation matrix is not implemented. Maybe convert to a full matrix first..."),
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
                a[0][0], a[0][1], a[0][2], a[0][0] * x + a[0][1] * y + a[0][2] * z + a[0][3];
                a[1][0], a[1][1], a[1][2], a[1][0] * x + a[1][1] * y + a[1][2] * z + a[1][3];
                a[2][0], a[2][1], a[2][2], a[2][0] * x + a[2][1] * y + a[2][2] * z + a[2][3];
                a[3][0], a[3][1], a[3][2], a[3][0] * x + a[3][1] * y + a[3][2] * z + a[3][3];
            ],
            (Transposed(a), Translate([x, y, z])) => matrix![
                a[0][0], a[1][0], a[2][0], a[0][0] * x + a[1][0] * y + a[2][0] * z + a[3][0];
                a[0][1], a[1][1], a[2][1], a[0][1] * x + a[1][1] * y + a[2][1] * z + a[3][1];
                a[0][2], a[1][2], a[2][2], a[0][2] * x + a[1][2] * y + a[2][2] * z + a[3][2];
                a[0][3], a[1][3], a[2][3], a[0][3] * x + a[1][3] * y + a[2][3] * z + a[3][3];
            ],
            (Scale(a), Scale(b)) => Scale(vec3_mul(a, b)),
            (Full(a), Scale([x, y, z])) => matrix![
                a[0][0] * x, a[0][1] * y, a[0][2] * z, a[0][3];
                a[1][0] * x, a[1][1] * y, a[1][2] * z, a[1][3];
                a[2][0] * x, a[2][1] * y, a[2][2] * z, a[2][3];
                a[3][0] * x, a[3][1] * y, a[3][2] * z, a[3][3];
            ],
            (Transposed(a), Scale([x, y, z])) => matrix![
                a[0][0] * x, a[1][0] * y, a[2][0] * z, a[3][0];
                a[0][1] * x, a[1][1] * y, a[2][1] * z, a[3][1];
                a[0][2] * x, a[1][2] * y, a[2][2] * z, a[3][2];
                a[0][3] * x, a[1][3] * y, a[2][3] * z, a[3][3];
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
            (Rotate(a), Rotate(b)) => Rotate(quaternion::mul(a, b)),
            (Rotate(q), b) => Full(quat_to_mat(q)) * b,
            (a, Rotate(q)) => a * Full(quat_to_mat(q)),
        }
    }
}

impl Mul<Point> for Matrix {
    type Output = Point;
    fn mul(self, rhs: Point) -> Self::Output {
        use Matrix::*;
        match self {
            Identity => rhs,
            Full(mat) => Point::from(row_mat4_transform(mat, rhs.0)),
            Transposed(mat) => Point::from(col_mat4_transform(mat, rhs.0)),
            Translate([dx, dy, dz]) => rhs + vector(dx, dy, dz),
            Scale([sx, sy, sz]) => point(rhs.x() * sx, rhs.y() * sy, rhs.z() * sz),
            Rotate(q) => {
                let [x, y, z] = quaternion::rotate_vector(q, [rhs.x(), rhs.y(), rhs.z()]);
                point(x, y, z)
            }
        }
    }
}

impl Mul<Vector> for Matrix {
    type Output = Vector;
    fn mul(self, rhs: Vector) -> Self::Output {
        use Matrix::*;
        match self {
            Identity => rhs,
            Full(mat) => Vector::from(row_mat4_transform(mat, rhs.0)),
            Transposed(mat) => Vector::from(col_mat4_transform(mat, rhs.0)),
            Translate(_) => rhs,
            Scale([sx, sy, sz]) => vector(rhs.x() * sx, rhs.y() * sy, rhs.z() * sz),
            Rotate(q) => {
                let [x, y, z] = quaternion::rotate_vector(q, [rhs.x(), rhs.y(), rhs.z()]);
                vector(x, y, z)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::approx_eq::ApproximateEq;
    use crate::tuple::point;
    use std::f64::consts::PI;

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

    /// A matrix multiplied by a point
    #[test]
    fn matrix_transform_p() {
        let a = matrix![
            1, 2, 3, 4;
            2, 4, 4, 2;
            8, 6, 4, 1;
            0, 0, 0, 1;
        ];
        let b = point(1, 2, 3);
        assert_eq!(a * b, point(18, 24, 33),);
    }

    /// A matrix multiplied by a point
    #[test]
    fn matrix_transform_v() {
        let a = matrix![
            1, 2, 3, 4;
            2, 4, 4, 2;
            8, 6, 4, 1;
            0, 0, 0, 1;
        ];
        let b = vector(1, 2, 3);
        assert_eq!(a * b, vector(14, 22, 32));
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
        let a = point(1, 2, 4);
        let b = vector(1, 2, 4);
        assert_eq!(Matrix::identity() * a, a);
        assert_eq!(Matrix::identity() * b, b);
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

    /// Rotating a point around the x axis
    #[test]
    fn rotate_x_point() {
        let p = point(0, 1, 0);
        let half_quarter = rotation_x(PI / 4.0);
        let full_quarter = rotation_x(PI / 2.0);
        assert_eq!(
            half_quarter * p,
            point(0, f64::sqrt(2.0) / 2.0, f64::sqrt(2.0) / 2.0)
        );
        assert_eq!(full_quarter * p, point(0, 0, 1));
    }

    /// The inverse of an x-rotation rotates in the opposite direction
    #[test]
    fn rotate_inv_x_point() {
        let p = point(0, 1, 0);
        let half_quarter = rotation_x(PI / 4.0);
        let inv = half_quarter.inverse();
        assert_eq!(
            inv * p,
            point(0, f64::sqrt(2.0) / 2.0, -f64::sqrt(2.0) / 2.0)
        );
    }

    /// Rotating a point around the y axis
    #[test]
    fn rotate_y_point() {
        let p = point(0, 0, 1);
        let half_quarter = rotation_y(PI / 4.0);
        let full_quarter = rotation_y(PI / 2.0);
        assert_eq!(
            half_quarter * p,
            point(f64::sqrt(2.0) / 2.0, 0, f64::sqrt(2.0) / 2.0)
        );
        assert_eq!(full_quarter * p, point(1, 0, 0));
    }

    /// Rotating a point around the z axis
    #[test]
    fn rotate_z_point() {
        let p = point(0, 1, 0);
        let half_quarter = rotation_z(PI / 4.0);
        let full_quarter = rotation_z(PI / 2.0);
        assert_eq!(
            half_quarter * p,
            point(-f64::sqrt(2.0) / 2.0, f64::sqrt(2.0) / 2.0, 0)
        );
        assert_eq!(full_quarter * p, point(-1, 0, 0));
    }

    /// A shearing transformation moves x in proportion to y
    #[test]
    fn shear_xy() {
        let transform = shearing(1, 0, 0, 0, 0, 0);
        let p = point(2, 3, 4);
        assert_eq!(transform * p, point(5, 3, 4));
    }

    /// A shearing transformation moves x in proportion to z
    #[test]
    fn shear_xz() {
        let transform = shearing(0, 1, 0, 0, 0, 0);
        let p = point(2, 3, 4);
        assert_eq!(transform * p, point(6, 3, 4));
    }

    /// A shearing transformation moves y in proportion to x
    #[test]
    fn shear_yx() {
        let transform = shearing(0, 0, 1, 0, 0, 0);
        let p = point(2, 3, 4);
        assert_eq!(transform * p, point(2, 5, 4));
    }

    /// A shearing transformation moves y in proportion to z
    #[test]
    fn shear_yz() {
        let transform = shearing(0, 0, 0, 1, 0, 0);
        let p = point(2, 3, 4);
        assert_eq!(transform * p, point(2, 7, 4));
    }

    /// A shearing transformation moves z in proportion to x
    #[test]
    fn shear_zx() {
        let transform = shearing(0, 0, 0, 0, 1, 0);
        let p = point(2, 3, 4);
        assert_eq!(transform * p, point(2, 3, 6));
    }

    /// A shearing transformation moves z in proportion to y
    #[test]
    fn shear_zy() {
        let transform = shearing(0, 0, 0, 0, 0, 1);
        let p = point(2, 3, 4);
        assert_eq!(transform * p, point(2, 3, 7));
    }

    /// Individual transformations are applied in sequence
    #[test]
    fn transform_sequence() {
        let p = point(1, 0, 1);
        let a = rotation_x(PI / 2.0);
        let b = scaling(5, 5, 5);
        let c = translation(10, 5, 7);

        let p2 = a * p;
        let p3 = b * p2;
        let p4 = c * p3;

        assert_eq!(p2, point(1, -1, 0));
        assert_eq!(p3, point(5, -5, 0));
        assert_eq!(p4, point(15, 0, 7));
    }

    /// Chained transformations must be applied in reverse order
    #[test]
    fn transform_chain() {
        let p = point(1, 0, 1);
        let a = rotation_x(PI / 2.0);
        let b = scaling(5, 5, 5);
        let c = translation(10, 5, 7);

        let t = c * b * a;
        assert_eq!(t * p, point(15, 0, 7));

        let t = Matrix::identity()
            .rotate(PI / 2.0, [1.0, 0.0, 0.0].into())
            .scale(5.0, 5.0, 5.0)
            .translate(10.0, 5.0, 7.0);
        assert_eq!(t * p, point(15, 0, 7));
    }

    /// The transformation matrix for the default orientation
    #[test]
    fn view_default() {
        let from = point(0, 0, 0);
        let to = point(0, 0, -1);
        let up = vector(0, 1, 0);
        let t = view_transform(from, to, up);
        assert_almost_eq!(t, Matrix::Identity);
    }

    /// A view transformation matrix looking in positive z direction
    #[test]
    fn view_reverse() {
        let from = point(0, 0, 0);
        let to = point(0, 0, 1);
        let up = vector(0, 1, 0);
        let t = view_transform(from, to, up);
        assert_almost_eq!(t, scaling(-1, 1, -1));
    }

    /// The view transforma moves the world
    #[test]
    fn view_shifted() {
        let from = point(0, 0, 8);
        let to = point(0, 0, 0);
        let up = vector(0, 1, 0);
        let t = view_transform(from, to, up);
        assert_almost_eq!(t, translation(0, 0, -8));
    }

    /// An arbitrary view transform
    #[test]
    fn view_arbitrary() {
        let from = point(1, 3, 2);
        let to = point(4, -2, 8);
        let up = vector(1, 1, 0);
        let t = view_transform(from, to, up);
        assert_almost_eq!(
            t,
            matrix![
                -0.50709, 0.50709,  0.67612, -2.36643;
                 0.76772, 0.60609,  0.12122, -2.82843;
                -0.35857, 0.59761, -0.71714,  0.00000;
                 0.00000, 0.00000,  0.00000,  1.00000;
            ]
        );
    }
}
