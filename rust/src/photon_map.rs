use crate::aabb::Aabb;
use crate::approx_eq::ApproximateEq;
use crate::color::{color, Color};
use crate::cosine_distribution::CosineDistribution;
use crate::lights::LightRay;
use crate::partial_sort::partition_by_key;
use crate::ray::Ray;
use crate::tuple::{point, vector, Point, Vector};
use rand::distributions::Distribution;
use rand::thread_rng;
use std::collections::BinaryHeap;

#[derive(Debug, Clone)]
pub struct StoredPhoton {
    position: [f32; 3],
    direction: [f32; 3],
    power: (f32, [u8; 3]),
    kd_flag: KdFlag,
}

impl StoredPhoton {
    pub fn new(p: Point, d: Vector, c: Color) -> Self {
        StoredPhoton {
            position: [p.x() as f32, p.y() as f32, p.z() as f32],
            direction: [d.x() as f32, d.y() as f32, d.z() as f32],
            power: Self::compress_color(c),
            kd_flag: KdFlag::Uninitialized,
        }
    }

    fn compress_color(c: Color) -> (f32, [u8; 3]) {
        let x = c.red().max(c.green()).max(c.blue()) as f32;
        let r = (255.0 * c.red() as f32 / x).max(0.0).min(255.0) as u8;
        let g = (255.0 * c.green() as f32 / x).max(0.0).min(255.0) as u8;
        let b = (255.0 * c.blue() as f32 / x).max(0.0).min(255.0) as u8;

        (x, [r, g, b])
    }

    fn decompress_color((x, [r, g, b]): (f32, [u8; 3])) -> Color {
        let r = r as f64 * x as f64 / 255.0;
        let g = g as f64 * x as f64 / 255.0;
        let b = b as f64 * x as f64 / 255.0;

        color(r, g, b)
    }

    pub fn position(&self) -> Point {
        point(self.position[0], self.position[1], self.position[2])
    }

    pub fn direction(&self) -> Vector {
        vector(self.direction[0], self.direction[1], self.direction[2])
    }

    pub fn power(&self) -> Color {
        Self::decompress_color(self.power)
    }

    pub fn scale_power(mut self, s: f64) -> Self {
        self.power.0 *= s as f32;
        self
    }

    fn set_kdflag(&mut self, flag: KdFlag) {
        self.kd_flag = flag
    }

    fn get_kdflag(&self) -> KdFlag {
        self.kd_flag
    }
}

impl ApproximateEq for StoredPhoton {
    fn approx_eq(&self, other: &Self) -> bool {
        self.position.approx_eq(&other.position)
            && self.direction.approx_eq(&other.direction)
            && self.power.eq(&other.power)
    }
}

impl ApproximateEq for &StoredPhoton {
    fn approx_eq(&self, other: &Self) -> bool {
        self.position.approx_eq(&other.position)
            && self.direction.approx_eq(&other.direction)
            && self.power.eq(&other.power)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct TravellingPhoton {
    ray: Ray,
    power: Color,
    kind: PhotonKind,
}

impl TravellingPhoton {
    pub fn new(origin: Point, direction: Vector, power: Color) -> Self {
        TravellingPhoton {
            ray: Ray::new(origin, direction),
            power,
            kind: PhotonKind::Direct,
        }
    }

    pub fn ray(&self) -> &Ray {
        &self.ray
    }

    pub fn power(&self) -> f64 {
        self.power.sum()
    }

    pub fn kind(&self) -> PhotonKind {
        self.kind
    }

    pub fn is_direct(&self) -> bool {
        match self.kind {
            PhotonKind::Direct => true,
            _ => false,
        }
    }

    pub fn is_diffuse(&self) -> bool {
        match self.kind {
            PhotonKind::Diffuse => true,
            _ => false,
        }
    }

    pub fn is_caustic(&self) -> bool {
        match self.kind {
            PhotonKind::Caustic => true,
            _ => false,
        }
    }

    pub fn store(&self, position: Point) -> StoredPhoton {
        StoredPhoton::new(position, -self.ray.direction(), self.power)
    }

    pub fn scatter(self, p: Point, normv: Vector, diffuse_reflectance: Color) -> Self {
        let pd_avg = diffuse_reflectance.sum() / 3.0;
        TravellingPhoton {
            ray: Ray::new(p, CosineDistribution::new(normv).sample(&mut thread_rng())),
            power: self.power * diffuse_reflectance / pd_avg,
            kind: self.kind.scatter(),
        }
    }

    pub fn reflect(self, p: Point, normv: Vector) -> Self {
        TravellingPhoton {
            ray: Ray::new(p, self.ray.direction().reflect(&normv)),
            power: self.power,
            kind: self.kind.reflect(),
        }
    }

    pub fn refract(self, p: Point, normv: Vector, n1: f64, n2: f64) -> Self {
        let n_ratio = n1 / n2;
        let cos_i = -self.ray.direction().dot(&normv);
        let sin2_t = n_ratio * n_ratio * (1.0 - cos_i * cos_i);
        let cos_t = (1.0 - sin2_t).sqrt();
        let direction = normv * (n_ratio * cos_i - cos_t) + self.ray.direction() * n_ratio;
        TravellingPhoton {
            ray: Ray::new(p, direction),
            power: self.power,
            kind: self.kind.refract(),
        }
    }
}

impl From<LightRay> for TravellingPhoton {
    fn from(lr: LightRay) -> Self {
        TravellingPhoton {
            ray: Ray::new(lr.origin, lr.direction),
            power: lr.color,
            kind: PhotonKind::Direct,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum PhotonKind {
    Direct,
    Caustic,
    Diffuse,
}

impl PhotonKind {
    fn scatter(self) -> Self {
        PhotonKind::Diffuse
    }

    fn reflect(self) -> Self {
        match self {
            PhotonKind::Direct => PhotonKind::Caustic,
            PhotonKind::Caustic => PhotonKind::Caustic,
            PhotonKind::Diffuse => PhotonKind::Diffuse,
        }
    }

    fn refract(self) -> Self {
        match self {
            PhotonKind::Direct => PhotonKind::Caustic,
            PhotonKind::Caustic => PhotonKind::Caustic,
            PhotonKind::Diffuse => PhotonKind::Diffuse,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum KdFlag {
    Uninitialized,
    Leaf,
    SplitX,
    SplitY,
    SplitZ,
}

#[derive(Debug)]
pub struct PhotonMap {
    photons: Vec<StoredPhoton>,
}

impl PhotonMap {
    pub fn from_vec(mut photons: Vec<StoredPhoton>) -> Self {
        let mut indices: Vec<usize> = (0..photons.len()).collect();
        let mut kd_idx_tree = vec![usize::max_value(); photons.len()];

        let extent = compute_extent(&photons);
        PhotonMap::balance(&mut photons, &mut indices, &mut kd_idx_tree, 0, extent);

        permute_inplace(&mut photons, kd_idx_tree);
        PhotonMap { photons }
    }

    pub fn get_photon(&self, index: usize) -> &StoredPhoton {
        &self.photons[index]
    }

    fn balance(
        photons: &mut [StoredPhoton],
        indices: &mut [usize],
        tree: &mut [usize],
        node: usize,
        extent: Aabb,
    ) {
        if indices.is_empty() {
            return;
        }

        if indices.len() == 1 {
            tree[node] = indices[0];
            photons[indices[0]].set_kdflag(KdFlag::Leaf);
            return;
        }

        let n_left = compute_number_of_left_children(indices.len());

        let idx;
        let sub_extents;
        if extent.size().x() >= extent.size().y() && extent.size().x() >= extent.size().z() {
            let key = |&i: &usize| photons[i].position[0];
            partition_by_key(n_left, indices, key);
            idx = indices[n_left];
            let median = key(&idx);
            tree[node] = idx;
            photons[idx].set_kdflag(KdFlag::SplitX);
            sub_extents = extent.split_x(median);
        } else if extent.size().y() >= extent.size().x() && extent.size().y() >= extent.size().z() {
            let key = |&i: &usize| photons[i].position[1];
            partition_by_key(n_left, indices, key);
            idx = indices[n_left];
            let median = key(&idx);
            tree[node] = idx;
            photons[idx].set_kdflag(KdFlag::SplitY);
            sub_extents = extent.split_y(median);
        } else {
            let key = |&i: &usize| photons[i].position[2];
            partition_by_key(n_left, indices, key);
            idx = indices[n_left];
            let median = key(&idx);
            tree[node] = idx;
            photons[idx].set_kdflag(KdFlag::SplitZ);
            sub_extents = extent.split_z(median);
        }

        Self::balance(
            photons,
            &mut indices[..n_left],
            tree,
            left_child(node),
            sub_extents.0,
        );
        Self::balance(
            photons,
            &mut indices[n_left + 1..],
            tree,
            right_child(node),
            sub_extents.1,
        );
    }

    pub fn find_nearest(&self, n_neighbors: usize, p: Point) -> (Vec<&StoredPhoton>, f64) {
        let mut heap = BinaryHeap::new();
        self.locate_photons(0, n_neighbors, p, &mut heap);
        let maxsquared_distance = heap.peek().unwrap().squared_distance;
        let nearest = heap.into_iter().map(|sp| sp.photon).collect();
        (nearest, maxsquared_distance)
    }

    fn locate_photons<'a>(
        &'a self,
        node: usize,
        n_neighbors: usize,
        p: Point,
        heap: &mut BinaryHeap<SearchedPhoton<'a>>,
    ) {
        if node >= self.photons.len() {
            return;
        }

        if !self.is_leaf(node) {
            let d = self.distance_to_splitting_plane(p, node);
            if d < 0.0 {
                self.locate_photons(left_child(node), n_neighbors, p, heap);
                if heap.len() < n_neighbors || d * d < heap.peek().unwrap().squared_distance {
                    self.locate_photons(right_child(node), n_neighbors, p, heap);
                }
            } else {
                self.locate_photons(right_child(node), n_neighbors, p, heap);
                if heap.len() < n_neighbors || d * d < heap.peek().unwrap().squared_distance {
                    self.locate_photons(left_child(node), n_neighbors, p, heap);
                }
            }
        }

        let d2 = (p - self.photons[node].position()).square_len();
        if heap.len() < n_neighbors || d2 < heap.peek().unwrap().squared_distance {
            if heap.len() == n_neighbors {
                heap.pop();
            }

            heap.push(SearchedPhoton {
                squared_distance: d2,
                photon: &self.photons[node],
            });
        }
    }

    fn distance_to_splitting_plane(&self, p: Point, node: usize) -> f64 {
        let photon = &self.photons[node];
        match photon.get_kdflag() {
            KdFlag::SplitX => (p.x() - photon.position().x()),
            KdFlag::SplitY => (p.y() - photon.position().y()),
            KdFlag::SplitZ => (p.z() - photon.position().z()),
            _ => unreachable!(),
        }
    }

    fn is_leaf(&self, node: usize) -> bool {
        left_child(node) >= self.photons.len()
    }
}

#[derive(Debug)]
struct SearchedPhoton<'a> {
    squared_distance: f64,
    photon: &'a StoredPhoton,
}

impl std::cmp::Ord for SearchedPhoton<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl std::cmp::PartialOrd for SearchedPhoton<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.squared_distance.partial_cmp(&other.squared_distance)
    }
}

impl std::cmp::Eq for SearchedPhoton<'_> {}

impl std::cmp::PartialEq for SearchedPhoton<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.squared_distance.eq(&other.squared_distance)
    }
}

fn left_child(n: usize) -> usize {
    2 * (n + 1) - 1
}

fn right_child(n: usize) -> usize {
    2 * (n + 1)
}

fn compute_extent(photons: &[StoredPhoton]) -> Aabb {
    let mut aabb = Aabb::empty_at(photons[0].position());
    for p in &photons[1..] {
        aabb = aabb.extend(p.position());
    }
    aabb
}

fn compute_number_of_left_children(n: usize) -> usize {
    match n {
        0 => 0,
        1 | 2 | 3 => 1,
        4 => 2,
        5 | 6 | 7 => 3,
        _ => {
            let tree_depth = (n as f64).log2().floor() as u32;
            let n_full_levels = usize::pow(2, tree_depth);
            let n_last_level = n - (n_full_levels - 1);

            let n_left;
            if n_last_level <= n_full_levels / 2 {
                n_left = (n_full_levels - 2) / 2 + n_last_level;
            } else {
                n_left = (n_full_levels - 2) / 2 + n_full_levels / 2;
            }

            n_left
        }
    }
}

fn permute_inplace<T: Clone>(a: &mut [T], mut indices: Vec<usize>) {
    for i in 0..a.len() {
        let x = a[i].clone();
        let mut j = i;
        loop {
            let k = indices[j];
            indices[j] = j;
            if k == i {
                break;
            }
            a.swap(j, k);
            j = k;
        }
        a[j] = x;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::approx_eq::ApproximateEq;
    use crate::color::color;
    use crate::tuple::{point, vector};

    /*#[test]
    fn photon_scatter() {
        unimplemented!("How to test random functions?")
    }*/

    #[test]
    fn photon_reflect() {
        let p0 = TravellingPhoton::new(point(0, 0, 0), vector(1, -1, -1), color(1, 1, 1));
        let p1 = p0.reflect(point(0, 0, 0), vector(0, 1, 0));
        assert_almost_eq!(p1.ray.origin(), point(0, 0, 0));
        assert_almost_eq!(p1.ray.direction(), vector(1, 1, -1));
    }

    #[test]
    fn photon_refract_equal_index() {
        let n1 = 1.0;
        let n2 = 1.0;
        let p0 = TravellingPhoton::new(
            point(0, 0, 0),
            vector(1, -1, 0).normalized(),
            color(1, 1, 1),
        );
        let p1 = p0.refract(point(0, 0, 0), vector(0, 1, 0), n1, n2);
        assert_almost_eq!(p1.ray.origin(), point(0, 0, 0));
        assert_almost_eq!(p1.ray.direction(), p0.ray.direction());
    }

    #[test]
    fn photon_refract_lo_hi() {
        let n1 = 1.0;
        let n2 = 2.0;
        let p0 = TravellingPhoton::new(
            point(0, 0, 0),
            vector(1, -1, 0).normalized(),
            color(1, 1, 1),
        );
        let p1 = p0.refract(point(0, 0, 0), vector(0, 1, 0), n1, n2);
        assert_almost_eq!(p1.ray.origin(), point(0, 0, 0));
        assert_almost_eq!(p1.ray.direction().square_len(), 1.0);
        assert_almost_eq!(p1.ray.direction(), vector(0.353553, -0.935414, 0));
    }

    #[test]
    fn photon_refract_hi_lo() {
        let n1 = 1.3;
        let n2 = 1.0;
        let p0 = TravellingPhoton::new(
            point(0, 0, 0),
            vector(1, -1, 0).normalized(),
            color(1, 1, 1),
        );
        let p1 = p0.refract(point(0, 0, 0), vector(0, 1, 0), n1, n2);
        assert_almost_eq!(p1.ray.origin(), point(0, 0, 0));
        assert_almost_eq!(p1.ray.direction().square_len(), 1.0);
        assert_almost_eq!(p1.ray.direction(), vector(0.919239, -0.393700, 0));
    }

    #[test]
    fn permute() {
        let mut a = vec![8, 6, 7, 5, 3, 0, 9];
        let indices = vec![3, 6, 2, 4, 0, 1, 5];
        permute_inplace(&mut a, indices);
        assert_eq!(a, vec![5, 9, 7, 3, 8, 6, 0]);
    }

    fn photon(p: Point) -> StoredPhoton {
        StoredPhoton::new(p, vector(0, 0, 1), color(1, 1, 1))
    }

    #[test]
    fn extent() {
        let photons = vec![
            photon(point(1, 3, -10)),
            photon(point(2, 2, 0)),
            photon(point(3, 1, 10)),
        ];
        assert_almost_eq!(
            compute_extent(&photons),
            Aabb::new(1.0, 3.0, 1.0, 3.0, -10.0, 10.0)
        )
    }

    #[test]
    fn balance2() {
        let mut photons = vec![photon(point(0, 0, 0)), photon(point(1, 1, 1))];
        let mut indices: Vec<usize> = (0..photons.len()).collect();
        let mut tree = vec![usize::max_value(); photons.len()];

        PhotonMap::balance(
            &mut photons,
            &mut indices,
            &mut tree,
            0,
            Aabb::new(0.0, 1.0, 0.0, 1.0, 0.0, 1.0),
        );

        assert_eq!(tree, vec![1, 0]);
    }

    #[test]
    fn balance5() {
        let mut photons = vec![
            photon(point(0, 0, 0)),
            photon(point(1, 1, 1)),
            photon(point(2, 2, 2)),
            photon(point(3, 3, 3)),
            photon(point(4, 4, 4)),
        ];
        let mut indices: Vec<usize> = (0..photons.len()).collect();
        let mut tree = vec![usize::max_value(); photons.len()];

        PhotonMap::balance(
            &mut photons,
            &mut indices,
            &mut tree,
            0,
            Aabb::new(0.0, 4.0, 0.0, 4.0, 0.0, 4.0),
        );

        assert_eq!(tree, vec![3, 1, 4, 0, 2]);
    }

    #[test]
    fn balance10() {
        let mut photons = vec![
            photon(point(0, 0, 0)),
            photon(point(1, 1, 1)),
            photon(point(2, 2, 2)),
            photon(point(3, 3, 3)),
            photon(point(4, 4, 4)),
            photon(point(5, 5, 5)),
            photon(point(6, 6, 6)),
            photon(point(7, 7, 7)),
            photon(point(8, 8, 8)),
            photon(point(9, 9, 9)),
        ];
        let mut indices: Vec<usize> = (0..photons.len()).collect();
        let mut tree = vec![usize::max_value(); photons.len()];

        PhotonMap::balance(
            &mut photons,
            &mut indices,
            &mut tree,
            0,
            Aabb::new(0.0, 9.0, 0.0, 9.0, 0.0, 9.0),
        );

        assert_eq!(tree, vec![6, 3, 8, 1, 5, 7, 9, 0, 2, 4]);
    }

    #[test]
    fn balance_same() {
        let mut photons = vec![
            photon(point(1, 1, 1)),
            photon(point(1, 1, 1)),
            photon(point(1, 1, 1)),
            photon(point(1, 1, 1)),
            photon(point(1, 1, 1)),
        ];
        let mut indices: Vec<usize> = (0..photons.len()).collect();
        let mut tree = vec![usize::max_value(); photons.len()];

        PhotonMap::balance(
            &mut photons,
            &mut indices,
            &mut tree,
            0,
            Aabb::new(1.0, 1.0, 1.0, 1.0, 1.0, 1.0),
        );

        tree.sort();
        assert_eq!(tree, vec![0, 1, 2, 3, 4]);
    }

    #[test]
    fn build_photon_map() {
        let photons = vec![
            photon(point(1, 1, 1)),
            photon(point(2, 2, 2)),
            photon(point(3, 3, 3)),
        ];
        let pm = PhotonMap::from_vec(photons.clone());

        assert_almost_eq!(pm.get_photon(0), &photons[1]);
        assert_almost_eq!(pm.get_photon(1), &photons[0]);
        assert_almost_eq!(pm.get_photon(2), &photons[2]);
    }

    #[test]
    fn kd_tree_split_axes() {
        let photons = vec![
            photon(point(1, 1, 1)),
            photon(point(2, 2, 2)),
            photon(point(3, 3, 3)),
            photon(point(4, 4, 4)),
            photon(point(5, 5, 5)),
        ];
        let pm = PhotonMap::from_vec(photons.clone());

        assert_almost_eq!(pm.get_photon(0), &photons[3]);
        assert_almost_eq!(pm.get_photon(1), &photons[1]);
        assert_almost_eq!(pm.get_photon(2), &photons[4]);
        assert_almost_eq!(pm.get_photon(3), &photons[0]);
        assert_almost_eq!(pm.get_photon(4), &photons[2]);

        assert_eq!(pm.get_photon(0).get_kdflag(), KdFlag::SplitX);
        assert_eq!(pm.get_photon(1).get_kdflag(), KdFlag::SplitY);
        assert_eq!(pm.get_photon(2).get_kdflag(), KdFlag::Leaf);
        assert_eq!(pm.get_photon(3).get_kdflag(), KdFlag::Leaf);
        assert_eq!(pm.get_photon(4).get_kdflag(), KdFlag::Leaf);
    }

    #[test]
    fn nearest_neighbor_lookup() {
        let photons = vec![
            photon(point(1, 1, 1)),
            photon(point(2, 2, 2)),
            photon(point(3, 3, 3)),
            photon(point(4, 4, 4)),
        ];
        let pm = PhotonMap::from_vec(photons.clone());

        assert_almost_eq!(
            pm.find_nearest(2, point(2.5, 2.5, 2.5)),
            (vec![&photons[1], &photons[2]], 0.75)
        );
        assert_almost_eq!(
            pm.find_nearest(2, point(0, 0, 0)),
            (vec![&photons[1], &photons[0]], 12.0)
        );
    }

    #[test]
    fn left_children() {
        assert_eq!(compute_number_of_left_children(0), 0);
        assert_eq!(compute_number_of_left_children(1), 1);
        assert_eq!(compute_number_of_left_children(2), 1);
        assert_eq!(compute_number_of_left_children(3), 1);
        assert_eq!(compute_number_of_left_children(4), 2);
        assert_eq!(compute_number_of_left_children(5), 3);
        assert_eq!(compute_number_of_left_children(6), 3);
        assert_eq!(compute_number_of_left_children(7), 3);
        assert_eq!(compute_number_of_left_children(8), 4);
        assert_eq!(compute_number_of_left_children(10), 6);
    }

    #[test]
    fn decompress_color() {
        assert_almost_eq!(
            StoredPhoton::decompress_color((1.0, [0, 0, 0])),
            color(0, 0, 0)
        );
        assert_almost_eq!(
            StoredPhoton::decompress_color((1.0, [255, 255, 255])),
            color(1, 1, 1)
        );
        assert_almost_eq!(
            StoredPhoton::decompress_color((1e3, [255, 127, 0])),
            color(1e3, 1e3 * 127.0 / 255.0, 0)
        );
    }

    #[test]
    fn compress_color() {
        let c = color(0, 0, 0);
        assert_almost_eq!(
            StoredPhoton::decompress_color(StoredPhoton::compress_color(c)),
            c
        );
        let c = color(1, 0, 0);
        assert_almost_eq!(
            StoredPhoton::decompress_color(StoredPhoton::compress_color(c)),
            c
        );
        let c = color(1e2, 1e3, 1e-3);
        assert_almost_eq!(
            StoredPhoton::decompress_color(StoredPhoton::compress_color(c)),
            color(98.03922, 1e3, 0.00000)
        );
    }
}

/*
N 1 | 2  |  3  |    4   |     5    |     6     |     7     |      8     |         10
M 0 | 1  |  1  |    2   |     2    |     3     |     3     |      4     |          6
----+----+-----+--------+----------+-----------+-----------+------------+-------------------------
  0 |  1 |  1  |    2   |     3    |      3    |     3     |      4     |          6
    | 0  | 0 2 |  1   3 |  1     4 |  1      5 |  1     5  |   2     6  |     3        8
               | 0      | 0 2      | 0 2    4  | 0 2   4 6 |  1 3   5 7 |  1     5   7   9
                                                           | 0          | 0 2   4
--------------------------------------------------------------------------------------------------
index in flat representation
                                                                                   0
                                                                              1        2
                                                                           3     4  5    6
                                                                          7 8   9
 */
