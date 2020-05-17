use rand::seq::SliceRandom;
use rand::{thread_rng, Rng};
use raytracing::color::color;
use raytracing::partial_sort::partition_by_key;
use raytracing::photon_map::{PhotonMap, StoredPhoton};
use raytracing::tuple::{point, vector};
use std::time::{Duration, Instant};

fn measure_full(n: usize) -> Duration {
    let rng = &mut thread_rng();
    let photons: Vec<_> = (0..n)
        .map(|_| {
            StoredPhoton::new(
                point(
                    rng.gen_range(-1.0, 1.0),
                    rng.gen_range(-1.0, 1.0),
                    rng.gen_range(-1.0, 1.0),
                ),
                vector(0, 1, 0),
                color(1, 1, 1),
            )
        })
        .collect();
    let start = Instant::now();
    let _photon_map = Some((PhotonMap::from_vec(photons), 1));
    Instant::now() - start
}

fn measure_partition(n: usize) -> Duration {
    let rng = &mut thread_rng();
    let mut data: Vec<_> = (0..n).collect();
    data.shuffle(rng);
    let start = Instant::now();
    partition_by_key(n / 2, &mut data, |&d| d as f32);
    Instant::now() - start
}

fn main() {
    println!("Partitioning:");
    for &n in &[10_000, 100_000, 1_000_000] {
        println!("{:8} {:?}", n, measure_partition(n))
    }

    println!("Balancing:");
    for &n in &[1_000, 10_000, 100_000] {
        println!("{:8} {:?}", n, measure_full(n))
    }
}
