use rand::{distributions::Distribution, thread_rng};
use rand_distr::UnitSphere;
use raytracing::color::color;
use raytracing::lights::{Beam, DiscLight, Light, PointLight, RealisticPointLight};
use raytracing::tuple::{point, vector, Vector};
use std::collections::BTreeMap;
use std::f64::consts::PI;
use std::time::{Duration, Instant};

struct LightData {
    source: Box<dyn Light>,

    n_samples: f64,
    cumulative_intensity_r2: f64,
    cumulative_intensity_r4: f64,
}

impl LightData {
    fn new(source: impl Light) -> Self {
        LightData {
            source: Box::new(source),
            n_samples: 0.0,
            cumulative_intensity_r2: 0.0,
            cumulative_intensity_r4: 0.0,
        }
    }
}

fn sphere_surface_area(radius: f64) -> f64 {
    4.0 * PI * radius * radius
}

fn main() {
    let mut lights = BTreeMap::new();
    lights.insert(
        "Point light",
        LightData::new(PointLight::new(point(0, 0, 0), color(1, 1, 1))),
    );
    lights.insert(
        "Realistic point light",
        LightData::new(RealisticPointLight::new(point(0, 0, 0), color(1, 1, 1))),
    );
    lights.insert(
        "Lambertian point light",
        LightData::new(DiscLight::new(
            point(0, 0, 0),
            vector(0, 1, 0),
            0.0,
            color(1, 1, 1),
        )),
    );
    lights.insert(
        "Lambertian disc light",
        LightData::new(DiscLight::new(
            point(0, 0, 0),
            vector(0, 1, 0),
            1.0,
            color(1, 1, 1),
        )),
    );
    lights.insert(
        "Beam light",
        LightData::new(Beam::new(
            point(0, 0, 0),
            vector(0, 0.5, 0),
            vector(0.5, 0, 0),
            color(1, 1, 1),
        )),
    );

    let rng = &mut thread_rng();

    let origin = point(0, 0, 0);

    loop {
        let start = Instant::now();
        while Instant::now() - start < Duration::from_secs(1) {
            let sample: [f64; 3] = UnitSphere.sample(rng);

            let sample_dir = Vector::from(sample);

            for l in lights.values_mut() {
                l.cumulative_intensity_r2 += l
                    .source
                    .incoming_at(origin + sample_dir * 2.0)
                    .intensity()
                    .sum()
                    / 3.0
                    * sphere_surface_area(2.0);
                l.cumulative_intensity_r4 += l
                    .source
                    .incoming_at(origin + sample_dir * 4)
                    .intensity()
                    .sum()
                    / 3.0
                    * sphere_surface_area(4.0);
                l.n_samples += 1.0;
            }
        }

        println!("{:>25} {:>6} {:>6}", "Type", "r=2", "r=4");
        println!("---------------------------------------------------------------");
        for (name, l) in &lights {
            println!(
                "{:>25} {:6.2} {:6.2}",
                name,
                l.cumulative_intensity_r2 / l.n_samples,
                l.cumulative_intensity_r4 / l.n_samples
            );
        }
        println!();
    }
}
