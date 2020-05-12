use crate::approx_eq::EPSILON;
use crate::tuple::Vector;
use rand::distributions::Distribution;
use rand::Rng;
use rand_distr::UnitSphere;

#[derive(Clone, Copy, Debug)]
pub struct CosineDistribution(Vector);

impl CosineDistribution {
    pub fn new(direction: Vector) -> Self {
        CosineDistribution(direction)
    }
}

impl Distribution<Vector> for CosineDistribution {
    #[inline]
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Vector {
        loop {
            let w: Vector = UnitSphere.sample(rng).into();
            let v = self.0 + w;
            let l = v.square_len();

            if l > EPSILON {
                return v / l;
            }
        }
    }
}
