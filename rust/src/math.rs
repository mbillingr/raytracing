use std::ops::Mul;

pub trait Squared {
    type Output;
    fn squared(self) -> Self::Output;
}

impl<T: Clone + Mul> Squared for T {
    type Output = T::Output;
    fn squared(self) -> Self::Output {
        self.clone().mul(self)
    }
}

/*impl Squared for f64 {
    type Output = f64;
    fn squared(self) -> Self::Output {
        self.mul(self)
    }
}*/
