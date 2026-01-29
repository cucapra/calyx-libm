use malachite::num::arithmetic::traits::RoundToMultipleOfPowerOf2;
use malachite::rounding_modes::RoundingMode;

pub trait RoundBinary: Sized {
    type Output;

    fn round(self, position: i64, mode: RoundingMode) -> Self::Output;

    #[inline]
    fn floor(self, position: i64) -> Self::Output {
        self.round(position, RoundingMode::Floor)
    }

    #[inline]
    fn ceil(self, position: i64) -> Self::Output {
        self.round(position, RoundingMode::Ceiling)
    }

    #[inline]
    fn round_toward_zero(self, position: i64) -> Self::Output {
        self.round(position, RoundingMode::Down)
    }

    #[inline]
    fn round_away_from_zero(self, position: i64) -> Self::Output {
        self.round(position, RoundingMode::Up)
    }

    #[inline]
    fn round_convergent(self, position: i64) -> Self::Output {
        self.round(position, RoundingMode::Nearest)
    }
}

impl<T> RoundBinary for T
where
    T: RoundToMultipleOfPowerOf2<i64>,
{
    type Output = T::Output;

    fn round(self, position: i64, mode: RoundingMode) -> Self::Output {
        self.round_to_multiple_of_power_of_2(position, mode).0
    }
}
