use std::ops::{Add, Sub, Mul, AddAssign, SubAssign, MulAssign, Div, Neg};
use std::cmp::{Ord, Eq, PartialEq, PartialOrd, Ordering};
use std::marker::PhantomData;
use std::fmt;

#[derive(Debug)]
pub struct Length<U> {
    value: f64,
    _m: PhantomData<U>
}
impl<U> Length<U> {
    pub fn zero() -> Self {
        Length { value: 0.0, _m: PhantomData }
    }
    pub fn is_zero(&self) -> bool {
        self.value == 0.0
    }
    pub fn new(value: impl Into<f64>, unit: U) -> Self {
        Length { value: value.into(), _m: PhantomData }
    }
}
impl<U> Clone for Length<U> {
    fn clone(&self) -> Self {
        Length { value: self.value, _m: PhantomData }
    }
}
impl<U> Copy for Length<U> {}

impl<U> Div<U> for Length<U> {
    type Output = f64;
    fn div(self, rhs: U) -> f64 {
        self.value
    }
}
impl<U> Default for Length<U> {
    fn default() -> Self {
        Length { value: 0.0, _m: PhantomData }
    }
}
impl<U> PartialEq for Length<U> {
    fn eq(&self, other: &Self) -> bool {
        self.value.eq(&other.value)
    }
}
impl<U> PartialOrd for Length<U> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.value.partial_cmp(&other.value)
    }
}

impl<U> Eq for Length<U> {}
impl<U> Ord for Length<U> {
    fn cmp(&self, rhs: &Self) -> Ordering {
        self.value.partial_cmp(&rhs.value).unwrap()
    }
}

impl<U> Add for Length<U> {
    type Output = Length<U>;
    fn add(self, rhs: Length<U>) -> Length<U> {
        Length { value: self.value + rhs.value, _m: PhantomData }
    }
}
impl<U> Sub for Length<U> {
    type Output = Length<U>;
    fn sub(self, rhs: Length<U>) -> Length<U> {
        Length { value: self.value - rhs.value, _m: PhantomData }
    }
}
impl<U, T: Into<f64>> Mul<T> for Length<U> {
    type Output = Length<U>;
    fn mul(self, rhs: T) -> Length<U> {
        Length { value: self.value * rhs.into(), _m: PhantomData }
    }
}
impl<U> AddAssign for Length<U> {
    fn add_assign(&mut self, rhs: Length<U>) {
        self.value += rhs.value;
    }
}
impl<U> SubAssign for Length<U> {
    fn sub_assign(&mut self, rhs: Length<U>) {
        self.value -= rhs.value;
    }
}
impl<U> Neg for Length<U> {
    type Output = Self;
    fn neg(self) -> Self {
        Length { value: -self.value, _m: PhantomData }
    }
}

pub struct Font;
pub struct Px;
pub struct Em;

macro_rules! impl_length {
    ($($unit:ty),*) => {
        $(
            impl fmt::Debug for Length<$unit> {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    write!(f, concat!("Length {{ value: {:?}, unit: ", stringify!($unit), " }}"), self.value)
                }
            }
            impl fmt::Display for Length<$unit> {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    write!(f, concat!("{} ", stringify!($unit)), self.value)
                }
            }
        )*
    };
}

impl_length!(Font, Em, Px);

/// scale * T/U
pub struct Scale<T, U> {
    factor: f64,
    _t: PhantomData<T>,
    _u: PhantomData<U>,
}
impl<T, U> Scale<T, U> {
    pub fn new(factor: f64, _t: T, _u: U) -> Self {
        Scale { factor, _t: PhantomData, _u: PhantomData }
    }
    pub fn inv(self) -> Scale<U, T> {
        Scale { factor: 1.0 / self.factor, _t: PhantomData, _u: PhantomData }
    }
}

impl<T, U> Clone for Scale<T, U> {
    fn clone(&self) -> Self {
        Scale { factor: self.factor, _t: PhantomData, _u: PhantomData }
    }
}
impl<T, U> Copy for Scale<T, U> {}

impl<T, U> Mul<Scale<T, U>> for Length<U> {
    type Output = Length<T>;
    fn mul(self, rhs: Scale<T, U>) -> Length<T> {
        Length { value: self.value * rhs.factor, _m: PhantomData }
    }
}
impl<T, U> Div<Scale<T, U>> for Length<T> {
    type Output = Length<U>;
    fn div(self, rhs: Scale<T, U>) -> Length<U> {
        Length { value: self.value / rhs.factor, _m: PhantomData }
    }
}
impl<T, U, V> Mul<Scale<U, V>> for Scale<T, U> {
    type Output = Scale<T, V>;
    fn mul(self, rhs: Scale<U, V>) -> Scale<T, V> {
        Scale { factor: self.factor * rhs.factor, _t: PhantomData, _u: PhantomData }
    }
}
impl<T, U, V> Div<Scale<V, U>> for Scale<T, U> {
    type Output = Scale<T, V>;
    fn div(self, rhs: Scale<V, U>) -> Scale<T, V> {
        Scale { factor: self.factor / rhs.factor, _t: PhantomData, _u: PhantomData }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Unit {
    Em(f64),
    Px(f64)
}
