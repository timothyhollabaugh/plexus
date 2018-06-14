pub trait FromTopology<T> {
    fn from_topology(other: T) -> Self;
}

pub trait IntoView<T> {
    fn into_view(self) -> T;
}

impl<T, U> IntoView<U> for T
where
    U: FromTopology<T>,
{
    fn into_view(self) -> U {
        U::from_topology(self)
    }
}
