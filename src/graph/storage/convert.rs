use graph::storage::{Storage, Topological};

pub trait AsStorage<T>
where
    T: Topological,
{
    fn as_storage(&self) -> &Storage<T>;
}

impl<'a, T, U> AsStorage<T> for &'a U
where
    T: Topological,
    U: AsStorage<T>,
{
    fn as_storage(&self) -> &Storage<T> {
        <U as AsStorage<T>>::as_storage(self)
    }
}

impl<'a, T, U> AsStorage<T> for &'a mut U
where
    T: Topological,
    U: AsStorage<T>,
{
    fn as_storage(&self) -> &Storage<T> {
        <U as AsStorage<T>>::as_storage(self)
    }
}

pub trait AsStorageMut<T>
where
    T: Topological,
{
    fn as_storage_mut(&mut self) -> &mut Storage<T>;
}

impl<'a, T, U> AsStorageMut<T> for &'a mut U
where
    T: Topological,
    U: AsStorageMut<T>,
{
    fn as_storage_mut(&mut self) -> &mut Storage<T> {
        <U as AsStorageMut<T>>::as_storage_mut(self)
    }
}
