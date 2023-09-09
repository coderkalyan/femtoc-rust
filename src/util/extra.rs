#[derive(Debug, Clone, Copy)]
pub struct Data(u32);

impl Data {
    pub fn value(self) -> u32 {
        self.0
    }
}

impl From<u32> for Data {
    fn from(value: u32) -> Data {
        Data(value)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Index(u32);

impl Index {
    pub fn value(self) -> usize {
        self.0 as usize
    }
}

impl From<usize> for Index {
    fn from(value: usize) -> Index {
        assert!(value <= (u32::MAX as usize));
        Index(value as u32)
    }
}

impl From<u32> for Index {
    fn from(value: u32) -> Index {
        Index(value)
    }
}

impl Into<Data> for Index {
    fn into(self) -> Data {
        Data::from(self.0)
    }
}

impl std::ops::Add<u32> for Index {
    type Output = Index;

    fn add(self, rhs: u32) -> Index {
        assert!(self.0 <= (u32::MAX - rhs));
        Index(self.0 + rhs)
    }
}

pub type Vec = std::vec::Vec<Data>;

pub struct Slice<'a>(&'a [Data]);

impl <'a> std::ops::Index<Index> for Slice<'a> {
    type Output = Data;

    fn index(&self, index: Index) -> &'a Data {
        &self.0[index.value()]
    }
}

impl <'a> Slice<'a> {
    pub fn from(slice: &'a [Data]) -> Slice {
        Slice(slice)
    }
}

pub trait ExtraData<T> {
    fn pack(&self, vec: &mut Vec);
    fn unpack(slice: Slice, index: Index) -> T;
}

pub struct Range {
    pub start: Index,
    pub end: Index,
}
