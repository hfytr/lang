use core::slice::GetDisjointMutIndex;
use std::{
    cmp::Ordering, ops::{Index, IndexMut}
};

use crate::parser::NodeKind;

#[derive(Debug, Default)]
pub struct AST {
    arena: Vec<Node>,
    files: Vec<String>,
}

impl AST {
    pub fn push(&mut self, new: Node) -> usize {
        self.arena.push(new);
        self.arena.len() - 1
    }

    pub fn get_mut<const N: usize>(&mut self, idxs: [usize; N]) -> [&mut Node; N] {
        self.arena
            .get_disjoint_mut(idxs)
            .expect("Overlapping indices in call to ast.get_mut()")
    }

    pub fn add_file(&mut self, fname: String) {
        self.files.push(fname);
    }
}

impl Index<usize> for AST {
    type Output = Node;
    fn index(&self, i: usize) -> &Self::Output {
        &self.arena[i]
    }
}

impl IndexMut<usize> for AST {
    fn index_mut(&mut self, i: usize) -> &mut Self::Output {
        &mut self.arena[i]
    }
}

#[derive(Debug)]
pub struct Node {
    pub kind: NodeKind,
    pub span: Span,
    pub data: NodeData,
}

#[derive(Debug)]
pub enum NodeData {
    IntLit(i128),
    UIntLit(u128),
    FloatLit(f64),
    String(String),
    Binary(Option<usize>, Option<usize>),
    Single(Option<usize>),
    None,
    Error,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Span {
    file: usize,
    start: (usize, usize),
    end: (usize, usize),
}

impl Span {
    pub fn new(file: usize, start: (usize, usize), end: (usize, usize)) -> Self {
        Self { file, start, end }
    }

    pub fn merge(&self, rhs: &Self) -> Self {
        assert_eq!(self.file, rhs.file);
        Self {
            file: self.file,
            start: self.start.min(rhs.start),
            end: self.end.max(rhs.end),
        }
    }

    pub fn merge_with(&mut self, rhs: &Self) {
        self.start = self.start.min(rhs.start);
        self.end = self.end.max(rhs.end);
    }

    pub fn contains(&self, (line, col): (usize, usize)) -> Ordering {
        if !(self.start.0 + 1..self.end.0).contains(&line) {
            return Ordering::Equal;
        }
        if self.start.0 == line && self.start.0 == self.end.0 {
            let col_range = self.start.1..self.end.1;
            if col_range.contains(&col) {
                Ordering::Equal
            } else if col < col_range.start {
                Ordering::Less
            } else {
                Ordering::Greater
            }
        } else if self.start.0 == line {
            if (self.start.1..).contains(&col) {
                Ordering::Equal
            } else {
                Ordering::Less
            }
        } else if self.end.0 == line {
            if (..self.end.0).contains(&col) {
                Ordering::Equal
            } else {
                Ordering::Greater
            }
        } else {
            panic!();
        }
    }

    fn disjoint<'a>(
        &'a self,
        mut rhs: &'a Span,
    ) -> bool {
        let mut lhs = self;
        if lhs.file != rhs.file {
            return true
        }
        if !(lhs.start.0..lhs.end.0).is_overlapping(&(rhs.start.0..rhs.end.0)) {
            return true;
        }
        if lhs.start.0 > rhs.start.0 || lhs.start.0 == rhs.start.0 && lhs.start.1 > rhs.start.1 {
            std::mem::swap(&mut lhs, &mut rhs);
        }
        if lhs.end.0 != rhs.start.0 {
            return false;
        }
        let col_range_1 = if lhs.start.0 == lhs.end.0 {
            lhs.start.1..lhs.end.1
        } else {
            0..lhs.end.1
        };
        let col_range_2 = if rhs.start.0 == rhs.end.0 {
            rhs.start.1..rhs.end.1
        } else {
            0..rhs.end.1
        };
        !col_range_1.is_overlapping(&col_range_2)
    }
}

impl PartialOrd for Span {
    fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
        assert_eq!(self.file, rhs.file);
        self.disjoint(rhs).then(|| {
            if self.start.0 != rhs.start.0 {
                return self.start.0.cmp(&rhs.start.0);
            }
            self.start.1.cmp(&rhs.start.1)
        })
    }
}
