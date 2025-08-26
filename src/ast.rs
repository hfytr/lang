use core::slice::GetDisjointMutIndex;
use std::{
    cmp::Ordering, fmt::Display, ops::{Index, IndexMut}
};

use crate::{ast, parser::NodeKind};

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
    VarKids(NodeList),
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

#[derive(Default, Debug)]
pub struct NodeList {
    nodes: Vec<AVLNode>,
    root: usize,
}

impl NodeList {
    pub fn from_nodes(ast: &AST, elems: impl Iterator<Item = usize>) -> Self {
        let mut ret = Self::default();
        for node in elems {
            ret.push(node, ast[node].span.clone());
        }
        ret
    }

    pub fn find(&self, pos: (usize, usize)) -> Option<usize> {
        let mut cur = 0;
        while self.nodes[cur].ast_node.is_none() {
            cur = if let Some(left) = self.nodes[cur].left
                && self.nodes[left].span.contains(pos).is_eq()
            {
                left
            } else if let Some(right) = self.nodes[cur].right
                && self.nodes[right].span.contains(pos).is_eq()
            {
                right
            } else {
                return None;
            }
        }
        self.nodes[cur].ast_node
    }

    pub fn push(&mut self, ast_node: usize, span: Span) {
        if self.nodes.is_empty() {
            self.nodes.push(AVLNode { span, left: None, right: None, parent: None, ast_node: None, balance: 0 });
            return;
        }
        let get_next = |cur: &AVLNode| match cur.span.partial_cmp(&span) {
            Some(Ordering::Equal) | None => panic!(),
            Some(Ordering::Less) => cur.right,
            Some(Ordering::Greater) => cur.left,
        };
        let mut cur = self.root;
        let mut maybe_next = get_next(&self.nodes[cur]);
        while let Some(next) = maybe_next {
            cur = next;
            maybe_next = get_next(&self.nodes[cur])
        }
        let on_left = match self.nodes[cur].span.partial_cmp(&span) {
            Some(Ordering::Equal) | None => panic!(),
            Some(Ordering::Less) => {
                self.nodes[cur].right = Some(self.nodes.len());
                self.nodes[cur].balance += 1;
                false
            }
            Some(Ordering::Greater) => {
                self.nodes[cur].left = Some(self.nodes.len());
                self.nodes[cur].balance -= 1;
                true
            }
        };
        self.nodes.push(AVLNode {
            parent: Some((cur, on_left)),
            left: None,
            right: None,
            balance: 0,
            span,
            ast_node: Some(ast_node),
        });
        while self.nodes[cur].balance != 0 && let Some((parent, on_left)) = self.nodes[cur].parent {
            if on_left && self.nodes[parent].balance <= 0 {
                self.nodes[parent].balance -= 1;
            } else if !on_left && self.nodes[parent].balance >= 0 {
                self.nodes[parent].balance += 1;
            }
            cur = match (self.nodes[parent].balance, self.nodes[cur].balance) {
                (2, 0..) => {
                    self.rotate_left(parent);
                    self.nodes[parent].balance -= 1 + self.nodes[cur].balance;
                    self.nodes[cur].balance -= 1;
                    cur
                }
                (2, ..0) => {
                    self.rotate_right(cur);
                    self.rotate_left(parent);
                    let child = *self.nodes[cur].left.as_ref().unwrap();
                    self.nodes[cur].balance += 1 + self.nodes[child].balance;
                    self.nodes[child].balance += 1;
                    self.nodes[parent].balance -= 1 + self.nodes[cur].balance;
                    self.nodes[cur].balance -= 1;
                    child
                }
                (-2, 1..) => {
                    self.rotate_left(cur);
                    self.rotate_right(parent);
                    let child = *self.nodes[cur].right.as_ref().unwrap();
                    self.nodes[cur].balance -= 1 + self.nodes[child].balance;
                    self.nodes[child].balance -= 1;
                    self.nodes[parent].balance += 1 + self.nodes[cur].balance;
                    self.nodes[cur].balance += 1;
                    child
                }
                (-2, ..1) => {
                    self.rotate_right(parent);
                    self.nodes[parent].balance += 1 + self.nodes[cur].balance;
                    self.nodes[cur].balance += 1;
                    cur
                }
                _ => parent
            }
        }
    }

    fn rotate_left(&mut self, top: usize) {
        let Some(right) = self.nodes[top].right else {
            return;
        };
        let on_left = self.nodes[top].parent.map(|p| p.1).unwrap_or(false);
        self.set_child(self.nodes[top].parent.map(|p| p.0), on_left, Some(right));
        self.set_child(Some(top), false, self.nodes[right].left);
        self.set_child(Some(right), true, Some(top));
    }

    fn rotate_right(&mut self, top: usize) {
        let Some(left) = self.nodes[top].left else {
            return;
        };
        let on_left = self.nodes[top].parent.map(|p| p.1).unwrap_or(false);
        self.set_child(self.nodes[top].parent.map(|p| p.0), on_left, Some(left));
        self.set_child(Some(top), false, self.nodes[left].right);
        self.set_child(Some(left), true, Some(top));
    }

    fn set_child(&mut self, parent: Option<usize>, left: bool, child: Option<usize>) {
        if let Some(child) = child {
            self.nodes[child].parent = parent.map(|p| (p, left));
        }
        if let Some(parent) = parent
            && left
        {
            self.nodes[parent].left = child;
        } else if let Some(parent) = parent
            && !left
        {
            self.nodes[parent].right = child;
        } else if let Some(child) = child {
            self.root = child;
        }
    }

    pub fn iter<'a>(&'a self) -> NodeListIter<'a> {
        let mut cur = 0;
        while let Some(left) = self.nodes[cur].left
            && self.nodes[cur].left.is_some()
        {
            cur = left;
        }
        NodeListIter { list: self, cur }
    }

    /// for use in tests. panics instead of returning false
    pub fn verify(&self) {
        let mut heights = vec![1; self.nodes.len()];
        self.get_heights(&mut heights, &Some(self.root));
        let mut stack = vec![self.root];
        while let Some(cur) = stack.pop() {
            let left_height = self.nodes[cur].left.map(|l| heights[l]).unwrap_or(0) as i32;
            let right_height = self.nodes[cur].right.map(|r| heights[r]).unwrap_or(0) as i32;
            assert_eq!(right_height - left_height, self.nodes[cur].balance as i32);
            if let Some(left) = self.nodes[cur].left {
                stack.push(left);
                let (parent, on_left) = self.nodes[left].parent.unwrap();
                assert!(self.nodes[left].span < self.nodes[cur].span);
                assert_eq!(parent, cur);
                assert!(on_left)
            }
            if let Some(right) = self.nodes[cur].right {
                stack.push(right);
                let (parent, on_left) = self.nodes[right].parent.unwrap();
                assert!(self.nodes[right].span > self.nodes[cur].span);
                assert_eq!(parent, cur);
                assert!(!on_left)
            }
        }
    }

    fn get_heights(&self, heights: &mut Vec<usize>, cur: &Option<usize>) -> usize {
        let Some(cur) = *cur else {return 0 };
        let left_height = self.get_heights(heights, &self.nodes[cur].left);
        let right_height = self.get_heights(heights, &self.nodes[cur].right);
        heights[cur] += left_height.max(right_height);
        heights[cur]
    }
}

impl Display for NodeList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut stack = vec![(self.root, 0)];
        while let Some((top, indent)) = stack.pop() {
            writeln!(
                f,
                "{}{top}: {:?} ast: {:?} balance: {} parent: {:?}",
                " ".repeat(indent * 2),
                self.nodes[top].span,
                self.nodes[top].ast_node,
                self.nodes[top].balance,
                self.nodes[top].parent,
            )?;
            if let Some(right) = self.nodes[top].right {
                stack.push((right, indent + 1))
            } else {
                writeln!(f, "{}None", " ".repeat((indent+1) * 2),)?;
            }
            if let Some(left) = self.nodes[top].left {
                stack.push((left, indent + 1))
            } else {
                writeln!(f, "{}None", " ".repeat((indent+1) * 2),)?;
            }
        }
        Ok(())
    }
}

pub struct NodeListIter<'a> {
    list: &'a NodeList,
    cur: usize,
}

impl<'a> Iterator for NodeListIter<'a> {
    type Item = usize;
    fn next(&mut self) -> Option<Self::Item> {
        while let Some(parent) = self.list.nodes[self.cur].parent && self.list.nodes[parent.0].right.unwrap_or(self.cur) == self.cur {
            self.cur = parent.0;
        }
        self.cur = *self.list.nodes[self.cur].right.as_ref().unwrap();
        while let Some(left) = self.list.nodes[self.cur].left && self.list.nodes[left].left.is_some() {
            self.cur = left;
        }
        Some(self.list.nodes[self.cur].ast_node.unwrap())
    }
}

#[derive(Debug)]
struct AVLNode {
    span: Span,
    left: Option<usize>,
    right: Option<usize>,
    parent: Option<(usize, bool)>,
    ast_node: Option<usize>,
    balance: i8,
}
