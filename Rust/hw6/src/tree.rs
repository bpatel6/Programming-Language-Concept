use std::rc::Rc;
use std::borrow::{Borrow};

#[derive(Debug)]
pub struct BSTNode {
    value: i64,
    left_child: Option<Rc<BSTNode>>,
    right_child: Option<Rc<BSTNode>>,
}

impl BSTNode {
    pub fn new(v: i64) -> BSTNode {
        BSTNode {
            value: v,
            left_child: None,
            right_child: None,
        }
    }

    /// Creates a binary tree for a list of values. The values are in
    /// the same order as a preorder traversal (e.g., the first element
    /// is the root value). It is assumed there are no duplicate values
    /// in the list
    pub fn from_preorder_list(vals: &[i64]) -> Option<Rc<BSTNode>> {
        if let Some(first) = vals.first() {
            let mut node = BSTNode::new(*first);
            let i = (1..vals.len()).find(|&i| vals[i] > vals[0]).unwrap_or_else(|| vals.len());
            node.left_child = BSTNode::from_preorder_list(&vals[1..i]);
            node.right_child = BSTNode::from_preorder_list(&vals[i..]);
            Some(Rc::from(node))
        } else {
            None
        }
    }

    /// Returns an Option containing a shared pointer to the left child
    /// node, if it exists.
    pub fn left_child(&self) -> Option<Rc<BSTNode>> {
        let x = self.borrow().left_child.clone();
        if x.is_none() == false {
            x
        } else {
            None
        }
    }

    /// Returns an Option containing a shared pointer to the right child
    /// node, if it exists.
    pub fn right_child(&self) -> Option<Rc<BSTNode>> {
        let x = self.borrow().right_child.clone();
        if x.is_none() == false {
            x
        } else {
            None
        }
    }

    /// Returns the node value.
    pub fn value(&self) -> i64 {
        self.value
    }

    /// Preorder traversal of the node, calling the given function with the
    /// node value.
    pub fn preorder<F: FnMut(i64)>(&self, f: &mut F) {
        let mut stack = vec![];
        stack.push(self);
        while !stack.is_empty() {
            let node = stack.pop().unwrap();
            f(node.value);
            match node.right_child {
                None => {}
                Some(ref i) => stack.push(i)
            }
            match node.left_child {
                None => {}
                Some(ref i) => stack.push(i)
            }
        }
    }

    /// Inorder traversal of the node, calling the given function with the
    /// node value.
    pub fn inorder<F: FnMut(i64)>(&self, f: &mut F) {
        let mut stack = vec![];
        stack.push(self);
        let mut node = stack.pop().unwrap();
        loop {
            loop {
                match node.right_child {
                    None => {}
                    Some(ref i) => stack.push(i)
                }
                stack.push(node);
                match node.left_child {
                    None => break,
                    Some(ref i) => node = i
                }
            }
            node = stack.pop().unwrap();
            while !stack.is_empty() && node.right_child.is_none() {
                f(node.value);
                node = stack.pop().unwrap();
            }
            f(node.value);
            if stack.is_empty() {
                break;
            } else {
                node = stack.pop().unwrap();
            }
        }
    }

    /// Postorder traversal of the node, calling the given function with the
    /// node value.
    pub fn postorder<F: FnMut(i64)>(&self, f: &mut F) {
        let mut stack = vec![];
        let mut result = vec![];
        stack.push(self);
        while !stack.is_empty() {
            let node = stack.pop().unwrap();
            result.push(node);
            match node.left_child {
                None => {}
                Some(ref i) => stack.push(i)
            }
            match node.right_child {
                None => {}
                Some(ref i) => stack.push(i)
            }
        }
        let rev_result = result.iter().rev();
        for i in rev_result {
            f(i.value);
        }
    }

    /// Returns the total number of nodes in the tree rooted at self (including
    /// self).
    pub fn size(&self) -> usize {
        let mut count : usize = 0;
        let mut stack = vec![];
        stack.push(self);
        while !stack.is_empty() {
            let node = stack.pop().unwrap();
            count += 1;
            match node.right_child {
                None => {}
                Some(ref n) => stack.push(n)
            }
            match node.left_child {
                None => {}
                Some(ref n) => stack.push(n)
            }
        }
        count
    }
}

#[derive(Debug)]
pub struct BST {
    root: Option<Rc<BSTNode>>,
}

impl BST {
    /// Construct a BST from a preorder list.
    pub fn from_preorder_list(vals: &[i64]) -> BST {
        return BST {
            root: BSTNode::from_preorder_list(vals),
        };
    }

    /// Apply the function using a preorder traversal.
    pub fn preorder<F: FnMut(i64)>(&self, mut f: F) {
        let root = self.borrow().root.clone();
        BSTNode::preorder(&*root.unwrap(), &mut f)
    }

    /// Apply the function using a postorder traversal.
    pub fn postorder<F: FnMut(i64)>(&self, mut f: F) {
        let root = self.borrow().root.clone();
        BSTNode::postorder(&*root.unwrap(), &mut f)
    }

    /// Apply the function using an inorder traversal.
    pub fn inorder<F: FnMut(i64)>(&self, mut f: F) {
        let root = self.borrow().root.clone();
        BSTNode::inorder(&*root.unwrap(), &mut f)
    }

    /// Get the total number of nodes in the BST.
    pub fn size(&self) -> usize {
        let root = self.borrow().root.clone();
        if root.is_none() {
            0
        } else {
            BSTNode::size(&*root.unwrap())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_preorder_list() {
        let vals = vec![1, 2, 3, 4, 5];
        let bst = BST::from_preorder_list(&vals);
        assert!(bst.root.is_some());
        let mut runner = Rc::clone(&bst.root.unwrap());
        vals.iter().for_each(|x| {
            assert!(runner.left_child().is_none());
            assert_eq!(runner.value(), *x);
            if *x < 5 {
                runner = Rc::clone(&runner).right_child().unwrap();
            }
        });
    }

    #[test]
    fn test_preorder_traversal() {
        let vals = vec![3, 1, 0, 2, 5, 4, 6];
        let bst = BST::from_preorder_list(&vals);
        assert!(bst.root.is_some());
        let mut idx = 0;
        bst.preorder(|x| {
            assert_eq!(x, vals[idx]);
            //println!("expected idx = {}, val = {}, result = {}", idx, vals[idx], x);
            idx += 1;
        });
    }

    #[test]
    fn test_postorder_traversal() {
        let expected = vec![0, 2, 1, 4, 6, 5, 3];
        let bst = BST::from_preorder_list(&vec![3, 1, 0, 2, 5, 4, 6]);
        assert!(bst.root.is_some());
        let mut idx = 0;
        bst.postorder(|x| {
            assert_eq!(x, expected[idx]);
            //println!("expected idx = {}, val = {}, result = {}", idx, expected[idx], x);
            idx += 1;
        });
    }

    #[test]
    fn test_inorder_traversal() {
        let expected = vec![0, 1, 2, 3, 4, 5, 6];
        let bst = BST::from_preorder_list(&vec![3, 1, 0, 2, 5, 4, 6]);
        assert!(bst.root.is_some());
        let mut idx = 0;
        bst.inorder(|x| {
            assert_eq!(x, expected[idx]);
            //println!("expected idx = {}, val = {}, result = {}", idx, expected[idx], x);
            idx += 1;
        });
    }

    #[test]
    fn test_tree_size() {
        let vals = vec![1, 2, 3, 4, 5];
        let bst = BST::from_preorder_list(&vals);
        assert_eq!(bst.size(), vals.len());
    }
}
