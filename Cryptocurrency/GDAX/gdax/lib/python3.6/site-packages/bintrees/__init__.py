#!/usr/bin/env python
#coding:utf-8
# Author:  mozman
# Purpose: binary trees package
# Created: 03.05.2010
# Copyright (c) 2010-2013 by Manfred Moitzi
# License: MIT License

from __future__ import absolute_import

__doc__ = """
Binary Tree Package
===================

Python Trees
------------

Balanced and unbalanced binary trees written in pure Python with a dict-like API.

Classes
~~~~~~~
* BinaryTree -- unbalanced binary tree
* AVLTree -- balanced AVL-Tree
* RBTree -- balanced Red-Black-Tree

Cython Trees
------------

Basic tree functions written in Cython/C, merged with _ABCTree() to provide the
full API of the Python Trees.

Classes
~~~~~~~

* FastBinaryTree -- unbalanced binary tree
* FastAVLTree -- balanced AVLTree
* FastRBTree -- balanced Red-Black-Tree

Overview of API for all Classes
===============================

* TreeClass ([compare]) -> new empty tree.
* TreeClass(mapping, [compare]) -> new tree initialized from a mapping
* TreeClass(seq, [compare]) -> new tree initialized from seq [(k1, v1), (k2, v2), ... (kn, vn)]

Methods
-------

* __contains__(k) -> True if T has a key k, else False, O(log(n))
* __delitem__(y) <==> del T[y], O(log(n))
* __getitem__(y) <==> T[y], O(log(n))
* __iter__() <==> iter(T)
* __len__() <==> len(T), O(1)
* __max__() <==> max(T), get max item (k,v) of T, O(log(n))
* __min__() <==> min(T), get min item (k,v) of T, O(log(n))
* __and__(other) <==> T & other, intersection
* __or__(other) <==> T | other, union
* __sub__(other) <==> T - other, difference
* __xor__(other) <==> T ^ other, symmetric_difference
* __repr__() <==> repr(T)
* __setitem__(k, v) <==> T[k] = v, O(log(n))
* clear() -> None, Remove all items from T, , O(n)
* copy() -> a shallow copy of T, O(n*log(n))
* discard(k) -> None, remove k from T, if k is present, O(log(n))
* get(k[,d]) -> T[k] if k in T, else d, O(log(n))
* is_empty() -> True if len(T) == 0, O(1)
* items([reverse]) -> list of T's (k, v) pairs, as 2-tuple, O(n)
* keys([reverse]) -> list of T's keys, O(n)
* values([reverse]) -> list of T's values, O(n)
* pop(k[,d]) -> v, remove specified key and return the corresponding value, O(log(n))
* pop_item() -> (k, v), remove and return some (key, value) pair as a 2-tuple, O(log(n))
* set_default(k[,d]) -> T.get(k, d), also set T[k]=d if k not in T, O(log(n))
* update(E) -> None.  Update T from dict/iterable E, O(E*log(n))
* iter_items(s, e, reverse) -> generator for (k, v) items of T for s <= key < e, O(n)

walk forward/backward, O(log(n))

* prev_item(key) -> get (k, v) pair, where k is predecessor to key, O(log(n))
* prev_key(key) -> k, get the predecessor of key, O(log(n))
* succ_item(key) -> get (k,v) pair as a 2-tuple, where k is successor to key, O(log(n))
* succ_key(key) -> k, get the successor of key, O(log(n))

slicing by keys

* item_slice(s, e, reverse) -> generator for (k, v) items of T for s <= key < e, O(n), synonym for iter_items(...)
* key_slice(s, e, reverse) -> generator for keys of T for s <= key < e, O(n)
* value_slice(s, e, reverse) -> generator for values of T for s <= key < e, O(n)
* T[s:e] -> TreeSlice object, with keys in range s <= key < e, O(n)
* del T[s:e] -> remove items by key slicing, for s <= key < e, O(n)

if 's' is None or T[:e] TreeSlice/iterator starts with value of min_key()
if 'e' is None or T[s:] TreeSlice/iterator ends with value of max_key()
T[:] is a TreeSlice which represents the whole tree.

The step argument of the regular slicing syntax T[s:e:step] will silently ignored.

TreeSlice is a tree wrapper with range check, and contains no references
to objects, deleting objects in the associated tree also deletes the object
in the TreeSlice.

* TreeSlice[k] -> get value for key k, raises KeyError if k not exists in range s:e
* TreeSlice[s1:e1] -> TreeSlice object, with keys in range s1 <= key < e1

  * new lower bound is max(s, s1)
  * new upper bound is min(e, e1)

TreeSlice methods:

* items() -> generator for (k, v) items of T, O(n)
* keys() -> generator for keys of T, O(n)
* values() -> generator for values of  T, O(n)
* __iter__ <==> keys()
* __repr__ <==> repr(T)
* __contains__(key)-> True if TreeSlice has a key k, else False, O(log(n))

Heap methods

* max_item() -> get biggest (key, value) pair of T, O(log(n))
* max_key() -> get biggest key of T, O(log(n))
* min_item() -> get smallest (key, value) pair of T, O(log(n))
* min_key() -> get smallest key of T, O(log(n))
* pop_min() -> (k, v), remove item with minimum key, O(log(n))
* pop_max() -> (k, v), remove item with maximum key, O(log(n))
* nlargest(i[,pop]) -> get list of i largest items (k, v), O(i*log(n))
* nsmallest(i[,pop]) -> get list of i smallest items (k, v), O(i*log(n))

Set methods (using frozenset)

* intersection(t1, t2, ...) -> Tree with keys *common* to all trees
* union(t1, t2, ...) -> Tree with keys from *either* trees
* difference(t1, t2, ...) -> Tree with keys in T but not any of t1, t2, ...
* symmetric_difference(t1) -> Tree with keys in either T and t1  but not both
* is_subset(S) -> True if every element in T is in S
* is_superset(S) -> True if every element in S is in T
* is_disjoint(S) ->  True if T has a null intersection with S

Classmethods

* from_keys(S[,v]) -> New tree with keys from S and values equal to v.

Helper functions

* bintrees.has_fast_tree_support() -> True if Cython extension is working else False (False = using pure Python implementation)

"""

from .bintree import BinaryTree
from .avltree import AVLTree
from .rbtree import RBTree


def has_fast_tree_support():
    return FastBinaryTree is not BinaryTree


try:
    from .cython_trees import FastBinaryTree
except ImportError:  # fall back to pure Python version
    FastBinaryTree = BinaryTree

try:
    from .cython_trees import FastAVLTree
except ImportError:  # fall back to pure Python version
    FastAVLTree = AVLTree

try:
    from .cython_trees import FastRBTree
except ImportError:  # fall back to pure Python version
    FastRBTree = RBTree
