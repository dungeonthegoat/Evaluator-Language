# Overview
Evaluator is a high-level, functional programming language written in [Silver](https://melt.cs.umn.edu/silver/) and was created for a school project. It uses eager evaluation, and each file is treated as one expression that is evaluated.
# Data types
The Evaluator language supports primitive data types such as ```int```, ```float```, ```bool```, and ```string```. On top of this, it also support algebraic data types:
```type MyType = OneType(int) | MultipleTypes(MyType, MyType)```
# Syntax
## Variable Declarations
```
let x = 12;
x + 1 -- Evaluates to 13
```
## Functions
While regular variables use basic type inference, the Function return type and argument types must be explicitly provided.
```
let int add int x int y = x + y;
add 2 3 -- Evaluated to 5
```
## Recursion and Pattern Matching
The Evaluator language supports recursive functions (written the same as any function) and pattern matching on data:
```
let int sumTree Tree t = ? t {
  | Branch(l, v, r) -> sumTree l + v + sumTree r
  | Leaf(v) -> v
};
```
