# WASM Redex Model
This is a model of the WebAssembly language syntax, semantics, and (in progress) validation rules.
The model is based off of the [2017 PLDI paper by Haas et al](https://dl.acm.org/doi/10.1145/3062341.3062363).

## Syntax
The syntactic representation used in the model is `s-expression` based.
It contains a few more parentheses than are present in the original grammer (to speed up parsing).
Other small differences include:
* The removal of the `.` character between types and a number of terminal expressions (e.g., `(i32.add)` becomes `(i32 add)`).
* Optional terms are handled via enumeration or faked using lists (there's a hidden low-priority TODO to clean this up).

The WASM Redex language is defined in `Syntax.rkt`. A typeset version can be viewed below and uses similar terminology to the 

![The WebAssembly language syntax](Syntax.pdf)



## Semantics
The reduction relation is in the form of a small-step operational semantics inside an evaluation context.
The evaluation context keeps track of the list of instructions surrounding the current code block.
Evaluation contexts can be thought of as being akin to a stack frame.
There are four parameters: `(s j v* e*) -> (s j v* e*)` with roughly equivalent meaning to the ones in the paper:
* `s`: the store which keeps track of all instances (modules), as well as all function tables and memories.
* `j`: the index of the current instance in which execution is taking place (used interchangeably with `i`).
* `v*`: the list of local variables (referred to by an index).
* `e*`: the list of instructions. Since values are expressions it is more helpful to think of this as `v* e*` where `v*` is the stack values and `e*` is the instruction list.

## Validation (In Progress)
