# WASM Redex Model
This is a model of the WebAssembly language syntax, semantics, and validation rules, written in [PLT Redex](https://redex.racket-lang.org/).
The model is based off of the [2017 PLDI paper by Haas et al](https://dl.acm.org/doi/10.1145/3062341.3062363).

The goal of this model is to provide a starting point for modeling extensions to the language.
For example, for a research project we have built an extended type system for WebAssembly on top of this model.
There are two straightforward ways to build language extensions using this model:

1. The preferred way is to use Redex's `define-extended-*` forms to explicity extend the basic WebAssembly specification. For example, using `define-extended-language` to extend the base WebAssembly syntax with new types or instructions, and then using `define-extended-relation` and `define-extended-judgment` when necessary, or creating new reduction relations and judgement forms.
2. Create a fork of this repository, and change the language definition, reduction relation, and judgment forms as needed.

## Syntax
The syntactic representation used in the model is `s-expression` based.
It contains a few more parentheses than are present in the original grammer (to speed up parsing).
Other small differences include:
* The removal of the `.` character between types and a number of terminal expressions (e.g., `i32.add` becomes `(i32 add)`).
* The explicit enumeration of the `sx` non-terminal in binops and relops.
* Optional terms are handled via enumeration or faked using lists (there's a hidden low-priority TODO to clean this up).

The WASM Redex language is defined in `Syntax.rkt`. A typeset version can be viewed below and uses similar terminology to the WebAssembly paper.

![The WebAssembly language syntax](Syntax.pdf)

## Semantics
The reduction relation is in the form of a small-step operational semantics inside an evaluation context.
The evaluation context keeps track of the list of instructions surrounding the current code block.
Evaluation contexts can be thought of as being akin to a stack frame.
There are four parameters: `(s v* e*) (-> i) (s v* e*)` with roughly equivalent meaning to the ones in the paper:
* `s`: the store which keeps track of all instances (modules), as well as all function tables and memories.
* `v*`: the list of local variables (referred to by an index).
* `e*`: the list of instructions. Since values are expressions it is more helpful to think of this as `v* e*` where `v*` is the stack values and `e*` is the instruction list.
* `i`: the index of the current instance in which execution is taking place.

The WASM Redex reduction semantics are defined in `Semantics/Semantics.rkt`.

The function `->` takes an instance number as a natural number and produces a `reduction-relation` for reducing terms under the given instance number.

## Validation
The type system is in the form of deduction rules on a context `C`, a sequence of instructions `e*`, and a function type `tf`.
The typing rules for instructions are defined in `Validation/InstructionTyping.rkt`, which provides the judgment-form `⊢`.

The typing rules for modules and module objects (tables, memories, globals, and functions) are defined in `Validation/ModuleTyping.rkt`,
which provides the `⊢-module-func`, `⊢-module-global`, `⊢-module-table`, `⊢-module-memory`, and `⊢-module` judgment-forms.

In `Validation/Typechecking.rkt` we provide an algorithm for finding a derivation that types a given syntax object.
This algorithm is more complicated than the reference validation algorithm since it needs to synthesize the actual program stacks for instructions after unconditional branches and `unreachable`.

The typechecking algorithm is split between typechecking functions for each judgment-form.
* `typecheck-module` produces derivations of `⊢-module` for a given `mod`.
* `typecheck-table` produces derivations of `⊢-module-table` for a given context and `tab`.
* `memory-derivation` produces derivations of `⊢-module-memory` for a given context and `mem`.
* `typecheck-global` produces derivations of `⊢-module-global` for a given context and `glob`.
* `typecheck-func` produces derivations of `⊢-module-func` for a given context and `func`.
* `typecheck-ins` produces derivaitions of `⊢` for a given context, `e*`, and pre- and post-stacks.
