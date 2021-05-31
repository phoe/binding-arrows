# Binding Arrows

An implementation of threading macros based on binding anonymous variables.

## Overview

This system implements binding threading macros - a kind of threading macros with different semantics than classical, [Clojure core threading macros](https://clojure.org/guides/threading_macros) or their extension, [swiss-arrows](https://github.com/rplevy/swiss-arrows). Two Common Lisp implementations of those are [arrows](https://github.com/Harleqin/arrows) and [arrow-macros](https://github.com/hipeta/arrow-macros).

This system is a fork of [arrows](https://github.com/Harleqin/arrows) with changes in semantics that make it impossible to merge back upstream.

## What's the difference?

A binding threading macro implicitly binds a variable on each computation step, as opposed to working purely on the syntactic level like the classical threading macros.

This has three main implications:

* Binding threading macros expand into a `let*` form.
  * Binding threading macros are nicer to read when macroexpanded.
  * Binding threading macros preserve intermediate binding steps for the debugger.
  * `setf` expansions are handled by explicit `setf` expanders for each macro.
* Binding threading macros assume that it is possible to evaluate each form resulting from threading each computation step.
  * This also means that e.g. `(-> foo (defun (bar) (1+ bar)))` is going to expand into a correct `defun` form on a traditional threading macro implementation, but will fail on a binding implementation (e.g. this one).
* Expansions of binding threading macros perform the aforementioned evaluation.
  * This means that e.g. `(->> (loop) (or t))` is going to return `t` on a traditional (Clojure-like) implementation of threading macros, but **will hang on a binding implementation** (e.g. this one).


## Contents

This system contains a package `binding-macros` that exports the following symbols:

* threading macros `->` and `->>`,
* diamond threading macros `-<>` and `-<>>`,
* short-circuiting threading macros `some->` and `some->>`,
* short-circuiting diamond threading macros `some-<>` and `some-<>>`,
* conditional threading macros `cond->` and `cond->>`,
* conditional diamond threading macros `cond-<>` and `cond-<>>`,
* inverted threading macro `->*`,
* named threading macro `as->`,
* inverted named threading macro `as->*`.

All of the aforementioned threading macros name valid places for use in `setf`.

## Loading

`(ql:quickload :binding-arrows)`

## Testing

`(asdf:test-system :binding-arrows)`

## Manual pages

* [Tutorial](doc/TUTORIAL.md)
* [Documentation](doc/DOCUMENTATION.md)
* [Examples](doc/EXAMPLES.md)

## License

MIT.
