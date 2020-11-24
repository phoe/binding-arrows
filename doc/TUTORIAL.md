# Binding Arrows - Tutorial

## Overview

The name "threading macros" has nothing to do with multiprocessing. Instead, the term "thread" refers to a piece of string that is driven by a needle through multiple layers of fabric. Threading macros work in a similar way, driving results of evaluating previous expressions into next expressions.

Traditional threading macros have their origins in [Clojure](https://clojure.org/guides/threading_macros) and have been further extended by a library named [swiss-arrows](https://github.com/rplevy/swiss-arrows). From there, they have found their way back into Common Lisp, and from there, I have created the variant of the traditional arrows ~~[as a failed experiment in rewriting `arrows` to use `let*`](https://github.com/Harleqin/arrows/pull/3)~~.

This tutorial covers binding threading macros first and foremost and does not fully apply to traditional binding macros. It should, however, allow you to learn the basics of traditional binding macros, if you look away a little whenever you see a `let*` and instead pretend that the individual bindings are not meant to undergo evaluation.

Henceforth, we will refer to binding threading macros as just threading macros.

Binding threading macros can be understood as operators that sequentially bind anonymous variables. We do not define any names for these variables (hence "anonymous"); instead, we declare where the result of the previous expression, bound to an automatically generated anonymous variable, should go in the current expression.

Different threading macros deviate from this general description; sometimes they bind only one variable, sometimes the bound variable is named, but the general skeleton, which can always be understood in terms of `let*`, stays the same.

## Basics

A threading macro is equivalent to a series of lexical variable bindings, where each binding (except for the first) refers to the value of the preceding binding. For example, let us consider a call to `->`, the simplest threading macro:

```lisp
(-> foo
  bar
  (baz)
  (quux 1 2 3))
```

This call is equivalent to:

```lisp
(let ((temp1 foo)
      (temp2 (bar temp1))
      (temp3 (baz temp2))
      (temp4 (quux temp3 1 2 3)))
  temp4)
```

* We can see that the temporary variable `temp1` is bound to the value of the first provided form, `foo`.
* Next, the variable `temp2` is bound to the value of `bar` called on the value of `temp1`. Because the form provided to the threading macro is a symbol, it is interpreted as the name of a function/macro/special operator.
* Next, the variable `temp3` is bound to the value of `baz` called on the value of `temp2`. In this case, the value provided to the threading macro is a list, so the variable `temp2` is inserted as the first argument to this call.
* Next, the variable `temp4` is bound to the value of `quux` called on four arguments. The variable `temp3` is spliced into the call as the first argument, before the original three arguments that were provided in the call to the threading macro.
* Finally, the value of `temp4` is returned.

Threading macros usually come in two flavors: some thread the first argument into each form, and some thread the last. We name the former "thread-first macros" and the latter "thread-last macros".

Thread-last macros can be identified by the double angle bracket in their name, e.g. `->>`, `-<>>`, `some->>`, `cond->>`.

If we used a thread-last macro instead:

```lisp
(->> foo
  bar
  (baz)
  (quux 1 2 3))
```

Then the equivalent resultant form would look like this:

```lisp
(let ((temp1 foo)
      (temp2 (bar temp1))
      (temp3 (baz temp2))
      (temp4 (quux 1 2 3 temp3)))
  temp4)
```

We can see that the first three bindings were not changed. However, the fourth one has the temporary variable spliced into the call as the *last* argument, as opposed to the *first*.

## Diamond threading macros 

Sometimes this behavior is not enough when we need to splice the value in the middle, e.g. as a second argument out of three. The diamond threading macros solve this problem. If the form contains a "diamond" (which is any symbol named `"<>"`), then the variable is spliced in its location; otherwise, they behave like standard threading macros.

For example, the following call:

```lisp
(-<> 42
  (foo "one" "two" "three")
  (bar 1 2 <> 3 4)
  (baz :quux 24 :fred <>))
```

Is equivalent to:

```lisp
(let* ((temp1 42)
       (temp2 (foo temp1 "one" "two" "three"))
       (temp3 (bar 1 2 temp2 3 4))
       (temp4 (baz :quux 24 :fred temp3)))
  temp4)
```

We can see that in the second binding, where no diamond was specified in the form passed to the threading macro, the variable was spliced in as the first argument. (It will be the last argument if we instead use the thread-last variant, `-<>>`.)

However, the third form contained a diamond in the middle that was replaced with the variable from the previous binding; a similar situation occurs in the fourth binding, where the argument was instead spliced at the end.

## Named threading macros

Diamond threading macros have an issue where the diamonds are only allowed to occur on the outermost level of the form. For instance, the following code will not work:

```lisp
(-<> 42
  (list (list <>))
  print)
```

A named threading macro is capable of solving this issue by introducing a named variable that subsequent bindings can directly refer to in order to be able to use it anywhere within its structure. The above example can be rewritten as:

```lisp
(as-> 42 var
  (list (list var))
  (print var))
```

Which is equivalent to:

```lisp
(let* ((var 42)
       (var (list (list var)))
       (var (print var)))
  var)
```

The same variable is rebound on each binding, allowing subsequent bindings to further use its modified value.

## Short-circuiting threading macros

Sometimes we want to abort early in a chain of computation; for example, we can imagine a situation where one computation step returns `nil`, in which case we do not want to execute the computation steps that follow.

The short-circuiting threading macros implement this paradigm (similar to the `Maybe` monad known in the functional programming world). If one of the values bound by the threading macro evaluates to `nil`, the subsequent steps are not evaluated, and `nil` is returned instead.

For example, the following form:

```lisp
(some-> foo
  bar
  (baz)
  (quux 1 2 3))
```

Is equivalent to:

```lisp
(let ((temp1 foo)
      (temp2 (if temp1 (bar temp1) nil))
      (temp3 (if temp2 (baz temp2) nil))
      (temp4 (if temp3 (quux temp3 1 2 3) nil)))
  temp4)
```

We can see that the `if` calls ensure that the previously bound value is non-`nil` before the next one is computed; otherwise, the currently bound variable becomes null, and all subsequent `if` calls return `nil` as well.

## Conditional threading macros

Another possible use case for threading macros is where we want to conditionally apply some transformations to the threaded value. The conditional threading macros allow an easy use of this paradigm.

Each binding in a conditional threaded macro is composed of a test (the `car` of the binding) and forms to be threaded if the test succeeds (the `cdr` of the binding). Let's analyze the following form:

```lisp
(cond-> foo
  (barp x y z)
  (bazp (baz))
  ((quuxp thing) (quux 1 2 3)))
```

The first form is treated as-is. The second and subsequent forms are composed of a test (`barp`, `bazp`, `(quuxp thing)`) and forms to thread through if the respective test evaluates to true.

This means that the above call is equivalent to:

```lisp
(let* ((temp1 foo)
       (temp2 (if barp
                  (-> temp1 x y z)
                  temp1))
       (temp3 (if bazp
                  (-> temp2 (baz))
                  temp2))
       (temp4 (if (quuxp thing)
                  (-> temp3 (quux 1 2 3))
                  temp3)))
  temp4)
```

We can see that, unlike in short-circuiting threading macros, all of the tests are evaluated in order. If the test succeeds, then the new value of the binding is generated using the inner threading macro; otherwise, the old value is reused.

## Nesting threading macros

Sometimes it is useful to nest threading macros. If we want to avoid using diamond macros in a situation where we want to splice our variables as first and last arguments, we can use a combination of nested `->` and `->>`.

The following example shows an example where we want to thread first arguments with the exception of a single form, in which we want to thread the last:

```lisp
(-> deeply-nested-plist
  (getf :foo)
  (getf :bar)
  (->> (mapcar #'reverse)))
```

Is equivalent to:

```lisp
(let* ((temp1 deeply-nested-plist)
       (temp2 (getf temp1 :foo))
       (temp3 (getf temp2 :bar))
       (temp4 (->> temp3 (mapcar #'reverse))))
  temp4)
```

And therefore to:

```lisp
(let* ((temp1 deeply-nested-plist)
       (temp2 (getf temp1 :foo))
       (temp3 (getf temp2 :bar))
       (temp4 (mapcar #'reverse temp3)))
  temp4)
```

An equivalent call of a diamond threading macro would look like the following:

```lisp
(-<> deeply-nested-plist
  (getf :foo)
  (getf :bar)
  (mapcar #'reverse <>))
```

The inverse of the aforementioned order is possible by the use of the inverted threading macro `->*`, which allows to thread last arguments with an exceptional case of binding the first argument.

The following example:

```lisp
(->> deeply-nested-alist
  (assoc :foo)
  cdr
  (assoc :bar)
  cdr
  (->* (mod 3))
  (expt 2))
```

Is equivalent to:

```lisp
(let* ((temp1 deeply-nested-alist)
       (temp2 (assoc :foo temp1))
       (temp3 (cdr temp2))
       (temp4 (assoc :bar temp3))
       (temp5 (cdr temp4))
       (temp6 (->* (mod 3) temp5))
       (temp7 (expt 2 temp6)))
  temp7)
```

And therefore to:

```lisp
(let* ((temp1 deeply-nested-alist)
       (temp2 (assoc :foo temp1))
       (temp3 (cdr temp2))
       (temp4 (assoc :bar temp3))
       (temp5 (cdr temp4))
       (temp6 (mod temp5 3))
       (temp7 (expt 2 temp6)))
  temp7)
```

An equivalent call of a diamond threading macro would look like the following:

```lisp
(-<>> deeply-nested-alist
      (assoc :foo)
      cdr
      (assoc :bar)
      cdr
      (mod <> 3)
      (expt 2))
```

## Named threading macros

Named threading macros permit the use of named variables and therefore allow for inserting blocks of non-threaded Lisp code into otherwise threaded sequences. This allows for easy inspection and overriding of values, especially in complex threading macro calls.

The threading macros that are useful for this are `as->` (for thread-first macros) and `as->*` (for thread-last macros). An example of the former is:

```lisp
(-> 3
  (as-> var
    (< x var y))
  not)
```

Which is equivalent to:

```lisp
(let* ((temp1 3)
       (temp2 (as-> temp1 var
                (< x var y)))
       (temp3 (not temp2)))
  temp3)
```

And therefore to:

```lisp
(let* ((temp1 3)
       (var temp1)
       (var (< x var y))
       (temp2 var)
       (temp3 (not temp2)))
  temp3)
```

A similar example for `as->*` may look like the following:

```lisp
(some->> 15
  (as->* var
    (progn (format t ";; Debug: ~S~%" var)
           var))
  (/ 75))
```

Which is equivalent to:

```lisp
(let* ((temp1 15)
       (temp2 (if temp1
                  (as->* var
                    (progn (format t ";; debug: ~s~%" var) var)
                    temp1)
                  nil))
       (temp3 (if temp2
                  (/ 75 temp2)
                  nil)))
  temp3)
```

And therefore to:

```lisp
(let* ((temp1 15)
       (var temp1))
  (format t ";; debug: ~s~%" var)
  (let* ((temp2 var)
         (temp3 (if temp2
                    (/ 75 temp2)
                    nil)))
    temp3))
```

As useful as named threading macros are, they may contribute negatively to code readability. If that becomes the case, it might be better to use explicit `let` bindings.

## Threading macros as places

It is possible to use a threading macro as a place in a `setf` call. This trait is specific to Common Lisp (since Clojure does not have mutable places) and permits for concise syntax for nesting accessors.

For example:

```lisp
(setf (-> 0 (nthcdr list) cdr cdr car) 42)
```

The above is equivalent to the following:

```lisp
(let* ((temp1 0)
       (temp2 (nthcdr temp1 list))
       (temp3 (cdr temp2))
       (temp4 (cdr temp3))
       (new-value 42))
  (rplaca temp4 new-value))
```

And therefore to the following:

```lisp
(rplaca (cdr (cdr (nthcdr 0 list))) 42)
```

## Short-circuiting threading macros as places

The `some` family of macros interacts differently with `setf` as a part of its short-circuiting. If the short-circuit is engaged anywhere in the thread, then the setting is not executed. For example, the following code will only set the `car` of `cons` if it is not null:

```lisp
(setf (some-> cons
        car)
      42)
```

Regardless of whether the setting occurs, the `setf` form still returns the new value in order to preserve language semantics.

## Conditional threading macros as places

The `cond` family of macros interacts differently with `setf` as a part of its conditionalization. `setf` of a `cond` threading macro generates a complex `setf` expansion that takes all the multiple possibilities of setf places that are present in the thread; the proper branch to take is selected at runtime.

For instance, this call is capable of setting four different places in total depending on the values of boolean parameters `x` and `y`:

```lisp
(setf (cond-> cons
        (x car)
        (y cdr))
      42)
```

If a `cond` macro is used in `setf`, the macroexpansion attempts to detect which of the threaded forms denote valid places and therefore generates setters only for them. The heuristic is imperfect and filters out literals and symbols from the CL package that cannot be used as places.
