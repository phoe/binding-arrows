# Arrows - Examples

See [the test suite](../t/test.lisp) for even more examples.

## Arrow macros

```lisp
BINDING-ARROWS> (-> 3
                  /)

;; (let ((x 3)) (/ x))

1/3
```

```lisp
BINDING-ARROWS> (-> 3
                  (expt 2))

;; (let ((x 3)) (expt x 2))

9
```

```lisp
BINDING-ARROWS> (->> 3
                  (expt 2))

;; (let ((x 3)) (expt 2 x))

8
```

## Diamond arrow macros

```lisp
BINDING-ARROWS> (-<> 3
                  (/ 2))

;; (let ((x 3)) (/ 3 2))

3/2
```

```lisp
BINDING-ARROWS> (-<> 3
                  (/ <> 2))

;; (let ((x 3)) (/ 3 2))

3/2
```

```lisp
BINDING-ARROWS> (-<> 3
                  (/ 2 <>))

;; (let ((x 3)) (/ 2 3))

2/3
```

```lisp
BINDING-ARROWS> (-<>> (list 1 2 3)
                  (remove-if #'oddp <> :count 1 :from-end t) ; substitute <>
                  (reduce #'+)                               ; insert last
                  /)                                         ; list designator

;; (let ((x (list 1 2 3)
;;       (y (remove-if #'oddp x :count 1 :from-end t))
;;       (z (reduce #'+ y)))
;;  (/ z))

1/3
```

```lisp
BINDING-ARROWS> (let ((x 3))
                  (-<> (incf x)  ; (let ((r (incf x)))
                    (+ <> <>)))  ;   (+ r r))

;; (let* ((x 3))
;;        (y (incf x))
;;   (+ y y))

8
```

## Conditional threading macros

```lisp
BINDING-ARROWS> (let ((cons (cons (cons 1 2) 3)))
                  (setf (cond-> cons
                          (t car)
                          (t cdr))
                        42)
                  cons)

((1 . 42) . 3)
```

```lisp
BINDING-ARROWS> (let ((cons (cons (cons 1 2) 3)))
                  (setf (cond-> cons
                          (nil car)
                          (t cdr))
                        42)
                  cons)

((1 . 2) . 42)
```

```lisp
BINDING-ARROWS> (let ((cons (cons (cons 1 2) 3)))
                  (setf (cond-> cons
                          (t car)
                          (nil cdr))
                        42)
                  cons)

(42 . 3)
```

```lisp
BINDING-ARROWS> (let ((cons (cons (cons 1 2) 3)))
                  (setf (cond-> cons
                          (nil car)
                          (nil cdr))
                        42)
                  cons)

42
```
