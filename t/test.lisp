(defpackage #:binding-arrows/test
  (:use #:cl #:binding-arrows #:hu.dwim.stefil))

(in-package #:binding-arrows/test)

(defsuite* test-binding-arrows)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Correctness tests

(deftest test--> ()
  (is (= (-> 3 /) 1/3))
  (is (= (-> 3 (/)) 1/3))
  (is (= (-> 3 (/ 2)) 3/2))
  (is (= (-> 3 (/ 2) /) 2/3)))

(deftest test-->> ()
  (is (= (->> 3 /) 1/3))
  (is (= (->> 3 (/)) 1/3))
  (is (= (->> 3 (/ 2)) 2/3))
  (is (= (->> 3 (/ 2) /) 3/2)))

(deftest test--<> ()
  (is (= (-<> 3 /) 1/3))
  (is (= (-<> 3 (/)) 1/3))
  (is (= (-<> 3 (/ 2)) 3/2))
  (is (= (-<> 3 (/ 2) /) 2/3))
  (is (= (let ((x 3))
           (-<> (incf x)
                (+ <> <>)))
         8)))

(deftest test--<>> ()
  (is (= (-<>> 3 /) 1/3))
  (is (= (-<>> 3 (/)) 1/3))
  (is (= (-<>> 3 (/ 2)) 2/3))
  (is (= (-<>> 3 (/ 2) /) 3/2))
  (is (= (-<>> (list 1 2 3)
               (remove-if #'oddp <> :count 1 :from-end t)
               (reduce #'+)
               /)
         1/3)))

(deftest test-some-> ()
  (is (null (some-> 3
                    (+ 5)
                    (member '(2 5 9))
                    first
                    (* 9))))
  (is (= (some-> 3
                 (+ 5)
                 (member '(2 5 8 9))
                 first
                 (* 9))
         72))
  (is (= (some-> 3
                 (+ 5)
                 (member '(2 5 8 9))
                 second
                 (* 9))
         81))
  (is (null (some-> 3
                    (+ 5)
                    (member '(2 5 8 9))
                    third
                    (* 9))))
  (is (null (some-> '(:a 1)
                    (getf :b)
                    1+))))

(deftest test-some->> ()
  (is (= (some->> '((:a . 3) (:b . 5))
                  (assoc :a)
                  cdr
                  1+)
         4))
  (is (null (some->> '((:a . 3) (:b . 5))
                     (assoc :c)
                     cdr
                     1+))))

(deftest test-some-<> ()
  (is (equal (some-<> 3
                      (list 1 2 <> 4 5)
                      (list :foo))
             '((1 2 3 4 5) :foo)))
  (is (null (some-<> 3
                     evenp
                     (list 1 2 <> 4 5)))))

(deftest test-some-<>> ()
  (is (equal (some-<>> 3
                       (list 1 2 <> 4 5)
                       (list :foo))
             '(:foo (1 2 3 4 5))))
  (is (null (some-<>> 3
                      evenp
                      (list 1 2 <> 4 5)))))

(deftest test-cond-> ()
  (is (equal (labels ((strcat (&rest things)
                        (apply #'concatenate 'string things))
                      (say (n)
                        (cond-> '()
                                ((zerop (mod n 3)) (strcat "Fizz"))
                                ((zerop (mod n 5)) (strcat "Buzz"))
                                (t (or (princ-to-string n))))))
               (mapcar #'say '(9 10 11 12 13 14 15)))
             '("Fizz" "Buzz" "11" "Fizz" "13" "14" "FizzBuzz"))))

(deftest test-cond->> ()
  (is (equal (flet ((say (n)
                      (cond->> '()
                               ((zerop (mod n 3)) (cons "Fizz"))
                               ((zerop (mod n 5)) (cons "Buzz"))
                               (t (->* (or (list (princ-to-string n))))
                                  reverse
                                  (apply #'concatenate 'string)))))
               (mapcar #'say '(9 10 11 12 13 14 15)))
             '("Fizz" "Buzz" "11" "Fizz" "13" "14" "FizzBuzz"))))

(deftest test-cond-<> ()
  (is (equal (let ((n 3))
               (cond-<> n
                        ((oddp n) (list 1 2 <> 4 5))
                        (t (list :foo))))
             '((1 2 3 4 5) :foo)))
  (is (equal (let ((n 3))
               (cond-<> 3
                        ((evenp n) (list 1 2 <> 4 5))
                        (t (list :foo))))
             '(3 :foo))))

(deftest test-cond-<>> ()
  (is (equal (let ((n 3))
               (cond-<>> n
                         ((oddp n) (list 1 2 <> 4 5))
                         (t (list :foo))))
             '(:foo (1 2 3 4 5))))
  (is (equal (let ((n 3))
               (cond-<>> 3
                         ((evenp n) (list 1 2 <> 4 5))
                         (t (list :foo))))
             '(:foo 3))))

(deftest test-->* ()
  (is (= (->* / 3) 1/3))
  (is (= (->* (/) 3) 1/3))
  (is (= (->* (/ 2) 3) 3/2))
  (is (= (->* (/ 2) / 3) 2/3)))

(deftest test-as-> ()
  (is (= (as-> 3 $
               (* 5 $)
               (/ $ 7))
         15/7))
  (is (= (as-> 0 n
               (1+ n)
               (1+ n))
         2)))

(deftest test-as->* ()
  (is (= (as->* $
                (* 5 $)
                (/ $ 7)
                3)
         15/7))
  (is (= (as->* n
                (1+ n)
                (1+ n)
                0)
         2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macroexpansion tests - utilities

(defun equal* (x y)
  (cond ((and (symbolp x) (null (symbol-package x))
              (symbolp y) (null (symbol-package y)))
         (string= x y))
        ((and (consp x) (consp y))
         (and (equal* (car x) (car y))
              (equal* (cdr x) (cdr y))))
        (t (equal x y))))

(defun test-expansion (form expected)
  (let* ((*gensym-counter* 0)
         (actual (macroexpand-1 form)))
    (is (equal* actual expected))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macroexpansion tests

(deftest expand--> ()
  (test-expansion
   '(-> foo bar (baz) (quux 1 2 3) (fred 4 5 6))
   '(let* ((#:var0 foo)
           (#:var1 (bar #:var0))
           (#:var2 (baz #:var1))
           (#:var3 (quux #:var2 1 2 3))
           (#:var4 (fred #:var3 4 5 6)))
     #:var4)))

(deftest expand-->> ()
  (test-expansion
   '(->> foo bar (baz) (quux 1 2 3) (fred 4 5 6))
   '(let* ((#:var0 foo)
           (#:var1 (bar #:var0))
           (#:var2 (baz #:var1))
           (#:var3 (quux 1 2 3 #:var2))
           (#:var4 (fred 4 5 6 #:var3)))
     #:var4)))

(deftest expand--<> ()
  (test-expansion
   '(-<> foo bar (baz) (quux 1 2 3) (fred 4 <> 5 6 <>))
   '(let* ((#:var0 foo)
           (#:var1 (bar #:var0))
           (#:var2 (baz #:var1))
           (#:var3 (quux #:var2 1 2 3))
           (#:var4 (fred 4 #:var3 5 6 #:var3)))
     #:var4)))

(deftest expand--<>> ()
  (test-expansion
   '(-<>> foo bar (baz) (quux 1 2 3) (fred 4 <> 5 6 <>))
   '(let* ((#:var0 foo)
           (#:var1 (bar #:var0))
           (#:var2 (baz #:var1))
           (#:var3 (quux 1 2 3 #:var2))
           (#:var4 (fred 4 #:var3 5 6 #:var3)))
     #:var4)))

(deftest expand-some-> ()
  (test-expansion
   '(some-> foo bar (baz) (quux 1 2 3) (fred 4 5 6))
   '(let* ((#:var0 foo)
           (#:var1 (if #:var0 (bar #:var0) nil))
           (#:var2 (if #:var1 (baz #:var1) nil))
           (#:var3 (if #:var2 (quux #:var2 1 2 3) nil))
           (#:var4 (if #:var3 (fred #:var3 4 5 6) nil)))
     #:var4)))

(deftest expand-some->> ()
  (test-expansion
   '(some->> foo bar (baz) (quux 1 2 3) (fred 4 5 6))
   '(let* ((#:var0 foo)
           (#:var1 (if #:var0 (bar #:var0) nil))
           (#:var2 (if #:var1 (baz #:var1) nil))
           (#:var3 (if #:var2 (quux 1 2 3 #:var2) nil))
           (#:var4 (if #:var3 (fred 4 5 6 #:var3) nil)))
     #:var4)))

(deftest expand-some-<> ()
  (test-expansion
   '(some-<> foo bar (baz :baz) (quux 1 <> 2 3))
   '(let* ((#:var0 foo)
           (#:var1 (if #:var0 (bar #:var0) nil))
           (#:var2 (if #:var1 (baz #:var1 :baz) nil))
           (#:var3 (if #:var2 (quux 1 #:var2 2 3) nil)))
     #:var3)))

(deftest expand-some-<>> ()
  (test-expansion
   '(some-<>> foo bar (baz :baz) (quux 1 <> 2 3))
   '(let* ((#:var0 foo)
           (#:var1 (if #:var0 (bar #:var0) nil))
           (#:var2 (if #:var1 (baz :baz #:var1) nil))
           (#:var3 (if #:var2 (quux 1 #:var2 2 3) nil)))
     #:var3)))

(deftest expand-cond-> ()
  (test-expansion
   '(cond-> foo (barp x y z) (bazp (baz)) ((quuxp thing) (quux 1 2 3)))
   '(let* ((#:var0 foo)
           (#:var1 (if barp (-> #:var0 x y z) #:var0))
           (#:var2 (if bazp (-> #:var1 (baz)) #:var1))
           (#:var3 (if (quuxp thing) (-> #:var2 (quux 1 2 3)) #:var2)))
     #:var3)))

(deftest expand-cond->> ()
  (test-expansion
   '(cond->> foo (bar-p x y z) (baz-p (baz)) ((quux-p thing) (quux 1 2 3)))
   '(let* ((#:var0 foo)
           (#:var1 (if bar-p (->> #:var0 x y z) #:var0))
           (#:var2 (if baz-p (->> #:var1 (baz)) #:var1))
           (#:var3 (if (quux-p thing) (->> #:var2 (quux 1 2 3)) #:var2)))
     #:var3)))

(deftest expand-cond-<> ()
  (test-expansion
   '(cond-<> foo (barp x y z) (bazp (baz)) ((quuxp thing) (quux 1 <> 2 3)))
   '(let* ((#:var0 foo)
           (#:var1 (if barp (-<> #:var0 x y z) #:var0))
           (#:var2 (if bazp (-<> #:var1 (baz)) #:var1))
           (#:var3 (if (quuxp thing) (-<> #:var2 (quux 1 <> 2 3)) #:var2)))
     #:var3)))

(deftest expand-cond-<>> ()
  (test-expansion
   '(cond-<>> foo (barp x y z) (bazp (baz)) ((quuxp thing) (quux 1 <> 2 3)))
   '(let* ((#:var0 foo)
           (#:var1 (if barp (-<>> #:var0 x y z) #:var0))
           (#:var2 (if bazp (-<>> #:var1 (baz)) #:var1))
           (#:var3 (if (quuxp thing) (-<>> #:var2 (quux 1 <> 2 3)) #:var2)))
     #:var3)))

(deftest expand-->* ()
  (test-expansion
   '(->* bar (baz) (quux 1 2 3) (fred 4 5 6) foo)
   '(let* ((#:var0 foo)
           (#:var1 (bar #:var0))
           (#:var2 (baz #:var1))
           (#:var3 (quux #:var2 1 2 3))
           (#:var4 (fred #:var3 4 5 6)))
     #:var4)))

(deftest expand-as-> ()
  (test-expansion
   '(as-> foo #:var bar (baz) (quux 1 2 3) (fred 4 5 6))
   '(let* ((#:var foo)
           (#:var (bar))
           (#:var (baz))
           (#:var (quux 1 2 3))
           (#:var (fred 4 5 6)))
     #:var)))

(deftest expand-as->* ()
  (test-expansion
   '(as->* #:var bar (baz) (quux 1 2 3) (fred 4 5 6) foo)
   '(let* ((#:var foo)
           (#:var (bar))
           (#:var (baz))
           (#:var (quux 1 2 3))
           (#:var (fred 4 5 6)))
     #:var)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SETF tests

(deftest setf--> ()
  (let ((list (list 1 2 3 4 5)))
    (setf (-> 0 (nthcdr list) cdr cdr car) 42)
    (is (equal list '(1 2 42 4 5)))))

(deftest setf-->> ()
  (let ((list (list 1 2 3 4 5)))
    (setf (->> list (nthcdr 0) cdr cdr car) 42)
    (is (equal list '(1 2 42 4 5)))))

(deftest setf--<> ()
  (let ((list (list 1 2 3 4 5)))
    (setf (-<> 0 (nthcdr <> list) cdr cdr car) 42)
    (is (equal list '(1 2 42 4 5)))))

(deftest setf--<>> ()
  (let ((list (list 1 2 3 4 5)))
    (setf (-<>> 0 (nthcdr <> list) cdr cdr car) 42)
    (is (equal list '(1 2 42 4 5)))))

(deftest setf-some-> ()
  (let ((list (list 1 2 3 4 5)))
    (setf (some-> 0 (nthcdr list) cdr cdr car) 42)
    (is (equal list '(1 2 42 4 5))))
  (let ((list (list 1 2 3 4 5)))
    (setf (some-> 0 (nthcdr list) atom cdr car) 42)
    (is (equal list '(1 2 3 4 5)))))

(deftest setf-some->> ()
  (let ((list (list 1 2 3 4 5)))
    (setf (some->> list (nthcdr 0) cdr cdr car) 42)
    (is (equal list '(1 2 42 4 5))))
  (let ((list (list 1 2 3 4 5)))
    (setf (some->> list (nthcdr 0) atom cdr car) 42)
    (is (equal list '(1 2 3 4 5)))))

(deftest setf-some-<> ()
  (let ((list (list 1 2 3 4 5)))
    (setf (some-<> 0 (nthcdr <> list) cdr cdr car) 42)
    (is (equal list '(1 2 42 4 5))))
  (let ((list (list 1 2 3 4 5)))
    (setf (some-<> 0 (nthcdr <> list) atom cdr car) 42)
    (is (equal list '(1 2 3 4 5)))))

(deftest setf-some-<>> ()
  (let ((list (list 1 2 3 4 5)))
    (setf (some-<>> 0 (nthcdr <> list) cdr cdr car) 42)
    (is (equal list '(1 2 42 4 5))))
  (let ((list (list 1 2 3 4 5)))
    (setf (some-<>> 0 (nthcdr <> list) atom cdr car) 42)
    (is (equal list '(1 2 3 4 5)))))

(deftest setf-cond-> ()
  (let ((list (list 1 2 3 4 5)))
    (setf (cond-> 0 (t (nthcdr list)) (t cdr) (nil car) (t cdr) (t car)) 42)
    (is (equal list '(1 2 42 4 5))))
  (flet ((make-test-data (x y)
           (let ((cons (copy-tree '((1 . 2) . 3))))
             (setf (cond-> cons (x car) (y cdr)) 42)
             cons)))
    (is (equal (make-test-data t t) '((1 . 42) . 3)))
    (is (equal (make-test-data nil t) '((1 . 2) . 42)))
    (is (equal (make-test-data t nil) '(42 . 3)))
    (is (equal (make-test-data nil nil) 42))))

(deftest setf-cond->> ()
  (let ((list (list 1 2 3 4 5)))
    (setf (cond->> list (t (nthcdr 0)) (t cdr) (nil car) (t cdr) (t car)) 42)
    (is (equal list '(1 2 42 4 5))))
  (flet ((make-test-data (x y)
           (let ((cons (copy-tree '((1 . 2) . 3))))
             (setf (cond->> cons (x car) (y cdr)) 42)
             cons)))
    (is (equal (make-test-data t t) '((1 . 42) . 3)))
    (is (equal (make-test-data nil t) '((1 . 2) . 42)))
    (is (equal (make-test-data t nil) '(42 . 3)))
    (is (equal (make-test-data nil nil) 42))))

(deftest setf-cond-<> ()
  (let ((list (list 1 2 3 4 5)))
    (setf (cond-<> list (t (nthcdr 0 <>)) (t cdr) (nil car) (t cdr) (t car)) 42)
    (is (equal list '(1 2 42 4 5))))
  (flet ((make-test-data (x y)
           (let ((cons (copy-tree '((1 . 2) . 3))))
             (setf (cond-<> cons (x (car <>)) (y (cdr <>))) 42)
             cons)))
    (is (equal (make-test-data t t) '((1 . 42) . 3)))
    (is (equal (make-test-data nil t) '((1 . 2) . 42)))
    (is (equal (make-test-data t nil) '(42 . 3)))
    (is (equal (make-test-data nil nil) 42))))

(deftest setf-cond-<>> ()
  (let ((list (list 1 2 3 4 5)))
    (setf (cond-<>> 0 (t (nthcdr <> list)) (t cdr) (nil car) (t car)) 42)
    (is (equal list '(1 42 3 4 5))))
  (flet ((make-test-data (x y)
           (let ((cons (copy-tree '((1 . 2) . 3))))
             (setf (cond-<>> cons (x (car <>)) (y (cdr <>))) 42)
             cons)))
    (is (equal (make-test-data t t) '((1 . 42) . 3)))
    (is (equal (make-test-data nil t) '((1 . 2) . 42)))
    (is (equal (make-test-data t nil) '(42 . 3)))
    (is (equal (make-test-data nil nil) 42))))

(deftest setf-->* ()
  (let ((list (list 1 2 3 4 5)))
    (setf (->* (nthcdr list) cdr cdr car 0) 42)
    (is (equal list  '(1 2 42 4 5)))))

(deftest setf-as-> ()
  (let ((list (list 1 2 3 4 5)))
    (setf (as-> list foo (cdr foo) (cdr foo) (car foo)) 42)
    (is (equal list  '(1 2 42 4 5)))))

(deftest setf-as->* ()
  (let ((list (list 1 2 3 4 5)))
    (setf (as->* foo (cdr foo) (cdr foo) (car foo) list) 42)
    (is (equal list  '(1 2 42 4 5)))))
