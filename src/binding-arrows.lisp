(defpackage #:binding-arrows
  (:use #:cl)
  (:export #:-> #:->> #:-<> #:-<>> #:->*
           #:some-> #:some->> #:some-<> #:some-<>>
           #:cond-> #:cond->> #:cond-<> #:cond-<>>
           #:as-> #:as->*))

(in-package #:binding-arrows)

(defvar *valid-cl-place-forms* ;; List from CLHS 5.1.2.2
  '(aref    cdadr                    get
    bit     cdar                     gethash
    caaaar  cddaar                   logical-pathname-translations
    caaadr  cddadr                   macro-function
    caaar   cddar                    ninth
    caadar  cdddar                   nth
    caaddr  cddddr                   readtable-case
    caadr   cdddr                    rest
    caar    cddr                     row-major-aref
    cadaar  cdr                      sbit
    cadadr  char                     schar
    cadar   class-name               second
    caddar  compiler-macro-function  seventh
    cadddr  documentation            sixth
    caddr   eighth                   slot-value
    cadr    elt                      subseq
    car     fdefinition              svref
    cdaaar  fifth                    symbol-function
    cdaadr  fill-pointer             symbol-plist
    cdaar   find-class               symbol-value
    cdadar  first                    tenth
    cdaddr  fourth                   third))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main arrow expansion function

(defun ensure-cons (thing)
  (if (consp thing) thing (list thing)))

(defun make-value-form (value-fn prev-symbol form)
  (if (null prev-symbol)
      form
      (funcall value-fn prev-symbol (ensure-cons form))))

(defun expand-aux (forms symbol-fn value-fn return-fn &optional env)
  (loop with symbol-fn = (or symbol-fn (lambda () (gensym "VAR")))
        with value-fn = (or value-fn #'value-first)
        with length = (length forms)
        with symbols = (loop repeat length collect (funcall symbol-fn))
        for form in forms
        for prev-symbol = nil then next-symbol
        for next-symbol in symbols
        for value-form = (make-value-form value-fn prev-symbol form)
        collect value-form into value-forms
        finally (return (funcall return-fn symbols value-forms env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Arrow expander

(defun expand-arrow-return (symbols value-forms env)
  (declare (ignore env))
  `(let* ,(mapcar #'list symbols value-forms)
     ,(car (last symbols))))

(defun expand-arrow (forms &key symbol-fn value-fn return-fn)
  (declare (ignore return-fn))
  (case (length forms)
    (0 'nil)
    (1 (first forms))
    (t (expand-aux forms symbol-fn value-fn #'expand-arrow-return))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Arrow SETF expander

(defun expand-arrow-setf-return (symbols value-forms env)
  (multiple-value-bind (vars vals stores store-fn access-fn)
      (get-setf-expansion (car (last value-forms)) env)
    (values (append (butlast symbols) vars)
            (append (butlast value-forms) vals)
            stores
            store-fn
            access-fn)))

(defun expand-arrow-setf-some-return (symbols value-forms env)
  (multiple-value-bind (vars vals stores store-fn access-fn)
      (get-setf-expansion (third (car (last value-forms))) env)
    (values (append (butlast symbols) vars)
            (append (butlast value-forms) vals)
            stores
            `(multiple-value-prog1 (values ,@stores)
               (when ,(car (last (butlast symbols))) ,store-fn))
            `(if ,(car (last (butlast symbols))) ,access-fn nil))))

(defun cond-setf-expansions (value-forms env)
  (loop for value-form in value-forms
        for place = (if (eq value-form (first value-forms))
                        value-form
                        (third value-form))
        collect (multiple-value-list (get-setf-expansion place env))))

(defun invalid-place-p (form first-value-p)
  (flet ((invalid-cons-p (form)
           (let ((thing (first (if first-value-p
                                   form
                                   (ensure-cons (third (third form)))))))
             (and (symbolp thing)
                  (eq (symbol-package thing) (find-package :cl))
                  (not (member thing *valid-cl-place-forms*))))))
    (typecase form
      (cons (invalid-cons-p form))
      (symbol nil)
      (t t))))

(defun cond-generate-store-fn (store symbols value-forms expansions)
  (let ((conds (loop for value-form in value-forms
                     for first-value-p = (eq value-form (first value-forms))
                     for test = (if first-value-p
                                    value-form
                                    (second value-form))
                     for (vars vals stores store-fn access-fn) in expansions
                     for bindings = (append (mapcar #'list vars vals)
                                            `((,(first stores) ,store)))
                     unless (invalid-place-p value-form first-value-p)
                       collect `(,test (let* ,bindings ,store-fn)) into result
                     finally (return (nreverse result)))))
    `(multiple-value-prog1 ,store
       ,@(butlast symbols)
       (cond ,@conds))))

(defun cond-generate-access-fn (symbols expansions)
  (let ((conds (loop for symbol in symbols
                     for (vars vals stores store-fn access-fn) in expansions
                     for bindings = (mapcar #'list vars vals)
                     collect `(,symbol (let* ,bindings ,access-fn)) into result
                     finally (return (nreverse result)))))
    `(cond ,@conds)))

(defun expand-arrow-setf-cond-return (symbols value-forms env)
  (let ((expansions (cond-setf-expansions value-forms env))
        (store (gensym "NEW")))
    (values (butlast symbols)
            (butlast value-forms)
            (list store)
            (cond-generate-store-fn store symbols value-forms expansions)
            (cond-generate-access-fn symbols expansions))))

(defun expand-arrow-setf (forms env &key symbol-fn value-fn
                                      (return-fn #'expand-arrow-setf-return))
  (case (length forms)
    (0 (error "Cannot get the SETF expansion of an empty threading macro."))
    (1 (get-setf-expansion (first forms)))
    (t (expand-aux forms symbol-fn value-fn return-fn env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macrology

(defmacro define-arrow (name lambda-list &body body)
  (let ((env (gensym "ENV"))
        (args (gensym "ARGS"))
        (flet-forms (gensym "FORMS")))
    (flet ((generate-macro (macro-name expand-fn &optional env)
             `(,macro-name
               ,name (,@lambda-list ,@(when env `(&environment ,env)))
               (flet ((expand (,flet-forms &rest ,args)
                        (apply #',expand-fn ,flet-forms ,@(when env `(,env))
                               ,args)))
                 ,@body))))
      `(progn ,(generate-macro 'defmacro 'expand-arrow)
              ,(generate-macro 'define-setf-expander 'expand-arrow-setf env)
              ',name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Value functions

(defun value-first (symbol form)
  (destructuring-bind (head . tail) form
    (list* head symbol tail)))

(defun value-last (symbol form)
  (append form (list symbol)))

(defun as-value (symbol form)
  (declare (ignore symbol))
  form)

(defun some-value (value-fn symbol form)
  `(if ,symbol ,(funcall value-fn symbol form) nil))

(defun some-value-first (symbol form)
  (some-value #'value-first symbol form))

(defun some-value-last (symbol form)
  (some-value #'value-last symbol form))

(defun cond-value (arrow symbol form)
  (destructuring-bind (test . forms) form
    `(if ,test (,arrow ,symbol ,@forms) ,symbol)))

(defun cond-value-first (symbol form)
  (cond-value '-> symbol form))

(defun cond-value-last (symbol form)
  (cond-value '->> symbol form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Diamond value functions

(defun diamond-value (value-fn symbol form)
  (flet ((diamondp (form) (and (symbolp form) (string= form "<>"))))
    (if (= 0 (count-if #'diamondp form))
        (funcall value-fn symbol form)
        (substitute-if symbol #'diamondp form))))

(defun diamond-value-first (symbol form)
  (diamond-value #'value-first symbol form))

(defun diamond-value-last (symbol form)
  (diamond-value #'value-last symbol form))

(defun some-diamond-value-first (symbol form)
  (some-value #'diamond-value-first symbol form))

(defun some-diamond-value-last (symbol form)
  (some-value #'diamond-value-last symbol form))

(defun cond-diamond-value-first (symbol form)
  (cond-value '-<> symbol form))

(defun cond-diamond-value-last (symbol form)
  (cond-value '-<>> symbol form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Arrow implementations

(define-arrow -> (&body forms)
  (expand forms))

(define-arrow ->> (&body forms)
  (expand forms :value-fn #'value-last))

(define-arrow -<> (&body forms)
  (expand forms :value-fn #'diamond-value-first))

(define-arrow -<>> (&body forms)
  (expand forms :value-fn #'diamond-value-last))

(define-arrow some-> (&body forms)
  (expand forms :value-fn #'some-value-first
                :return-fn #'expand-arrow-setf-some-return))

(define-arrow some->> (&body forms)
  (expand forms :value-fn #'some-value-last
                :return-fn #'expand-arrow-setf-some-return))

(define-arrow some-<> (&body forms)
  (expand forms :value-fn #'some-diamond-value-first
                :return-fn #'expand-arrow-setf-some-return))

(define-arrow some-<>> (&body forms)
  (expand forms :value-fn #'some-diamond-value-last
                :return-fn #'expand-arrow-setf-some-return))

(define-arrow cond-> (&body forms)
  (expand forms :value-fn #'cond-value-first
                :return-fn #'expand-arrow-setf-cond-return))

(define-arrow cond->> (&body forms)
  (expand forms :value-fn #'cond-value-last
                :return-fn #'expand-arrow-setf-cond-return))

(define-arrow cond-<> (&body forms)
  (expand forms :value-fn #'cond-diamond-value-first
                :return-fn #'expand-arrow-setf-cond-return))

(define-arrow cond-<>> (&body forms)
  (expand forms :value-fn #'cond-diamond-value-last
                :return-fn #'expand-arrow-setf-cond-return))

(define-arrow ->* (&body forms)
  (let ((forms (append (last forms) (butlast forms))))
    (expand forms)))

(define-arrow as-> (initial-form var &body forms)
  (let ((forms (cons initial-form forms)))
    (expand forms :symbol-fn (constantly var) :value-fn #'as-value)))

(define-arrow as->* (var &body forms)
  (let ((forms (append (last forms) (butlast forms))))
    (expand forms :symbol-fn (constantly var) :value-fn #'as-value)))
