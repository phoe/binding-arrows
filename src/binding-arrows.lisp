(defpackage #:binding-arrows
  (:use #:cl)
  (:export #:-> #:->> #:-<> #:-<>> #:->*
           #:some-> #:some->> #:some-<> #:some-<>>
           #:cond-> #:cond->> #:cond-<> #:cond-<>>
           #:as-> #:as->*))

(in-package #:binding-arrows)

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

(defun expand-arrow-setf-if-return (symbols value-forms env)
  (multiple-value-bind (vars vals stores store-fn access-fn)
      (get-setf-expansion (third (car (last value-forms))) env)
    (values (append (butlast symbols) vars)
            (append (butlast value-forms) vals)
            stores
            `(multiple-value-prog1 (values ,@stores)
               (when ,(car (last (butlast symbols))) ,store-fn))
            access-fn)))

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

(define-arrow -> (&rest forms)
  (expand forms))

(define-arrow ->> (&rest forms)
  (expand forms :value-fn #'value-last))

(define-arrow -<> (&rest forms)
  (expand forms :value-fn #'diamond-value-first))

(define-arrow -<>> (&rest forms)
  (expand forms :value-fn #'diamond-value-last))

(define-arrow some-> (&rest forms)
  (expand forms :value-fn #'some-value-first
                :return-fn #'expand-arrow-setf-if-return))

(define-arrow some->> (&rest forms)
  (expand forms :value-fn #'some-value-last
                :return-fn #'expand-arrow-setf-if-return))

(define-arrow some-<> (&rest forms)
  (expand forms :value-fn #'some-diamond-value-first
                :return-fn #'expand-arrow-setf-if-return))

(define-arrow some-<>> (&rest forms)
  (expand forms :value-fn #'some-diamond-value-last
                :return-fn #'expand-arrow-setf-if-return))

(define-arrow cond-> (&rest forms)
  (expand forms :value-fn #'cond-value-first
                :return-fn #'expand-arrow-setf-if-return))

(define-arrow cond->> (&rest forms)
  (expand forms :value-fn #'cond-value-last
                :return-fn #'expand-arrow-setf-if-return))

(define-arrow cond-<> (&rest forms)
  (expand forms :value-fn #'cond-diamond-value-first
                :return-fn #'expand-arrow-setf-if-return))

(define-arrow cond-<>> (&rest forms)
  (expand forms :value-fn #'cond-diamond-value-last
                :return-fn #'expand-arrow-setf-if-return))

(define-arrow ->* (&rest forms)
  (let ((forms (append (last forms) (butlast forms))))
    (expand forms)))

(define-arrow as-> (initial-form var &rest forms)
  (let ((forms (cons initial-form forms)))
    (expand forms :symbol-fn (constantly var) :value-fn #'as-value)))

(define-arrow as->* (var &rest forms)
  (let ((forms (append (last forms) (butlast forms))))
    (expand forms :symbol-fn (constantly var) :value-fn #'as-value)))
