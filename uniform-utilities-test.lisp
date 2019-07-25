
;; Local Variables:
;; eval: (put 'defdpar 'lisp-indent-function 0)
;; End:

(defpackage :uniform-utilities-test
  (:use :cl :prove :uniform-utilities))
(in-package :uniform-utilities-test)

(plan nil)
(setq *default-reporter* :tap)

;; Owing to the use of let forms, try running tests after package reloading,
;; in case of failures.

(defclass foo () ((a :initform 3)))
(defstruct bar a)

(defmacro getf-env (&body body)
  `(progn
     (defparameter str "abcde")
     (defparameter vec #(a b c d e))
     (defparameter lst '(a b c d e))
     (defparameter dict (alexandria:alist-hash-table '((a . b) (c . d))))
     (defparameter arr #2A ((1 2) (3 4)))
     (defparameter clos-object (make-instance 'foo))
     (defparameter struct (make-bar :a 3))
     ,@body))

(deftest setf-getv-without-types
  (getf-env
    (is (progn (setf (getv str 0) #\f) (getv str 0)) #\f)
    (is (progn (setf (getv vec 0) 'f) (getv vec 0)) 'f)
    (is (progn (setf (getv lst 0) 'f) (getv lst 0)) 'f)
    (is (progn (setf (getv dict 'a) 'f) (getv dict 'a)) 'f)
    (is (progn (setf (getv arr (list 0 1)) '(2))
               (getv arr '(0 1)))
        '(2))
    (is (progn (setf (getv clos-object 'a) 5)
               (getv clos-object 'a))
        5)
    (is (progn (setf (getv struct 'a) 7)
               (getv struct 'a))
        7)))

(deftest setf-getv-with-types
  (getf-env
    (is-error (setf (getv str 0 'list) #\f)
              'type-error)
    (is-error (setf (getv vec 0 'hash-table) #\f)
              'type-error)
    (is-error (setf (getv lst 0 'string) #\f)
              'type-error)
    (is-error (setf (getv arr (list 0 1) 'list) 5)
              'type-error)
    (is-error (setf (getv dict 'c 'string) 'f)
              'type-error)
    (setf (getv str 0 'string) #\f)
    (is (getv str 0 'string) #\f)
    (is (progn (setf (getv vec 0 'vector) 'f) (getv vec 0 'vector)) 'f)
    (is (progn (setf (getv lst 0 'list) 'f) (getv lst 0 'list)) 'f)
    (is (progn (setf (getv dict 'a 'hash-table) 'f) (getv dict 'a 'hash-table)) 'f)
    (is (progn (setf (getv arr (list 0 1) 'array) '(2))
               (getv arr '(0 1) 'array))
        '(2))
    (setf (getv clos-object 'a 'standard-object) 5)
    (is (getv clos-object 'a 'standard-object)  5)
    (is (progn (setf (getv struct 'a 'structure-object) 7)
               (getv struct 'a 'structure-object))
        7)))


(defmacro env-getf-chained (&body body)
  `(let ((list '(1 2 #(a b #2A((q w e)(r t y)))))
         (list-str '("hello" "world"))
         (list-ht (list (alexandria:alist-hash-table '(("uniform" . "utilities"))
                                                     :test 'equal))))
     ,@body))

(deftest getv-chained
  (env-getf-chained
    (is (getv-chained list
                      2 2 '(1 1))
        't)
    (is (getv-chained list-str 1 3) #\l)
    (is (getv-chained list-ht 0 "uniform") "utilities" :test #'string=)))
(deftest setf-getv-chained
  (env-getf-chained
    (is (progn (setf (getv-chained list
                                   2 2 '(1 1))
                     list-ht)
               (getv-chained list
                             2 2 '(1 1) 0 "uniform"))
        "utilities"
        :test #'string=)))

(defmacro env-defdpar (&body body)
  `(flet ((foo () (values '(1 2) 6)))
     ,@body))

(deftest defdpar-without-errors
  (env-defdpar
    (defdpar b c (foo))
    (defdpar (d e) (foo))
    (defdpar f (car (foo))))
  (is b '(1 2) :test #'equalp)
  (is c 6)
  (is d 1)
  (is e 2)
  (is f 1))

(deftest defpar-errors
  (env-defdpar
    (is-error (defdpar (a) (foo)) 'simple-error)
    (is-error (defdpar (a b c) (foo)) 'simple-error)))

(deftest dlet*-without-errors
  (is (dlet* (((a c) b (values '(1 3) 2))
              (d e f (values 1 2))
              ((a b) '(4 5))) 
        (list a b c d e f))
      '(4 5 3 1 2 NIL)
      :test #'equalp))

(deftest dlet*-with-errors
  (is-error (dlet* (((a b) 3)) t) 'simple-error)
  (is-error (dlet* (((a b c) '(1 2))) t) 'simple-error))
