(defpackage :uniform-utilities
  (:use :common-lisp :metabang-bind :iterate)
  (:shadowing-import-from :iterate)
  (:export
   :getv
   :getv-chained
   :defdpar)
  ;; "bind" from metabang bind
  #.(cons :export (loop for s being the external-symbols of (find-package :metabang-bind) collect s))
  #.(cons :export (loop for s being the external-symbols of (find-package :iterate) collect s)))

(in-package :uniform-utilities)

(defun infer-intended-type-of-object (object)
  (etypecase object
    (hash-table 'hash-table)
    (string 'string)
    (vector 'vector)
    (array 'array)
    (list 'list)
    (standard-object 'standard-object)
    (structure-object 'structure-object)))

(defun getv (object key &optional intended-type-of-object)
  "Get the value associated with KEY in OBJECT.
Optionally, specify the type of OBJECT in INTENDED-TYPE-OF-OBJECT.
Pass the indexes as a list in case of an array.
INTENDED-TYPE-OF-OBJECT, if specified, must be a symbol, and one of
  HASH-TABLE, SIMPLE-STRING, SIMPLE-VECTOR, STRING, VECTOR, ARRAY, LIST,
  SEQUENCE, STANDARD-OBJECT, STRUCTURE-OBJECT."
  (unless intended-type-of-object
    (setq intended-type-of-object (infer-intended-type-of-object object)))
  ;; For reasons beyond my comprehension, (etypecase object ...) without above
  ;; instead of this doesn't let types be checked.
  (ecase intended-type-of-object
    (hash-table (gethash key object))
    (simple-string (schar object key))
    (simple-vector (svref object key))
    (string (char object key))
    (vector (aref object key))
    (array (apply #'aref object key))
    (list (nth key object))
    (sequence (elt object key))
    (standard-object (slot-value object key))
    (structure-object (slot-value object key))))

(define-compiler-macro getv (&whole form object key &optional intended-type-of-object)
  (alexandria:switch
      (intended-type-of-object :test 'equalp)
    (''hash-table `(gethash ,key ,object))
    (''simple-string `(schar ,object ,key))
    (''simple-vector `(svref ,object ,key))
    (''string `(char ,object ,key))
    (''vector `(aref ,object ,key))
    (''array `(apply #'aref ,object ,key))
    (''list `(nth ,key ,object))
    (''sequence `(elt ,object ,key))
    (''standard-object `(slot-value ,object ,key))
    (''structure-object `(slot-value ,object ,key))
    (t form)))

(defun (setf getv) (value object key &optional intended-type-of-object)
  (unless intended-type-of-object
    (setq intended-type-of-object (infer-intended-type-of-object object)))
  (ecase intended-type-of-object
    (hash-table (setf (gethash key object) value))
    (simple-string (setf (schar object key) value))
    (simple-vector (setf (svref object key) value))
    (string (setf (char object key) value))
    (vector (setf (aref object key) value))
    (array (setf (apply #'aref object key) value))
    (list (setf (nth key object) value))
    (sequence (setf (elt object key) value))
    (standard-object (setf (slot-value object key) value))
    (structure-object (setf (slot-value object key) value))))

(define-compiler-macro (setf getv)
    (&whole form value object key &optional intended-type-of-object)
  (alexandria:switch
      (intended-type-of-object :test 'equalp)
    (''hash-table `(setf (gethash ,key ,object) ,value))
    (''simple-string `(setf (schar ,object ,key) ,value))
    (''simple-vector `(setf (svref ,object ,key) ,value))
    (''string `(setf (char ,object ,key) ,value))
    (''vector `(setf (aref ,object ,key) ,value))
    (''array `(setf (apply #'aref ,object ,key) ,value))
    (''list `(setf (nth ,key ,object) ,value))
    (''sequence `(setf (elt ,object ,key) ,value))
    (''structure-object `(setf (slot-value ,object ,key) ,value))
    (''standard-object `(setf (slot-value ,object ,key) ,value))
    (t form)))

(defun getv-chained (object &rest keys)
  "Chain GETV as follows:
 >> (let ((a '((1 2 3) (4 5 6)))) (getv-chained a 0 2))
 2
However, this prohibits type specification - it poses ambiguity with
multidimensional array index."
  (reduce #'(lambda (object key) (getv object key))
          keys
          :initial-value object))

(defun (setf getv-chained) (value object &rest keys)
  " >> (let ((a '((1 2 3) (4 5 6)))) (setf (getv-chained a 0) '(a b c d)) a)
 ((A B C D) (4 5 6))
This won't work with multidimensional arrays though!"
  (setf (getv (apply #'getv-chained object (butlast keys))
              (car (last keys)))
        value))

(defun traverse-tree (var-structure val-structure &key cons)
  "A helper function for DEFDPAR.

Example: 
 >> (traverse-tree '((a b) c) '((1 2) 3) :cons 'defparameter)
 ((defparameter a (car (car ((1 2) 3))))
  (defparameter b (car (cdr (car ((1 2) 3)))))
  (defparameter c (car (cdr ((1 2) 3)))))"
  (cond ((null var-structure) ())
        ((symbolp var-structure) `((,cons ,var-structure ,val-structure)))
        (t
         (append (traverse-tree (car var-structure) `(car ,val-structure) :cons cons)
                 (traverse-tree (cdr var-structure) `(cdr ,val-structure) :cons cons)))))

(defun tree-equivalent-p (tree1 tree2)
  (cond ((and (null tree1) (null tree2)) t)
        ((or (null tree1) (null tree2) nil))
        ((symbolp tree1) t)
        (t (and (= (length tree1) (length tree2))
                (tree-equivalent-p (car tree1) (car tree2))
                (tree-equivalent-p (cdr tree1) (cdr tree2))))))

(defmacro defdpar (&rest things)
  "A combination of destructuring-bind, multiple-value-bind and defparameter.
Inspired by dsetq from :iterate library.

Example. 
 (flet ((foo () (values '(1 2) 3)))
   (defdpar 
     (:values b c) (foo)
     (d e) (foo)
     f (car (foo))))"
  (when things
    `(progn
       ,(let ((values (gensym))
              (var-structure (first things))
              (val-structure (second things)))
          (cond ((symbolp var-structure)
                 `(defparameter ,var-structure ,val-structure))
                ((eq :values (first var-structure))
                 `(let ((,values (multiple-value-list ,val-structure)))
                    (unless (tree-equivalent-p ',(cdr var-structure) ,values)
                      (error (format nil "Cannot unpack ~A to ~A"
                                     ,values ',(cdr var-structure))))
                    ,@(traverse-tree (cdr var-structure) values :cons 'cl:defparameter)))
                (t
                 `(let ((,values ,val-structure))
                    (unless (tree-equivalent-p ',var-structure ,values)
                      (error (format nil "Cannot unpack ~A to ~A" ,values ',var-structure)))
                    ,@(traverse-tree var-structure values :cons 'cl:defparameter)))))
       ,@(cdr (macroexpand-1 `(defdpar ,@(cddr things)))))))
