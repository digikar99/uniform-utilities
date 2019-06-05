;; This library intends to provide a python like interface
;; to getting things done in common lisp.
;; These features include:
;; - easier usage of vectors and hash-tables

(defpackage :digikar-utilities
  (:use :common-lisp)
  (:export
   :make-hash
   :make-vector
   :join-using
   :list-case
   :get-val
   :set-val
   :add
   :nand
   :nor
   :prefix-to-infix
   :read-file
   :write-file
   :getf-equal
   :replace-all
   :copy-instance))

(in-package :digikar-utilities)

;; the following function needs to be defined before the
;; +format-delimiters+ constant, for obvious reasons.
(defun make-hash (pairs)
  "Takes input in the form '((1 2) (3 4)) and returns a hash-table
with the mapping 1=>2 and 3=>4."
  (if (hash-table-p pairs)
      pairs ; to take care of the reader macro syntax confusion defined below
    (let ((hash-table (make-hash-table :test 'equal)))
      (loop for (key val) in pairs do
            (setf (gethash key hash-table) val))
      hash-table)))

(defvar +format-delimiters+
  (make-hash '(("\n" "~%"))))
(defun get-format-delimiters (delimiter)
  "format uses some seemingly obscure delimiters, such as ~% instead of \n."
  (let ((format-delimiter (gethash delimiter +format-delimiters+)))
    (if format-delimiter format-delimiter delimiter)))

(defun join-using (delimiter list/vector)
  "Joins the elements of list / vector using the delimiter. 
Equivalent of the python delimiter.join function."
  (when (vectorp list/vector)
    (setq list/vector (loop for val across list/vector collect val)))
  (format nil (concatenate 'string
                           "~{~A~^"
                           (get-format-delimiters delimiter)
                           "~}")
          list/vector))

(defun make-vector (list)
  "Converts list to vector."
  (apply #'vector list))

(defmacro get-val (object key &optional intended-type-of-object)
  "Get the value associated with KEY in OBJECT.
Optionally, expand the code according to INTENDED-TYPE-OF-OBJECT"
  (if intended-type-of-object
      (progn
	(setq intended-type-of-object (cadr intended-type-of-object))
	(case intended-type-of-object
	  (hash-table `(gethash ,key ,object))
	  (sequence `(elt ,object ,key))
	  (simple-vector `(svref ,object ,key))
	  (vector `(aref ,object ,key))
	  (array `(aref ,object ,key))
	  (string `(char ,object ,key))
	  (list `(nth ,object ,key))))
      `(cond ((vectorp ,object) (aref ,object ,key))
	     ((hash-table-p ,object) (gethash ,key ,object))
	     (t
	      (error (format nil
			     "Type of ~d cannot be inferred"
			     ,object))))))

(defun set-val (vec/hash key value)
  "Set the value (destructive) associated with key in the hash-table, or 
the value at position key in the vector, to value."
  (cond ((vectorp vec/hash) (setf (aref vec/hash key) value))
        ((hash-table-p vec/hash) (setf (gethash key vec/hash) value))
        (t (error "Expected vector or hash-table"))))

;; ==========================================================================
;; The following code for json-like reader macros was originally found at:
;; https://gist.github.com/chaitanyagupta/9324402
;;
;; It has been modified since then.

(defconstant +hash+ #\#)
(defconstant +left-square+ #\[)
(defconstant +right-square+ #\])
(defconstant +left-brace+ #\{)
(defconstant +right-brace+ #\})

(defun read-delimiter (stream char)
  (declare (ignore stream))
  (error "Delimiter ~S shouldn't be read alone" char))

(defun read-next-object
    (delimiter &optional (input-stream *standard-input*))
  (flet ((peek-next-char () (peek-char t input-stream t nil t))
         (discard-next-char () (read-char input-stream t nil t)))
    (if (and delimiter (char= (peek-next-char) delimiter))
        (progn
          (discard-next-char)
          nil)
      (read input-stream t nil t))))

(defun read-vector (stream char n)
  (declare (ignore char))
  (declare (ignore n))
  (loop
     for object = (read-next-object +right-square+ stream)
     while object
     collect object into objects
     finally (return `(make-vector (list ,@objects)))))

(defun read-vector-literally (stream char)
  (declare (ignore char))
  (loop
     for object = (read-next-object +right-square+ stream)
     while object
     collect object into objects
     finally (return (make-vector objects))))

;; OBSERVATION: The following doesn't work as expected.
;; (Originally, the decision to eval was determined by an *eval-in-<type>*
;;  global variable as below.)
;; (let* ((*eval-in-hash-table* t)
;;        (a 2))
;;   #{a "2"}) 
;; PROPOSITION: Merely declaiming / declaring this as special won't work,
;; because read-expansion happens before the value of *eval* is changed.
;; POSSIBLE SOLUTION: Use the value of n to indicate whether to eval.
;; Further, for safety purposes, eliminate eval altogether.

(defun read-hash-table (stream char n)
  (declare (ignore char))
  (declare (ignore n))
  (loop
       for key = (read-next-object +right-brace+ stream)
       while key
       for value = (read-next-object +right-brace+ stream)
       while value
       for pair = `(list ,key ,value)
       collect pair into pairs
       finally
         (return `(make-hash (list ,@pairs)))))

(defun read-hash-table-literally (stream char)
  (declare (ignore char))
  (loop
       for key = (read-next-object +right-brace+ stream)
       while key
       for value = (read-next-object +right-brace+ stream)
       while value
       for pair = (list key value)
       collect pair into pairs
       finally
         (return `(make-hash ',pairs))))

(set-dispatch-macro-character #\# #\{ #'read-hash-table)
(set-dispatch-macro-character #\# #\[ #'read-vector)
(set-macro-character #\{ #'read-hash-table-literally)
(set-macro-character #\[ #'read-vector-literally)
(set-macro-character +right-brace+ 'read-delimiter)
(set-macro-character +right-square+ 'read-delimiter)


;; ------------------------------------------------------------------------

(defmacro list-case (list &rest clauses)
  "Case using different lengths of list.
Example: CL-USER> (list-case '(1 2 3)
                             ((x y) (+ x y))
                             ((x y z) (- (+ x y) z)))
         0"
  `(let ((len (length ,list)))
     (case len
       ,@(loop for clause in clauses
               collect (list (length (car clause)) 
                             `(destructuring-bind ,(car clause) ,list
                                ,@(cdr clause)))))))

(defun add (&rest args)
  "Returns the addition of numbers, or concatenation of strings or lists."
  (when args
    (cond ((numberp (first args)) (apply #'+ args))
          ((listp (first args)) (apply #'append args))
          ((stringp (first args)) (apply #'concatenate 'string args)))))

(defmacro nand (&rest args) `(not (and ,@args)))
(defmacro nor (&rest args) `(not (or ,@args)))

(defun prefix-to-infix (expr)
  (cond ((or (not (listp expr))
             (equal 'not (car expr))) expr)
        (t
         (apply #'append `(,(prefix-to-infix (cadr expr)))
                (loop for var in (cddr expr)
                      collect (list (car expr)
                                    (prefix-to-infix var)))))))

(defun read-file (filename)
  "Read and returns the first lisp-object from file filename."
  (with-open-file (f filename :direction :input :if-does-not-exist nil)
                  (when f (read f))))

(defun write-file (filename lisp-object)
  "Writes the lisp-object to file filename, overwrites if the file already exists."
  (with-open-file (f filename :direction :output :if-does-not-exist :create
                     :if-exists :supersede)
                  (format f "~d" lisp-object)))

(defun getf-equal (plist indicator)
  "getf using #'equal for comparison"
  (loop for key in plist by #'cddr
        for value in (rest plist) by #'cddr
        when (equal key indicator)
        return value))

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement. Credits: Common Lisp Cookbook"
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
       while pos)))

;;; CREDITS: https://stackoverflow.com/questions/11067899/is-there-a-generic-method-for-cloning-clos-objects
(defgeneric copy-instance (object &rest initargs &key &allow-other-keys)
  (:documentation "Makes and returns a shallow copy of OBJECT.

  An uninitialized object of the same class as OBJECT is allocated by
  calling ALLOCATE-INSTANCE.  For all slots returned by
  CLASS-SLOTS, the returned object has the
  same slot values and slot-unbound status as OBJECT.

  REINITIALIZE-INSTANCE is called to update the copy with INITARGS.")
  (:method ((object standard-object) &rest initargs &key &allow-other-keys)
    (let* ((class (class-of object))
           (copy (allocate-instance class)))
      (dolist (slot-name (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots class)))
        (when (slot-boundp object slot-name)
          (setf (slot-value copy slot-name)
            (slot-value object slot-name))))
      (apply #'reinitialize-instance copy initargs))))

;; ========================================================================

(defpackage :digikar-utilities.logic
  (:use :common-lisp)
  (:export :-> :<- :<> :gen-truth-table))

(in-package :digikar-utilities.logic)

(defun -> (x y) "Truth value of x implies y"(or (not x) y))
(defun <- (x y) "Truth value of y implies x"(or (not y) x))
(defun <> (x y) "Truth value of x if and only if y" (and (-> x y) (<- x y)))

(defun nilp (list) "Returns nil if the list is not nil." (equal nil list))
(defun gen-all-cases (sym)
  (if (nilp sym) '(())
    (let* ((recursed (gen-all-cases (cdr sym)))
           (with-truth (mapcar (lambda (l) (cons t l)) recursed))
           (with-nil (mapcar (lambda (l) (cons nil l)) recursed)))
      (append with-truth with-nil))))

(defmacro gen-truth-table (symbols expression)
  "Generate truth table of expression. symbols should be a list of all 
the boolean variables present in expr."
  (declare)
  `(let ((all-cases (gen-all-cases (quote ,symbols))))
     (values
      (loop for case in all-cases
            ;; (print case)
            collect (cons (apply (lambda ,symbols ,expression) case)
                          (list case)))
      ',symbols)))


