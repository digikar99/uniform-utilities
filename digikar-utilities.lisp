(defpackage :digikar-utilities
  (:use :common-lisp)
  (:export
   :get-val
   :join-strings-using
   :list-case
   :prefix-to-infix
   :read-file
   :write-file
   :getf-equal
   :replace-all
   :shallow-copy))

(in-package :digikar-utilities)

(defun get-val (object key &optional intended-type-of-object)
  "Get the value associated with KEY in OBJECT.
Optionally, specify the type of OBJECT in INTENDED-TYPE-OF-OBJECT.
Pass the indexes as a list in case of an array."
  (unless intended-type-of-object
    (setq intended-type-of-object
	  (etypecase object
	    (vector 'vector)
	    (hash-table 'hash-table)
	    (array 'array)
	    (list 'list))))
  (ecase intended-type-of-object
	  (hash-table (gethash key object))
	  (sequence (elt object key))
	  (simple-vector (svref object key))
	  (vector (aref object key))
	  (array (apply #'aref object key))
	  (string (char object key))
	  (list (nth key object))))

(define-compiler-macro get-val (&whole form object key &optional intended-type-of-object)
  "Get the value associated with KEY in OBJECT.
Optionally, specify the type of OBJECT in INTENDED-TYPE-OF-OBJECT.
Pass the indexes as a list in case of an array."
  (alexandria:switch
       (intended-type-of-object :test 'equalp)
       (''hash-table `(gethash ,key ,object))
       (''sequence `(elt ,object ,key))
       (''simple-vector `(svref ,object ,key))
       (''vector `(aref ,object ,key))
       (''array `(apply #'aref ,object ,key))
       (''string `(char ,object ,key))
       (''list `(nth ,key ,object))
       (t form)))

(defun (setf get-val) (value object key &optional intended-type-of-object)
  "Set the value associated with KEY in OBJECT to VALUE. (is destructive)
Optionally, specify the type of OBJECT in INTENDED-TYPE-OF-OBJECT.
Pass the indexes as a list in case of an array."
  (unless intended-type-of-object
    (setq intended-type-of-object
	  (etypecase object
	    (vector 'vector)
	    (hash-table 'hash-table)
	    (array 'array)
	    (list 'list))))
  (ecase intended-type-of-object
      (hash-table (setf (gethash key object) value))
      (sequence (setf (elt object key) value))
      (simple-vector (setf (svref object key) value))
      (vector (setf (aref object key) value))
      (array (setf (apply #'aref object key) value))
      (string (setf (char object key) value))
      (list (setf (nth key object) value))))

(define-compiler-macro (setf get-val)
        (&whole form value object key &optional intended-type-of-object)
  "Set the value associated with KEY in OBJECT to VALUE. (is destructive)
Optionally, specify the type of OBJECT in INTENDED-TYPE-OF-OBJECT.
Pass the indexes as a list in case of an array."
  (alexandria:switch
   (intended-type-of-object :test 'equalp)
   (''hash-table `(setf (gethash ,key ,object) ,value))
   (''sequence `(setf (elt ,object ,key) ,value))
   (''simple-vector `(setf (svref ,object ,key) ,value))
   (''vector `(setf (aref ,object ,key) ,value))
   (''array `(setf (apply #'aref ,object ,key) ,value))
   (''string `(setf (char ,object ,key) ,value))
   (''list `(setf (nth ,key ,object) ,value))
   (t form)))

;; (defmacro slice (object &optional start end interval &key type)
;;   (if type
;;       (progn
;; 	(setq intended-type-of-object (cadr intended-type-of-object))
;; 	(case intended-type-of-object
;; 	  (hash-table `(gethash ,key ,object))
;; 	  (sequence `(elt ,object ,key))
;; 	  (simple-vector `(svref ,object ,key))
;; 	  (vector `(aref ,object ,key))
;; 	  (array `(apply #'aref ,object ,key))
;; 	  (string `(char ,object ,key))
;; 	  (list `(nth ,key ,object))))
;;       `(cond ((vectorp ,object) (aref ,object ,key))
;; 	     ((hash-table-p ,object) (gethash ,key ,object))
;; 	     ((arrayp ,object) (apply #'aref ,object ,key))
;; 	     ((listp ,object) (nth ,key ,object))
;; 	     (t
;; 	      (error (format nil
;; 			     "Type of ~d cannot be inferred"
;; 			     ,object))))))

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
     finally (return `(vector ,@objects))))

(defun read-vector-literally (stream char)
  (declare (ignore char))
  (loop
     for object = (read-next-object +right-square+ stream)
     while object
     collect object into objects
     finally (return `(cons 'vector objects))))

(defun read-hash-table (stream char n)
  (declare (ignore char))
  (declare (ignore n))
  (loop
     for key = (read-next-object +right-brace+ stream)
     while key
     for value = (read-next-object +right-brace+ stream)
     while value
     for pair = `(cons ,key ,value)
     collect pair into pairs
     finally
       (return `(alexandria:alist-hash-table (list ,@pairs)))))

(defun read-hash-table-literally (stream char)
  (declare (ignore char))
  (loop
     for key = (read-next-object +right-brace+ stream)
     while key
     for value = (read-next-object +right-brace+ stream)
     while value
     for pair = (cons key value)
     collect pair into pairs
     finally
       (return `(alexandria:alist-hash-table ',pairs))))

  
(set-dispatch-macro-character #\# #\{ #'read-hash-table)
(set-dispatch-macro-character #\# #\[ #'read-vector)
(set-macro-character #\{ #'read-hash-table-literally)
(set-macro-character #\[ #'read-vector-literally)
(set-macro-character +right-brace+ 'read-delimiter)
(set-macro-character +right-square+ 'read-delimiter)


;; ------------------------------------------------------------------------

;; This is a bit performant (using TIME) that using format and concatenate
(defun join-strings-using (delimiter-string &rest args)
  (with-output-to-string (out)
    (format out (first args))
    (loop
       for arg in (cdr args)
       do
	 (format out delimiter-string)
	 (format out arg))))

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
    (values (when f (read f))
	    (every #'identity (list f)))))

(defun write-file (filename lisp-object &optional if-exists)
  "Writes the lisp-object to file filename, overwrites if the file already exists."
  (when (and (not if-exists) (probe-file filename))
    (setq if-exists
	  (progn
	    (format t "~d already exists. Would you like to~%" filename)
	    (format t " [1] Rename the old file to ~d.bak~%" filename)
	    (format t "  2  Replace (supersede) the old file~%")
	    (format t "  3  Don't do anything~%")
	    (format t "Specify the chosen option number: ")
	    (force-output)
	    (setq if-exists (read))
	    (case if-exists
	      (1 :rename)
	      (2 :supersede)
	      (3 (return-from write-file nil))
	      (t :rename)))))
  (with-open-file (f filename :direction :output :if-does-not-exist :create
                     :if-exists if-exists)
    (format f "~d" (write-to-string lisp-object))
    t))

(defun getf-equal (plist indicator)
  "getf using #'equal for comparison"
  (loop for key in plist by #'cddr
        for value in (rest plist) by #'cddr
        when (equal key indicator)
        return value))

;; https://lispcookbook.github.io/cl-cookbook/strings.html
;; WARNING code not optimized for large files.
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
(defgeneric shallow-copy (object &rest initargs &key &allow-other-keys)
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
  (:export :nand
	   :nor
	   :->
	   :<-
	   :<>
	   :gen-truth-table))

(in-package :digikar-utilities.logic)

(defmacro nand (&rest args) `(not (and ,@args)))
(defmacro nor (&rest args) `(not (or ,@args)))


(defun -> (x y) "Truth value of x implies y"(or (not x) y))
(defun <- (x y) "Truth value of y implies x"(or (not y) x))
(defun <> (x y) "Truth value of x if and only if y" (and (-> x y) (<- x y)))

(defun gen-all-cases (sym)
  (if (null sym) '(())
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


