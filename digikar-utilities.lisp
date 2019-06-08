(defpackage :digikar-utilities
  (:use :common-lisp)
  (:export
   :get-val
   :slice-vector
   :slice-string
   :slice-list
   :slice
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

(declaim (ftype (function (vector &optional t t t *)) slice-vector))
(defun slice-vector (sequence &optional (start 0) (end (length sequence)) (interval 1)
				allow-negative-indices)
  (destructuring-bind (st ed abs-interval)
      (normalize start end interval (length sequence) allow-negative-indices)
    (let* ((out-size (ceiling  (/ (- ed st)
                                      interval)))
           (out (make-array (if (< out-size 0)
				(return-from slice-vector nil)
				out-size))))
      (if (and interval (< interval 0))
          (loop
             for i downfrom st to (1+ ed) by abs-interval
             for out-idx from 0
             do
               (setf (get-val out out-idx 'vector)
                     (get-val sequence i 'vector)))
          (loop
             for i from st to (1- ed) by abs-interval
             for out-idx from 0
             do
               (setf (get-val out out-idx 'vector)
                     (get-val sequence i 'vector))))
      out)))

(declaim (ftype (function (string &optional t t t *)) slice-string))
(defun slice-string (sequence &optional (start 0) (end (length sequence)) (interval 1)
				allow-negative-indices)
  (destructuring-bind (st ed abs-interval)
      (normalize start end interval (length sequence) allow-negative-indices)
    (let* ((out-size (ceiling  (/ (- ed st)
                                      interval)))
           (out (make-string (if (< out-size 0)
				(return-from slice-string nil)
				out-size))))
      (if (and interval (< interval 0))
          (loop
             for i downfrom st to (1+ ed) by abs-interval
             for out-idx from 0
             do
               (setf (get-val out out-idx 'string)
                     (get-val sequence i 'string)))
          (loop
             for i from st to (1- ed) by abs-interval
             for out-idx from 0
             do
               (setf (get-val out out-idx 'string)
                     (get-val sequence i 'string))))
      out)))

(declaim (ftype (function (list &optional t t t *) list) slice-list))
(defun slice-list (sequence &optional (start 0) (end (length sequence)) (interval 1)
			      allow-negative-indices)
  (let ((len (length sequence)))
    (destructuring-bind (st ed abs-interval)
	(normalize start end interval len allow-negative-indices)
      (if (and interval (< interval 0))
	  (loop
	     for elt in (nthcdr (- len st 1) (reverse sequence))
	     for i downfrom st to (1+ ed)
	     if (= 0 (rem (- i st) abs-interval))
	     collect elt)
	  (loop
	     for elt in (nthcdr st sequence)
	     for i from st to (1- ed)
	     if (= 0 (rem (- i st) abs-interval))
	     collect elt)))))

(defun slice (sequence &optional (start 0) end (interval 1)
			 (allow-negative-indices nil) &key type)
  "Equivalent of the python list slicing."
  (unless type
    (setq type
          (etypecase sequence
            (list 'list)
            (string 'string)
            (vector 'vector))))
  (let ((slice-fun
	 (ecase type
	   (list 'slice-list)
	   (string 'slice-string)
	   (vector 'slice-vector))))
    (apply slice-fun (list sequence start end interval allow-negative-indices))))

(defun normalize (start end interval length allow-negative-indices)
  (if allow-negative-indices
      (LIST (COND ((NULL START)
		   (if (and interval (< interval 0))
		       (1- length)
		       0))
		  ((< START 0) (+ START LENGTH))
		  (T  START))
	    (COND ((NULL END)
		   (if (and interval (< interval 0))
		       -1
		       LENGTH))
		  ((< END 0) (+ END LENGTH))
		  (T  END))
	    (cond (interval (abs interval))
		  (t 1)))
      (progn
	(assert (<= 0 (or start 0) (or end length)))
	(assert (<= 0 (or interval 1)))
	(list (or start 0) (or end length) (or interval 1)))))

(define-compiler-macro slice
    (&whole form sequence &optional (start 0) end (interval 1)
	    (allow-negative-indices nil) &key type)
  (if (or (null interval) (= 1 interval))
      `(progn
	 (assert (<= 0 (or ,start 0)))
	 (assert (<= 0 (or ,end 0)))
	 (assert (<= 0 (or ,interval 1)))
	 (subseq ,sequence (or ,start 0) ,end))
      (alexandria:switch
       (type :test 'equalp)
       (''list `(slice-list ,sequence ,start ,end ,interval ,allow-negative-indices))
       (''string `(slice-string ,sequence ,start ,end ,interval ,allow-negative-indices))
       (''vector `(slice-vector ,sequence ,start ,end ,interval ,allow-negative-indices))
       (t form))))

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
     finally (return (apply #'vector objects))))

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
       (return `(alexandria:alist-hash-table (list ,@pairs) :test 'equal))))

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
       (return `(alexandria:alist-hash-table ',pairs :test 'equal))))

  
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


