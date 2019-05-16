;; This library intends to provide a python like interface
;; to getting things done in common lisp.
;; These features include:
;; - easier usage of vectors and hash-tables

;; eval has been used in read-left-brace and read-left-bracket

(defpackage :digikar-utilities
  (:use :common-lisp)
  (:export
   :make-hash
   :make-vector
   :*eval-in-vector*
   :*eval-in-hash-table*
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
   :replace-all))

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

(defun get-val (vec/hash key)
  "Get the value associated with key in the hash-table, or 
the value at position key in the vector."
  (cond ((vectorp vec/hash) (aref vec/hash key))
        ((hash-table-p vec/hash) (gethash key vec/hash))
        (t (error "Expected vector or hash-table"))))

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
(defconstant +left-bracket+ #\()
(defconstant +right-bracket+ #\))
(defconstant +left-brace+ #\{)
(defconstant +right-brace+ #\})
(defconstant +comma+ #\,) ;; separator in hash-tables

(defun read-separator (stream char)
  (declare (ignore stream))
  (error "Separator ~S shouldn't be read alone" char))

(defun read-delimiter (stream char)
  (declare (ignore stream))
  (error "Delimiter ~S shouldn't be read alone" char))

(defun read-next-object-for-vector
    (delimiter &optional (input-stream *standard-input*))
  (flet ((peek-next-char () (peek-char t input-stream t nil t))
         (discard-next-char () (read-char input-stream t nil t)))
    (if (and delimiter (char= (peek-next-char) delimiter))
        (progn
          (discard-next-char)
          nil)
      (read input-stream t nil t))))

(defvar *eval-in-vector* t
  "If true #(a b) can be read as #(1 2), where a=1 and b=2; else as #(a b).")

(defun read-left-bracket (stream char n)
  (declare (ignore char))
  (declare (ignore n))
  (let ((*readtable* (copy-readtable)))
    (loop
     for object = (read-next-object-for-vector +right-bracket+ stream)
     while object
     collect (if *eval-in-vector* (eval object) object) into objects
     finally (return (make-vector objects)))))

(defun read-next-object-for-hash-table
    (delimiter separator &optional (input-stream *standard-input*))
  (flet ((peek-next-char () (peek-char t input-stream t nil t))
         (discard-next-char () (read-char input-stream t nil t)))
    (if (and delimiter (char= (peek-next-char) delimiter))
        (progn
          (discard-next-char)
          nil)
      (let* ((object (read input-stream t nil t))
             (next-char (peek-next-char)))
        (cond
         ((char= next-char separator) (discard-next-char))
         ((and delimiter (char= next-char delimiter)) nil))
        object))))

(defvar *eval-in-hash-table* t
  "If true #{a b} can be read as #{1 2}, where a=1 and b=2; else as #{a b}.")

(defun read-left-brace (stream char n)
  (declare (ignore char))
  (declare (ignore n))
  (let ((*readtable* (copy-readtable)))
    (set-macro-character +comma+ 'read-separator)
    (loop
     for key = (read-next-object-for-hash-table +right-brace+
                                                +comma+
                                                stream)
     while key
     for value = (read-next-object-for-hash-table +right-brace+
                                                  +comma+
                                                  stream)
     collect (if *eval-in-hash-table*
                 (list (eval key) (eval value))
               (list key value))
     into pairs
     finally (return (make-hash pairs)))))

(set-dispatch-macro-character #\# #\( #'read-left-bracket)
(set-dispatch-macro-character #\# #\{ #'read-left-brace)
(set-macro-character +right-bracket+ 'read-delimiter)
(set-macro-character +right-brace+ 'read-delimiter)

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


