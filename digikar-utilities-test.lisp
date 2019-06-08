
(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload '(prove digikar-utilities) :silent t))

(in-package :cl-user)

(defpackage :digikar-utilities.test
  (:use :cl :prove :digikar-utilities))
(in-package :digikar-utilities.test)

(plan 6)
(subtest "READER MACROs"
  ;; (format t "Testing reader macros...~%~%")
  (is [a b 1 2 "a"] #(a b 1 2 "a") :test 'equalp)
  (is (let ((a 1) (b 2)) #[a b]) #(1 2) :test 'equalp)
  (is {a b} (alexandria:plist-hash-table '(a b) :test 'equal) :test 'equalp)
  (is (let ((a "a") (b 2))
        #{a b})
      (alexandria:plist-hash-table '("a" 2) :test 'equal) :test 'equalp)
  (force-output))

(defmacro is-compiler-expand (got expected)
  `(is (funcall (compiler-macro-function (car ',got))
                ',got
               nil)
       ',expected
      :test 'equalp))

(let ((str "abcde")
      (vec [a b c d e])
      (list '(a b c d e))
      (dict {a b c d})
      (arr #2A ((1 2) (3 4))))
  (subtest "SLICE - as a function"
    (loop for obj in (list str vec list)
       do
       ;; returned object is a copy
         (isnt (slice obj 0 5) (slice obj 0 5) :test 'eq) 
       ;; allow-negative-indices if nil
         (is-error (slice obj nil nil -1) 'simple-error)
         (is-error (slice obj nil -2 1) 'simple-error)
         (is-error (slice obj -2 -1 1) 'simple-error)
       ;; allow negative indices is t
         (is (slice obj nil nil -1 t) (reverse obj) :test 'equalp)
         (is (slice obj 0 5 -1 t) '() :test 'equalp)
         (is (slice obj -1 -6 -1 t) (reverse obj) :test 'equalp))
    ;; backward indexing works as expected, at least for these cases
    (is (slice str 4 0 -2 t) "ec" :test 'equalp)
    (is (slice str -1 0 -2 t) "ec" :test 'equalp)
    (is (slice vec 4 0 -2 t) [e c] :test 'equalp)
    (is (slice list 4 0 -2 t) '(e c) :test 'equalp))
  (subtest "SLICE - as a macro"
    (locally
        ;; type warning for this form
        (declare (sb-ext:muffle-conditions warning))
      (is-error (slice vec nil nil 2 nil :type 'string) 'simple-type-error))
    (is (car (last
              (funcall (compiler-macro-function 'slice)
                       '(slice vec nil nil nil)
                       nil)))
        '(subseq vec (or nil 0) nil)
        :test 'equalp)
    (is-compiler-expand (slice str nil nil 2 nil :type 'string)
                        (slice-string str nil nil 2 nil))
    (is-compiler-expand (slice vec nil nil 2 nil :type 'vector)
                        (slice-vector vec nil nil 2 nil))
    (is-compiler-expand (slice list nil nil 2 nil :type 'list)
                        (slice-list list nil nil 2 nil))
    (let ((a 1) (b 3) (c 2))
      `(is-compiler-expand (slice vec ,a ,b ,c t :type 'vector)
                           (slice-vector vec ,a ,b ,c t))))

  (subtest "GET-VAL as a function"
    (is (get-val str 0) #\a)
    (is (get-val vec 0) 'a)
    (is (get-val list 0) 'a)
    (is (get-val dict 'a) 'b)
    (is (get-val arr (list 0 1)) 2))
  (subtest "GET-VAL as compiler macro"
    (is-compiler-expand (get-val str 0) (get-val str 0))
    (is-compiler-expand (get-val str 0 'string) (char str 0))
    (is-compiler-expand (get-val vec 0 'string) (char vec 0))
    (is-compiler-expand (get-val vec 0 'vector) (aref vec 0))
    (is-compiler-expand (get-val arr (list 0 1) 'array)
                        (apply #'aref arr (list 0 1)))
    (is-compiler-expand (get-val dict 'c 'hash-table)
                        (gethash 'c dict))))



