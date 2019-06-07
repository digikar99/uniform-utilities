---
title: digikar-utilities
---

A library for "getting things done" in Common Lisp.

# Installation

WARNING: This will modify your readtable. 

This library is not stable - it is subject to changes. Though, at this point, only additions are likely.

```lisp
CL-USER> (ql:quickload 'digikar-utilities)
To load "digikar-utilities":
  Load 1 ASDF system:
    digikar-utilities
; Loading "digikar-utilities"

(DIGIKAR-UTILITIES)
```

Available on github at [digikar99/cl-digikar-utilities](https://github.com/digikar99/cl-digikar-utilities).

Please report the issues on [github](https://github.com/digikar99/cl-digikar-utilities/issues).

# Examples and Documentation

<p id="two-cols"></p>

## Syntax and Interfaces for Built-in classes

### Square Brackets for Vectors

```lisp
CL-USER> [a b]
#(a b)

CL-USER> (aref [a b] 0)
A
```
`'equal` has been used as the test function, as it allows for strings. If you want evaluation, the following syntax may be used.

```lisp
CL-USER> (let* ((a 1) (vec [a 3]))
           (aref vec 0))
1
```

### Braces for Hash Tables

```lisp
CL-USER> {a b}
#<HASH-TABLE :TEST EQUAL :COUNT 1 {1002AB3E03}>

CL-USER> (gethash 'a {a b})
B
T
```
`'equal` has been used as the test function, as it allows for strings. If you want evaluation, the following syntax may be used.

```lisp
CL-USER> (let* ((a 1) (hash #{a 'b}))
           (gethash 1 hash))
B
T
```
### get-val

`(get-val object key &optional intended-type-of-object)`

Examples:

```lisp
CL-USER> (get-val [a b] 0)
A
CL-USER> (get-val {a b} 'a)
B
T
CL-USER> (get-val '(a b c) 0)
A
CL-USER> (get-val "abc" 0)
#\a
CL-USER> (get-val #2A((0 0)) '(0 0))
0
```
For safety, the following may also be used:
```lisp
CL-USER> (defvar some-var [a b])
SOME-VAR
CL-USER> (get-val some-var 0 'hash-table)
; Evaluation aborted on #<TYPE-ERROR expected-type: HASH-TABLE datum: #<(SIMPLE-VECTOR 2) {10032C361F}>>.
```
Currently, supported values for `intended-type-of-object` include `hash-table`,
`vector`, `simple-vector`, `array`, `string` and `sequence`.

Also, there is no runtime overhead on specifying `intended-type-of-object` explicitly:
```lisp
CL-USER> (funcall (compiler-macro-function 'get-val) 
                  '(get-val [a b] 0 'vector)
                  nil)
(AREF #(A B) 0)

CL-USER> (funcall (compiler-macro-function 'get-val) 
                  '(get-val [a b] 0 (first '(vector)))
                  nil)
(GET-VAL #(A B) 0 (FIRST '(VECTOR)))

```

### (setf get-val)
Also refer [get-val](#get-val).
```lisp
CL-USER> (defvar vec [a b])
VEC
CL-USER> (setf (get-val vec 0) 5)
5
CL-USER> vec
#(5 B)
```

### slice
`(slice sequence &optional (start 0) end (interval 1) allow-negative-indices &key type)`

Equivalent of the python list slicing:
```lisp
CL-USER> (slice [1 2 3 4 5] 3)
#(4 5)
CL-USER> (slice [1 2 3 4 5] 3 4)
#(4)
CL-USER> (slice [1 2 3 4 5] nil nil -2)
NIL
CL-USER> (slice [1 2 3 4 5] nil nil -2 t)
#(5 3 1)
```
Type specialisation if specified:
```lisp
CL-USER> (funcall (compiler-macro-function 'slice)
                  '(slice "abcde" nil nil 2 t :type 'string)
                  nil)
(SLICE-STRING "abcde" NIL NIL 2)
```
Reduces to `subseq` if interval is 1 or `nil`:
```lisp
CL-USER> (funcall (compiler-macro-function 'slice)
                  '(slice "abcde" nil nil 1 t :type 'string)
                  nil)
(SUBSEQ "abcde" (OR NIL 0) NIL)
```

## Other utility functions

### join-strings-using
`(join-strings-using delimiter-string &rest args)`
```lisp
CL-USER> (join-strings-using (string #\tab) "a" "b")
"a	b"
```

### list-case
`(list-case list &rest clauses)`
```lisp
CL-USER> (list-case '(1 2 3)
                    ((x y) (+ x y))
                    ((x y z) (- (+ x y) z)))
0
```

### prefix-to-infix
`(prefix-to-infix expr)`
```lisp
CL-USER> (prefix-to-infix '(+ (/ 5 6)
                              7 
                              (* 8 9)))
((5 / 6) + 7 + (8 * 9))
```

### write-file
`(write-to-file filename lisp-object &optional if-exists)`
```lisp
CL-USER> (write-file "file" '(10 20 30))
T
CL-USER> (write-file "file" '(56 40))
file already exists. Would you like to
 [1] Rename the old file to file.bak
  2  Replace (supersede) the old file
  3  Don't do anything
Specify the chosen option number: 2

T
```

### read-file
`(read-file filename)` returns two values. The first value is the contents of the file, if the second value is T. The second value is nil if the file does not exist.
```lisp
CL-USER> (read-file "file")
(56 40)
T
CL-USER> (read-file "non-existent")
NIL
NIL
```

### replace-all
`(replace-all string part replacement &key (test #'char=))`
```lisp
CL-USER> (replace-all "5, 6, 7" "," "")
"5 6 7"
```
This function is taken from [the Common Lisp Cookbook](https://lispcookbook.github.io/cl-cookbook/strings.html) and is unoptimized for large inputs.

### shallow-copy
`(shallow-copy object &rest initargs &key &allow-other-keys)`
```lisp
CL-USER> (defclass foo ()
           ((str :initarg :str :accessor str)))
#<STANDARD-CLASS COMMON-LISP-USER::FOO>
CL-USER> (defvar obj (make-instance 'foo :str "abcd"))
OBJ
CL-USER> (defvar obj2 (shallow-copy obj))
OBJ2
CL-USER> (eq obj obj2)
NIL
CL-USER> (eq (str obj2) (str obj))
T
```
Discussion about copying objects can be found [here](https://stackoverflow.com/questions/11067899/is-there-a-generic-method-for-cloning-clos-objects).


# Also check out

## [The Common Lisp Cookbook](http://lispcookbook.github.io/cl-cookbook/)

---

This template was taken from [The Common Lisp Cookbook][tCLC].

[tCLC]: https://github.com/LispCookbook/cl-cookbook