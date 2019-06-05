
## Background and Introduction

WARNING: This will modify your read-table.

This is yet another utility library for common lisp. (Several libraries can be found at [cliki](https://cliki.net/utilities). Notable ones besides those include  [Alexandria](http://common-lisp.net/project/alexandria/) and [cl21](https://lispcookbook.github.io/cl-cookbook/cl21.html). There's also [a good discussion on reddit about "fixing" common lisp](https://www.reddit.com/r/lisp/comments/6t6fqs/which_sugared_library_do_common_lispers_prefer/).

I don't think it is good enough yet; therefore, I'm using a personalized name - in case someone comes up with a "God" level library, let them use a good name. (Learnt over reddit that one should give a good name, only after it is proven to be good. And it is reasonable: we don't want to waste good names. :p)

## 1 Min Overview

#### Packages and exported functions / macros

_digikar-utilities_

- make-hash
- make--vector
- join-using
- list-case
- get-val
- set-val
- add
- nand
- nor
- prefix-to-infix
- write-file
- read-file
- getf-equal
- copy-instance

_digikar-utilities.logic_

- ->
- <-
- <>
- gen-truth-tableq

The documentation for each of these can be viewed using `(describe ,symbol-name)`. (Eg. `(describe 'join-using)`.) 

#### Examples


```lisp
    CL-USER> (ql:quickload 'digikar-utilities)
    (DIGIKAR-UTILITIES)

    CL-USER> (digikar-utilities:make-vector '(1 2 3))
    #(1 2 3)

    CL-USER> (digikar-utilities:make-hash '(("a" 1) (5 25)))
    #<HASH-TABLE :TEST EQUAL :COUNT 2 {1003B1AD33}>

    CL-USER> (setq myvar 555)
    555

    CL-USER> [myvar]
    #(MYVAR) ;; changing this print-object is non-trivial with my knowledge.

    CL-USER> (get-val {myvar "myvar"} 'myvar)
    "myvar"
    T
    ;; while a print-object function can be defined for hash-tables, and was 
    ;; included previously, it can clutter up the screen when the hash-table 
    ;; is large.

    CL-USER> #[myvar] ;; also for hash-tables
    #(555) 
    ;; While it is possible to do this by using a state variable,
    ;; since this expansion happens at read-time, it does not work
    ;; for forms like let and progn.

    CL-USER> (setq nested #[4                 ; Currently, there is no
                            #{'a [5 6]}])     ; indentation support
    ;; Also, be careful about what to eval
    #(4 #<HASH-TABLE :TEST EQUAL :COUNT 1 {1002002CF3}>)
    
    CL-USER> (get-val (get-val nested 1) 'a) ; uniform syntax for 
    #(5 6)                                   ; both hash-tables and vectors

    CL-USER> (set-val nested 1 (+ 1 2))
    3

    CL-USER> (digikar-utilities:join-using " " '("aa" "b")) ; also works with vectors
    "aa b"

    CL-USER> (digikar-utilities:list-case '(1 2 3)
                                          ((x y) (+ x y))
                                          ((x y z) (- (+ x y) z)))
    0
    
    CL-USER> (digikar-utilities:add 4 5 6)
    15

    CL-USER> (digikar-utilities:add "a" "b")
    "ab"

    CL-USER> (digikar-utilities:add '(1 2 3) '(4)) ; those are only three cases here
    (1 2 3 4)

    CL-USER> (digikar-utilities:prefix-to-infix '(+ a (- 4 5) b))
    (A + (4 - 5) + B)

    CL-USER> (digikar-utilities:write-file "testing" '(a b c))
    NIL

    CL-USER> (digikar-utilities:read-file "testing")
    (a b c)

    CL-USER> (digikar-utilities:getf-equal '("a" "b" "c" "d") "a")
    "b"

    CL-USER> (digikar-utilities:replace-all "hello" "l" "a")
    "heaao"

    CL-USER> (defclass foo () ())
    #<STANDARD-CLASS COMMON-LISP-USER::FOO>

    CL-USER> (digikar-utilities:copy-instance (make-instance 'foo))
    #<FOO {1001F93E73}>

    CL-USER> (digikar-utilities.logic:gen-truth-table (a b c) (and a b c))
    ((T (T T T)) (NIL (T T NIL)) (NIL (T NIL T)) (NIL (T NIL NIL)) (NIL (NIL T T))
     (NIL (NIL T NIL)) (NIL (NIL NIL T)) (NIL (NIL NIL NIL)))
    (A B C)
    
```
