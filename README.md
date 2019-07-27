# uniform-utilities

The library aims to provide a uniform way of getting things done in Common Lisp. Unlike its alpha-variant (digikar-utiliites), this does not modify the readtable, and has a single goal.

Towards this, it defines:

- getv
- getv-chained
- defdpar
- dlet*

and borrows from  and [iterate](https://digikar99.github.io/cl-iterate-docs/). (`iterate` is available separately on quicklisp.)

There also exists [access](https://github.com/AccelerationNet/access) which might be considered more uniform that `getv` below; however, it has a significant runtime overhead (factor of 10) and type checking seems to be at issue.

Further, there is also [metabang-bind](https://common-lisp.net/project/metabang-bind/user-guide.html), which [isn't perfect](https://www.reddit.com/r/Common_Lisp/comments/cfxk03/unifying_all_bindings_metabangbindbind/) - neither is this library though.

### getv
`(getv object key &optional intended-type-of-object)`

`getv` provides a uniform interface to strings, vectors, hash-tables, arrays, lists, standard objects as well as structs. On specifying `intended-type-of-object`, there is no runtime overhead, along with type safety of the specialized functions assocated with the data types - this is achieved using `compiler-macros`.

There also exists `(setf getv)`. This, too, has compiler macros.

### getv-chained
`(getv-chained object &rest keys)`

This allows for chaining the accessors:

```lisp
CL-USER> (let ((a '((1 2 3) (4 5 6)))) 
           (getv-chained a 0 2))
3
```

However, unlike `getv`, this does not allow for any type specifiers. (There arises ambiguity with multidimensional array syntax.)

This, too, has `(setf getv-chained)`.

### Reader macros for getv-chained

Example:
```lisp
(named-readtables:in-readtable uniform-utilities:getv-chained)
(defdpar str '("hello"))
[str 0 2] ; => #\l
(setf [str 0 2] #\d)
str ; => ("hedlo")
```

Caution: `in-readtable` does not seem very reliable. For instance, the following does not work 
as expected:
```lisp
(named-readtables:in-readtable uniform-utilities:getv-chained) ; [str 0 2] works
(named-readtables:in-readtable :standard)                      ; does not work
(named-readtables:in-readtable uniform-utilities:getv-chained) ; does not work
```



### defdpar
`(defdpar &rest vars-and-val)`

Example:
```lisp
(flet ((foo () (values '(1 2) 3)))
   (defdpar b c (foo))      ; b is '(1 2), c is 3
   (defdpar (d e) (foo))    ; d is 1, e is 2
   (defdpar f (car (foo)))) ; f is 1
```

This provides a combination of `destructuring` with `defparameter`. However, lambda lists are not supported in the general case - the support is only restricted for `destructuring` coupled with `multiple-values`.

### dlet*
`(dlet* bindings &body body)`

Example:
```lisp
(dlet* (((a c) b (values '(1 3) 2))
        (d e f (values 1 2))
        ((a b) '(4 5))) 
  (list a b c d e f))
;; => (4 5 3 1 2 NIL)
```

## Testing

```lisp
(ql:quickload :uniform-utilities-test)
(prove:run-test-package :uniform-utilities-test)
```
