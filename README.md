# uniform-utilities

The library aims to provide a uniform way of getting things done in Common Lisp. Unlike its alpha-variant (digikar-utiliites), this does not modify the readtable, and has a single goal.

Towards this, it defines:

- getv
- getv-chained
- defdpar

and borrows from [metabang-bind](https://common-lisp.net/project/metabang-bind/user-guide.html) and [iterate](https://digikar99.github.io/cl-iterate-docs/). (Both `metabang-bind` and `iterate` are available on quicklisp.)

There also exists [access](https://github.com/AccelerationNet/access) which might be considered more uniform that `getv` below; however, it has a significant runtime overhead (factor of 10) and type checking seems to be at issue.

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

However, unlike `getv`, this does not allow for any type specifiers. (There's arises ambiguity with multidimensional array syntax.)

This, too, has `(setf getv-chained)`.

### defdpar
`(defdpar &rest things)`

```lisp
(flet ((foo () (values '(1 2) 3)))
   (defdpar 
     (:values b c) (foo)
     (d e) (foo)
     f (car (foo))))
```

This provides a combination of `destructuring` with `defparameter`. However, lambda lists are not supported in the general case - the support is only restricted for `destructuring` coupled with `multiple-values`.

