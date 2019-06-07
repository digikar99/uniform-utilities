
## Background and Introduction

WARNING: This will modify your read-table.

This is yet another utility library for common lisp. (Several libraries can be found at [cliki](https://cliki.net/utilities). Notable ones besides those include  [Alexandria](http://common-lisp.net/project/alexandria/) and [cl21](https://lispcookbook.github.io/cl-cookbook/cl21.html). There's also [a good discussion on reddit about "fixing" common lisp](https://www.reddit.com/r/lisp/comments/6t6fqs/which_sugared_library_do_common_lispers_prefer/).

I don't think it is good enough yet; therefore, I'm using a personalized name - in case someone comes up with a "God" level library, let them use a good name. (Learnt over reddit that one should give a good name, only after it is proven to be good. And it is reasonable: we don't want to waste good names. :p)

Detailed documentation is available on [github pages](https://digikar99.github.io/cl-digikar-utilities/).

The library is still unreleased, and therefore, subject to changes.

## Undocumented


```lisp
    CL-USER> (digikar-utilities:getf-equal '("a" "b" "c" "d") "a")
    "b"

    CL-USER> (digikar-utilities.logic:gen-truth-table (a b c) (and a b c))
    ((T (T T T)) (NIL (T T NIL)) (NIL (T NIL T)) (NIL (T NIL NIL)) (NIL (NIL T T))
     (NIL (NIL T NIL)) (NIL (NIL NIL T)) (NIL (NIL NIL NIL)))
    (A B C)
    
```
