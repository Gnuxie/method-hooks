# Method Hooks

method-hooks provides simple hooks dispatched by methods and supports method combination and qualifiers.

**documentation can be viewed [here](https://gnuxie.gitlab.io/method-hooks/)**
## Motivation
A friend thought that methods qualified with `progn` with the same type specializer lists would accumulate to run like hooks,

Which could be quite useful so here we are.

## Features

 * create hooks with `defhook`
 * set the default qualifier to use for hooks in a generic with `define-hook-generic`.
 * change how hooks dispatch with `finalize-dispatch-method` or add behavoir to the dispatch method, which will enable the use of `call-next-method`.
* create a dispatcher for a new method-combination type with `define-dispatch`, or to change the behavoir of hook dispatch for an existsing qualifier.
* `set-dispatch-for-qualifier` to set the default dispatcher to use for a given qualifier
* `dispatch` to dispatch hooks for a specific method, (useful within `finalize-dispatch-method`).

## Usage

```
(ql:quickload :method-hooks)

(defgeneric foo (a))

(method-hooks:defhook foo user ((a string))
  (print a))
  
(method-hooks:defhook foo hey :before ((a string))
  (print "hey"))
  
(foo "me")

===>
"hey" 
"me" 
```

```
(define-hook-generic baz ()) ; defaults to progn for least astonishment
(defhook baz meow () (print "meow"))
(defhook baz woof () (print "woof"))

> (baz)

"woof" 
"meow" 
(WOOF MEOW)
```

```
(define-hook-generic adding (x) ; remembers the method combination type and uses that as default.
  (:method-combination +))      ; can be overriden with (:default-qualifier :unqualified) (or another combination type)
  
(defhook adding once ((x integer)) x)
(defhook adding twice ((x integer)) x)

> (adding 3)

6
```
