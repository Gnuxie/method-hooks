# Method Hooks

method-hooks provides simple hooks dispatched by methods and supports method combination and qualifiers.

## Motivation
A friend thought that methods qualified with `progn` with the same type specializer lists would accumulate to run like hooks,

Which could be quite useful so here we are.

## Features

* `define-hook-generic` to set the default qualifier to use on hook-dispatchers
* `defhook` to create hooks
* `finalize-dispatch-method` to determine what happens in the method that will dispatch the hooks.
* `dispatch` to dispatch the hooks within finalize-dispatch-methods
* `define-dispatch` to create a dispatcher for a new method-combination type
* `set-dispatch-for-qualifier` to set the default dispatcher to use for a given qualifier

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
(WOOF BAR)
```

```
(define-hook-generic adding (x) ; remembers the method combination type and uses that as default.
  (:method-combination +))      ; can be overriden with (:default-qualifier :unqualified) (or another combination type)
  
(defhook adding once ((x integer)) x)
(defhook adding twice ((x integer)) x)

> (adding 3)

6
```


The macro `finalize-dispatch-method` can be used to add a body to a dispatch method, which would mean you could make use of `call-next-method`.
