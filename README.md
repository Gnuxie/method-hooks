# Method Hooks

method-hooks provides simple hooks dispatched by methods and supports method combination and qualifiers.

## Motivation
A friend thought that methods qualified with `progn` with the same type specializer lists would accumulate to run like hooks.

My friend's mistake is now reality!
## Usage

```
(ql:quickload :method-hooks)

(defgeneric foo (a))

(method-hooks:defhook foo user ((a string)) ()
  (print a))
  
(method-hooks:defhook foo hey ((a string)) (:before)
  (print "hey"))
  
(foo "me")

===>
"hey" 
"me" 
```

The macro `finalize-dispatch-method` can be used to add a body to a dispatch method, which would mean you could make use of `call-next-method`.
