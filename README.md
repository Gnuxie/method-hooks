# Method Hooks

method-hooks provides simple hooks dispatched by methods and supports method combination and qualifiers.

**documentation can be viewed [here](https://gnuxie.gitlab.io/method-hooks/)**
## Motivation

A friend thought that methods qualified with `progn` with the same type specializer lists would accumulate to run like hooks,

Which could be quite useful so here we are.

## Features

 * create hooks with `defhook`
 * set the default qualifier to use for hooks in a generic with `define-hook-generic`.
 * change how hooks dispatch with `finalize-dispatch-method` or add behaviour to the dispatch method, which will enable the use of `call-next-method`.
* create a dispatcher for a new method-combination type with `define-dispatch`, or to change the behaviour of hook dispatch for an existing qualifier.
* `set-dispatch-for-qualifier` to set the default dispatcher to use for a given qualifier
* `dispatch` to dispatch hooks for a specific method, (useful within `finalize-dispatch-method`).
* suppression of no-applicable-method error to get extensible hook points by passing (:hook-point t) to define-hook-generic.

## Usage

### Getting started

You can jump straight into defining hooks, they will by default (where define-hook-generic hasn't been used for the generic you're using) be unqualified just like normal methods.

```
(ql:quickload :method-hooks)

(defgeneric foo (a))

(method-hooks:defhook foo user ((a string))
  (print a))
  
(method-hooks:defhook foo hey :before ((a string))
  (print "hey"))
  
> (foo "me")
"hey" 
"me" 
```

#### Using define-hook-generic with defhook

If you want to `defhook` to remember what qualifier to use for a generic, you can use `define-hook-generic` which takes all the same options as `defgeneric` and optionally takes `:default-qualifier` which by default will be the method-combination type supplied. If no method combination type has been supplied then by default `define-hook-generic` will use `progn` as the default qualifier & combination.

```
(define-hook-generic baz ()) ; defaults to progn for least astonishment
(defhook baz meow () (print "meow"))
(defhook baz woof () (print "woof"))

> (baz)

"woof" 
"meow" 
(WOOF MEOW)
```

Here is an example where we will use the `+` combination-type to show that `defhook` by default will use the combination-type supplied as the default qualifier.

```
(define-hook-generic adding (x) ; remembers the method combination type and uses that as default.
  (:method-combination +))      ; can be overridden with (:default-qualifier :unqualified) (or another combination type)
  
(defhook adding once ((x integer)) x)
(defhook adding twice ((x integer)) x)

> (adding 3)

6
```

The keyword `:unqualified` can be supplied as a qualifier to `defhook`, `define-hook-generic`, `set-dispatch-for-qualifier` and anything else exported by this system as this is how unqualified methods are distinguished internally, however this will likely never be unnecessary.

### defining a new dispatcher

There are some dispatchers already defined in [/src/known-dispatchers.lisp](https://gitlab.com/Gnuxie/method-hooks/blob/master/src/known-dispatchers.lisp) and these are good examples to go by.

### Using finalize-dispatch-method

`defhook` expands with a definition for the dispatch method, this is so that each dispatch-method doesn't have to be finalized.
If you wanted to edit the dispatch method you can and you can do anything you want in there, you must however dispatch the hooks yourself with `dispatch` or by hand.

```
(finalize-dispatch-method adding ((x integer))
  (format t "dispatching hooks")
  (dispatch adding + ((x integer))))
```

#### dispatching without a dispatcher

To dispatch by hand you would as of writing have to understand internals. The definition for `dispatch` shows you how to do this, I think I will make this easier if there is demand in future.

### what are specialized-lambda-list, vanilla-lambda-list, type-list, descriptive-lambda-list?

using method-hooks::destructure-specialized-lambda list will give you a good idea e.g. 

```
> (destructure-specialized-lambda-list descriptive-lambda-list vanilla-lambda-list type-list '((x integer) y)
    (values descriptive-lambda-list type-list vanilla-lambda-list))
((X INTEGER) (Y T))
(INTEGER T)
(X Y)
```

It's also important to note that hooks are interned by the gf-name and **type-list** not the specialized-lambda-list as the variable symbols in the specialized-lambda-list can change.

### More (and more practical) examples

Here is a [version](https://gitlab.com/Gnuxie/cl-matrix/blob/47758656a3df54d15ce41da92a5c90f42d02fdf9/src/base-events.lisp) of cl-matrix where event listening is done with method-hooks.

Here is a [version](https://gitlab.com/Gnuxie/cl-matrix/blob/master/src/base-events.lisp) where I badly abuse deeds for comparison.
