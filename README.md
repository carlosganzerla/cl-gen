# Common Lisp generators

This project (attempts to) implements generators (like Javascript
[generators](https://javascript.plainenglish.io/javascript-lazy-evaluation-generators-examples-included-f9eaa517f969))
in CL. Currently this project is experimental.

## Usage

The idea of generators is basically (although not limited to) to create lazy
sequences (like `IEnumerable`/`Seq` in .NET). To do that there are basically
three building blocks: The `yield` and `stop` functions, and the
`generator-bind` macro.

The `yield` function is a copy of Javascript `yield`. It yields back the
control to the caller (no pun intended). The `generator-bind` macro creates a
context for binding variables to generator yielded values, and to establish a
generator context. The `stop` function will terminate the generator context,
returning the supplied value:

```lisp
(defun numbers ()
  (do ((x 0 (1+ x)))
      (nil)
      (yield x)))

(generator-bind (x (numbers))
  (format t "Yielded ~A~%" x)
  (when (= 100 x)
    (stop 'stahp)))
```

Contrary to JS, which callers ask for values, in this implementation callers
retrieve values until `stop` is called.

The results would be like:

```lisp
Yielded 0
Yielded 1
...
Yielded 100
STAHP
```

Generators can be nested also:

```lisp
(defun even-numbers ()
  (generator-bind (x (numbers))
    (when (evenp x)
      (yield x))))

(generator-bind (x (even-numbers))
    (print x)
    (stop-when (>= x 100) 'ok))
```

```lisp
0
2
4
...
100
```

In this case, we created an new generator `even-numbers` which consumes from
`numbers`. The last block consumes from the former, printing only even numbers.

## Todo

To make the usage more coveneient than of Javascript, it'd be nice to have a
`next` function and another type of context which yields values only when
requested explicitly. Also, more tests have to be made and if the idea seems
good enough some profiling and optimizations will be done as well.
