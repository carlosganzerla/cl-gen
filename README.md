# Common Lisp Generators

The `cl-gen` project is a lib that intends to implement generators similar to
Javascript
[generators](https://javascript.plainenglish.io/javascript-lazy-evaluation-generators-examples-included-f9eaa517f969)
in CL. Currently this project is experimental.

## Usage

**It's possible that this doc is outdated because this is an early stage
project. I suggest that you look into it just to get the idea. If you want
better documentation, see the [examples file](examples.lisp). Proper
documentation and tests will be done some time**

The idea of generators is basically (although not limited to) to create lazy
sequences (like `IEnumerable`/`Seq` in .NET). To do that there are basically
three building blocks: The `yield`, `next` and `stop` functions. The
`generator-bind` and `generator-collect` macros were devised for convenience of
usage.

The `yield` function is a copy of Javascript `yield`. It yields back the
control to the caller (no pun intended). The `generator-bind` macro creates a
context for binding variables to generator yielded values, and to establish a
generator context. The `stop` function will terminate the generator context,
returning the supplied value. The `next` function, on the other hand, will skip
the current iteration, and will supply a value to the return value of `yield`.

Let's see an example:

```lisp
(defun numbers ()
  (do ((x 0 (1+ x)))
      (nil)
      (yield x)))

(generator-bind (x) ((numbers))
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
  (generator-bind (x) ((numbers))
    (when (evenp x)
      (yield x))))

(generator-bind (x) ((even-numbers))
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

An iteration must be skipped explicitly. Also, the results of an iteration
may be collected automatically (unless the iteration is skipped):

```lisp
(defun tree-generator (tree)
  (mapcar (lambda (x)
            (if (consp x)
                (tree-generator x)
                (yield x)))
          tree))

(defun find-tree (pred tree)
  (generator-collect (e) (tree-generator tree)
    (if (funcall pred e)
        e
        (next))))
```

```lisp
CL-USER> (find-tree #'evenp '(1 2 3 (5 4 0 3 (1 2) (1 (4 5) 2 9) 6) (1 3 2)))
(2 4 0 2 4 2 6 2)
```

Here the `next` function will skip the current iteration. The
`generator-collect` macro will collect all completed iteration results on a
list and return it on the last iteration.

Check [the examples file](src/examples.lisp) for more examples. Currently no
unit tests exist.

## Next steps

Recently I've been trying to make the `yield` and `next` function exactly like
Javascript. It turns out this is kinda hard to do on Common Lisp. If this was
Scheme I could use continuations. In CL, continuations are possible using some
macros, but I didn't find it very convenient as it is in Scheme, so I decided
to leave it and instead improve the current functionality. Instead of asking
for values, the calling code may stop the iteration completely or just skip to
the next value. If the iteration is skipped, the user may supply the value that
will be returned in the `yield` function (this one is very similar to Js).

I think the next move would be create some robust examples (a more complex
program) to check the viability. If it seems good, I'll add proper tests and
documentation. The generator binds themselves should not slow by default
because they simply are lambda calls on the same lexical scope (no stack
unwinding required). Proper profiling will be done if the viability condition
is satisfied.

Feel free to open PRs, issues or contact me.
