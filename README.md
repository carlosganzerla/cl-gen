# Common Lisp Generators

The `cl-gen` project is a lib that intends to implement generators similar to
Javascript
[generators](https://javascript.plainenglish.io/javascript-lazy-evaluation-generators-examples-included-f9eaa517f969)
in CL. To do that, a small lib of
[continuation](https://courses.cs.washington.edu/courses/cse341/04wi/lectures/15-scheme-continuations.html)
macros based on Paul Graham's [On Lisp](http://www.paulgraham.com/onlisp.html)
macros presented on chapter 20, plus a few tweaks and fixes.

## Idea

Generators are special in that they may execute a block of code lazily, and
pause the execution of the block after calling `yield`. The generator may be
called again and the code execution will resume from the previous point.
Generators are also very useful for creating lazy sequences (like
`IEnumerable`/`Seq` in .NET). In Common Lisp, since there's no (AFAIK) built-in
function to access the whole call stack, implementing generators is rather
tricky. Continuations make this possible. Since they are not built-in in CL as
they are in Scheme, I created a small lib based on PG's book, updated to fix
some stuff that may cause trouble. The generator macros were based on the
continuation macros to allow pause/resuming of execution and provide a similar
experience to JS.

## Usage

It's not essential to understand continuations to use this lib. Since their
implementation is only a means to an end, I'll let the source document itself.
There's this
[article](https://ashok-khanna.medium.com/continuations-in-common-lisp-1911cb413a03)
that walks through the macros presented on PG's book. A particular difference
is that the continuations implemented here use exclusively a well defined
lexical context, so every code that uses continuation must be called within a
`cc-context` macro. Just put it on the main function and the macros will do the
rest.

On generators we have two core macros: `yield-bind` and `next-bind`. They are
parallel to `yield` and `next` on JS respectively. Using continuations we may
also establish the bindings for the values that `yield` and `next` would
receive. `yield-bind` basically evaluates and yields the result of a form. It
returns a `generator` structure that contains the next function call (the
body after the `yield`). The `defgen` is a `defuncc` (`defun` with
continuations enabled) with a top `yield-bind` to
simulate the lazy behavior of `function*`. For example if we define the
following generator and call it:

```lisp
(defgen generator ()
  (yield-bind () "Lorem"
    (yield-bind () "Ipsum"
      (yield-bind () "Dolor"))))
```

```shell
EXAMPLES> (cc-context (generator))
#S(CL-GEN::GENERATOR
   :CALL #<FUNCTION (LAMBDA (&OPTIONAL &REST #:G0) :IN %GENERATOR) {5361A13B}>)
NIL
CL-USER>
```

We can see that two values were returned. The first value is always the
generator structure. The other values are the rest of the values returned from
the function body. Suppose we cheat and call the internal function `%next`
directly:

```shell
EXAMPLES> (cl-gen::%next *)
#S(CL-GEN::GENERATOR
   :CALL #<FUNCTION (LAMBDA (&OPTIONAL &REST #:G2)
                      :IN
                      EXAMPLES::%GENERATOR) {5361A19B}>)
"Lorem"
EXAMPLES>
```

The first yielded value was returned (just as we've called the first `next`) on
JS. The first value again is a generator structure, but this time with the call
corresponding to the next `yield-bind`. Generator structures are immutable and
do not exhaust like in JS.

The `*` means the last returned value on my REPL (I use SLIMV).

If we repeat the process thrice:

```shell
EXAMPLES> (cl-gen::%next *)
#S(CL-GEN::GENERATOR
   :CALL #<FUNCTION (LAMBDA (&OPTIONAL &REST #:G4) :IN %GENERATOR) {5361A1FB}>)
"Ipsum"
EXAMPLES> (cl-gen::%next *)
#S(CL-GEN::GENERATOR
   :CALL #<FUNCTION (LAMBDA (&OPTIONAL &REST #:G6) :IN %GENERATOR) {5361A25B}>)
"Dolor"
EXAMPLES> (cl-gen::%next *)
NIL
```

When there are no more calls a single `NIL` is returned.

We can see that we have the equivalent as this:

```javascript
function* generator() {
  yield "Lorem";
  yield "Ipsum";
  yield "Amet";
}

const gen = generator();
console.log(gen.next());
console.log(gen.next());
console.log(gen.next());
console.log(gen.next());
```

And the result is somewhat similar:

```javascript
{ value: 'Lorem', done: false }
{ value: 'Ipsum', done: false }
{ value: 'Amet', done: false }
{ value: undefined, done: true }
```

Of course `%next` is not exported, so you should not use it. To consume a
generator, we use the `next-bind` macro:

```lisp
(cc-context
  (let ((gen (generator)))
    (next-bind (x) (gen)
      (print x)
      (next-bind (y) (gen)
        (print y)
        (next-bind (z) (gen)
          (print z))))))
```

```shell
"Lorem" 
"Ipsum" 
"Dolor" 
"Dolor"
EXAMPLES> 
```

The last `"Dolor"` was returned because the last form is returned, just like
any CL code block. More simple usage examples are on the [examples
file](src/examples.lisp).

### Iteration

Commonly, generators are used on iteration and sequence generation. For a more
realistic and common usage, some utility macros were designed:
`generator-bind`, `generator-loop` and `do-yield`.

`do-yield` has the same semantics of `do`. The difference is that the body is
yielded. Take a sequence generator for instance:

```lisp
(defgen generate-seq (&key (init 0) (end nil) (step 1))
  (do-yield ((x init (+ x step)))
            ((and end (>= x end)))
            x))
```

Consuming it we have

```shell
SEQUENCES> (cc-context (generate-seq))
(%next cl-gen::%next *)
...
0
(%next cl-gen::%next *)
...
1
(%next cl-gen::%next *)
...
2
```

We can consume the generator automatically using `generator-loop`, which will
iterate a generator until exhaustion:

```lisp
(cc-context 
  (generator-loop (x) (generate-seq :end 5)
    (print x)))
```

```shell
0 
1 
2 
3 
4 
NIL
```

Another macro useful for iteration `generator-bind`. The user may decide wether
to call the next iteration or skip it.

```lisp
(defgen even-seq (seq)
  (generator-bind (x) seq
    (if (evenp x)
      (yield x)
      (next))))

(cc-context
  (generator-loop (x) (even-seq (generate-seq :end 10))
    (print x)))
```

```shell
2 
4 
6 
8 
NIL
```

`yield` and `next` are local functions only valid on the context established by
`generator-bind`. Next skips the iteration, while `yield` yields the value and
will resume on the next iteration after the generator is called again. `yield`
is a tail recursive call on the context of `generator-bind`, so it should be
called as a tail recursive function would to avoid weird behavior or stack
overflows.

A caveat is that these iteration macros are built on top of tail-recursive
functions. See the constraints section for more details.

There's a good example of usage of these macros and generator combination on
the [sequences example file](src/sequences.lisp).

### Constraints

The lib provides similar experince to JS, with some constraints. The first
being that you must use a continuation context to use them. Using it on the
top-level should suffice but the overhead that it may cause to larger programs
it is yet to be discovered.

The second constraint is that generator forwarding
or consumption must be tail recursive in general. For a small number of
possible iterations it won't really matter, but this scenario isn't really
applicable to generators that are used to simulate infinite sequences.

The third is the absence of some additional capabilities of JS generators as
unwindings and throwing exceptions from the outside. I didn't try to implement
these, but they may be feasible. The current objective is to implement a
minimal and functional interface with an easy (or at least not very hard) to use
API.

## Next steps

Previously, this lib implementation didn't have the pause functionality. It
provided a nice API to decouple functions, but didn't introduce nothing
relevant and all it did was some boilerplate reduction since everything it did
could be done by simple functional programming without much fuss (see the
[`old`](https://github.com/carlosganzerla/cl-gen/tree/old) branch).
I resisted to use continuations for some time but basically
the functionality can't be reproduced without it AFAIK.

The next step IMO would be to improve the API. The direcitons for it depends
really on the use cases. The core macros may be paired with recursive functions
that don't fit in the existing iteration macros and new abstractions may arise
from this. I'll be trying to use this lib for other stuff and adding new
features along the way should they be generic enough.

I'll add tests and docstrings soon.

Feel free to open PRs, issues or contact me.
