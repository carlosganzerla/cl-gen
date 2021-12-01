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
Generators are also very useful for creating lazy sequence (like
`IEnumerable`/`Seq` in .NET). In Common Lisp, since there's no (AFAIK) built-in
function to access the whole call stack, implementing generators is rather
tricky. Continuations make this possible. Since they are not built-in in CL as
they are in Scheme, I created a small lib based on PG's book, updated to fix
some stuff that may cause trouble. The generator macros were based on the
continuation macros to allow pause/resuming of execution and provide a similar
experience.

## Usage

It's not essential to understand continuations to use this lib. Since their
implementation is only a mean to an end, I'll let the source document itself.
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

## Next steps

Previously, this lib implemented a version that didn't have the pause
functionality. It provided a nice API to decouple functions, but didn't
introduce nothing relevant and all it did was some boilerplate reduction since
everything it did could be done by simple functional programming without much
fuss (see the [`old`]() branch). I resisted to use continuations for some time
but basically the functionality can't be reproduced without it.

The next logical step would be to improve the API. For that it depends really
on the use cases. The core macros may be paired with recursive functions that
don't fit in the existing iteration macros and new abstractions may arise from
this. I'll be trying to use this lib for other stuff and adding new features
along the way should they be generic enough.

Feel free to open PRs, issues or contact me.
