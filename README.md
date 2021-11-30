# Common Lisp Generators

The `cl-gen` project is a lib that intends to implement generators similar to
Javascript
[generators](https://javascript.plainenglish.io/javascript-lazy-evaluation-generators-examples-included-f9eaa517f969)
in CL. To do that, a small lib of
[continuation](https://courses.cs.washington.edu/courses/cse341/04wi/lectures/15-scheme-continuations.html)
macros based on Paul Graham's [On Lisp]() macros presented on chapter 20, plus
a few tweaks and fixes.

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

For continuations, we have:

For generators, we have:



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
