Schala
======

## The experimental programming language meta-interpreter

Schala is a Rust-language framework written to make it easy to
create and experiment with toy programming languages. It provides
a common REPL, and a typeclass `ProgrammingLanguage` with methods
for tokenizing text, parsing tokens, evaluating an abstract syntax tree,
and other tasks that are common to all programming languages.

Schala started out life as an experiment in writing a Javascript-like
programming language that would never encounter any kind of runtime
value error, but rather always return `null` under any kind of error
condition. I had seen one too many Javascript `Uncaught TypeError:
Cannot read property ___ of undefined` messages, and I was a bit frustrated.
Plus I had always wanted to write a programming langauge from scratch,
and Rust is a fun language to program in.

Over time I became interested in playing around with other sorts
of programming languages as well, and wanted to make the process
as general as possible. I changed the name of the project to
Schala, after the Princess of Zeal from *Chrono Trigger*, because I
like classic JRPGs and because it sounds sort of like Scala, and I am
continuing to work on the project as my time permits.

### Reference works

Here's a partial list of resources I've made use of in the process
of learning how to write a programming language.

#### Evaluation
*Understanding Computation*, Tom Stuart, O'Reilly 2013

#### Parsing
http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
[Crafting Interpreters](http://www.craftinginterpreters.com/)

#### LLVM
http://blog.ulysse.io/2016/07/03/llvm-getting-started.html
