
TODO:
-null-only language should be called Maaru
-haskell-ish langauge should be called Robo
-typeful scripting language should be called schala
rename accordingly!

# Schala - a programming language meta-interpreter

Schala is a Rust-language framework written to make it easy to
create and experiment with toy programming languages. It provides
a common REPL, and a trait `ProgrammingLanguage` with methods
for tokenizing text, parsing tokens, evaluating an abstract syntax tree,
and other tasks that are common to all programming languages.

Schala started out life as an experiment in writing a Javascript-like
programming language that would never encounter any kind of runtime value
error, but rather always return `null` under any kind of error condition. I had
seen one too many Javascript `Uncaught TypeError: Cannot read property ___ of
undefined` messages, and I was a bit frustrated.  Plus I had always wanted to
write a programming langauge from scratch, and Rust is a fun language to
program in.  Over time I became interested in playing around with other sorts
of programming languages as well, and wanted to make the process as general as
possible.

The name of the project comes from Schala the Princess of Zeal from the 1995
SNES RPG *Chrono Trigger*. I like classic JRPGs and enjoyed the thought of
creating a language name confusingly close to Scala. The naming scheme for
languages implemented with the Schala meta-interpreter is Chrono Trigger
characters.

## Languages implemented using the meta-interpreter

* The eponymous *Schala* language is an interpreted/compiled scripting langauge,
designed to be relatively simple, but with a reasonably sophisticated type
system.

* *Maaru* was the original Schala (since renamed to free up the name *Schala*
  for the above language), a very simple dynamically-typed scripting language
  such that all possible runtime errors result in null rather than program
  failure.

* *Robo* is an experiment in creating a lazy, functional, strongly-typed language
much like Haskell

## Reference works

Here's a partial list of resources I've made use of in the process
of learning how to write a programming language.

### Evaluation
*Understanding Computation*, Tom Stuart, O'Reilly 2013
*Basics of Compiler Design*, Torben Mogensen

### Parsing
http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/

[Crafting Interpreters](http://www.craftinginterpreters.com/)

### LLVM
http://blog.ulysse.io/2016/07/03/llvm-getting-started.html

###Rust resources
https://thefullsnack.com/en/rust-for-the-web.html
https://rocket.rs/guide/getting-started/
