
# TODO Items

- Next priorities: - get ADTs working, get matches working

- inclusive/exclusive range syntax like .. vs ..=

- sketch of an idea for the REPL:
        -each compiler pass should be a (procedural?) macro like
        compiler_pass!("parse", dataproducts: ["ast", "parse_tree"], {
         match parsing::parse(INPUT) {
           Ok(
                PASS.add_artifact(
        }

-should have an Idris-like `cast To From` function

- REPL:
  - want to be able to do things like `:doc Identifier`, and have the language load up these definitions to the REPL


* change 'trait' to 'interface'
  -think about idris-related ideas of multiple implementations of a type for an interface (+ vs * impl for monoids, for preorder/inorder/postorder for Foldable)

* Share state between programming languages

* idea for Schala - scoped types - be able to define a quick enum type scoped to a function ro something, that only is meant to be used as a quick bespoke interface between two other things

* another idea, allow:
type enum {
  type enum MySubVariant {
    SubVariant1, SubVariant2, etc.
    }
 Variant1(MySubVariant),
 Variant2(...),
 }



* idea for Schala: both currying *and* default arguments!
        ex. fn a(b: Int, c:Int, d:Int = 1) -> Int
            a(1,2) : Int
            a(1,2,d=2): Int
            a(_,1,3) : Int -> Int
            a(1,2, c=_): Int -> Int
            a(_,_,_) : Int -> Int -> Int -> Int



- AST : maybe replace the Expression type with "Ascription(TypeName, Box<Expression>) nodes??
- parser: add a "debug" field to the Parser struct for all debug-related things

-scala-style html"dfasfsadf${}" string interpolations!

*Compiler passes architecture

-ProgrammingLanguageInterface defines a evaluate_in_repl() and evaluate_no_repl() functions
-these take in a vec of CompilerPasses

struct CompilerPass {
        name: String,
        run: fn(PrevPass) -> NextPass
}

-change "Type...." names in parser.rs to "Anno..." for non-collision with names in typechecking.rs

-get rid of code pertaining to compilation specifically, have a more generation notion of "execution type"
