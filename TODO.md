
# TODO Items

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
