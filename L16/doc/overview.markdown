#AFP 2015: Annotated learning outcomes

* DSL: design embedded domain specific languages
    * DSL.Concepts: (abstract) syntax, semantics, ...
    * DSL.Implement: implement EDSLs in Haskell (as combinator libraries)
* Types: read, understand and extend Haskell programs which use advanced type system features
    * Types.Class: type classes, newtypes, deriving, ...
    * Types.GADT: (generalised) algebraic datatypes & type families
    * Types.HOT: functors, monads and monad transformers
* Spec: use specification based development techniques
    * Spec.Test: formulate and test properties about the program
    * Spec.Prove: reason about correctness of functional programs
    * Spec.Trans: transform programs on the basis of reasoning
* Expl: explain and discuss the above topics

#Relating course parts to learning outcomes

Notation: Lnn = Lecture #nn, Amm = Assignment #mm.

##DSL: design embedded domain specific languages
###DSL.Concepts: (abstract) syntax, semantics, ...

* L01: laziness, referential transparency
* L02: deep and shallow embedding
* L02: first DSELs: constructors, run functions etc.
* L03/L04: Simple I/O-library
* L04/L05: Parsing concepts (syntax, semantics, combinators)
* L05/L06: Semantics of programming languages
* L09: deep & shallow embedding
* A01: recognising concepts from the course

###DSL.Implement: implement EDSLs in Haskell (as combinator libraries)

* L04/L05: Parsing (as an application of DSLs and Monads)
* L05/L06: Implementing interpreters
* L08: QuickCheck - two DSL's: for properties & for generators
* L09: Designing EDSLs
* A01: EDSL for Turtle graphics
* A02: EDSL for CGI scripts

##Types: read, understand and extend Haskell programs which use advanced type system features

* L01: recap of Haskell
* L02: compositionality and abstraction
* L07: Discussing modelling and implementation
* A03: explore a Hackage library and report on the results

###Types.Class: type classes, newtypes, deriving, ...

* L01: type classes
* L05/L06: (multi-parameter) type classes
* L09: heavy use of type classes and instances
* L11/L13: design patterns: newtypes, type classes, Show, Read and QuickCheck

###Types.GADT: (gen.) algebraic datatypes & type fam.

* L03/L04: early example of GADTs (Program a)
* L09: use of GADTs and type families
* L11/L13: GADTs and Type families
* L12: Datatypes and families (similar to GADT's)

###Types.HOT: functors, monads and monad transformers

* L03/L04: higher-order types
* L03/L04: Monads
* L04/L05: higher order functions, parser monad
* L05/L06: higher order functions
* L05/L06: Monad repetition
* L05/L06: Monad Transformers: ReaderT, ...
* L06: Transformers: StateT, ErrorT, ...
* L09: use of type families

##Spec: use specification based development techniques

###Spec.Test: formulate and test properties

* L03/L04: Monad laws
* L08: Specification
* L08: Testing (QuickCheck - properties & generators)
* L10: Shrinking of test cases
* L10: Program coverage (the tool hpc)
* L10: Writing QuickCheck generators for a larger program (Compiler example)

###Spec.Prove: reason about correctness of functional programs

* L08: Proving properties of functional programs
* L11/L13: programming with strong invariants
* L12: Curry-Howard-correspondance (program : type) ~= (proof : theorem)
* L12: Agda - a dependently typed language similar to Haskell

###Spec.Trans: transform programs on the basis of reasoning

* L03: Stepwise refinement based on laws (Program a)
* L04/L05: From a simple deep embedding towards an efficient implementation.
* L05/L06: Extending an interpreter step by step
* L06: Stepwise refinement based on laws
* L10: Monomorphizing the Parser datatype (for testing)
* L14: Program transformation guest lecture
* A01: Extensions to the Turtle language
* A02: Part II: Task 2: Optimising the Replay monad

##Expl: explain and discuss the above topics

* L*: Several lectures contained interactive sessions
* L07: Explicit focus on group solving of exam questions
* L15: Explicit focus on group solving of exam questions
* A0*: Working in pairs -> discussion
* A01: Thoughts and reflections on the Turtle implementation
* A03: Write about the library you chose.
