#AFP 2014: Annotated learning outcomes
(Not yet updated for 2015 lecture numbering.)

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
* L03: Simple I/O-library
* L04: Parsing concepts (syntax, semantics, combinators)
* L05: Semantics of programming languages
* L10: deep & shallow embedding
* A01: recognising concepts from the course

###DSL.Implement: implement EDSLs in Haskell (as combinator libraries)

* L04: Parsing (as an application of DSLs and Monads)
* L05: Implementing interpreters
* L07: QuickCheck - two DSL's: for properties & for generators
* L10: Designing EDSLs
* L13: Derivatives and Interval artithmetic
* A01: EDSL for Turtle graphics
* A02: EDSL for CGI scripts

##Types: read, understand and extend Haskell programs which use advanced type system features

* L01: recap of Haskell
* L02: compositionality and abstraction
* L08: Discussing modelling and implementation
* A03: explore a Hackage library and report on the results

###Types.Class: type classes, newtypes, deriving, ...

* L01: type classes
* L05: (multi-parameter) type classes
* L10: heavy use of type classes and instances
* L11: design patterns: newtypes, type classes, Show, Read and QuickCheck
* L13: Num class, Haskell for Scientific Computing

###Types.GADT: (gen.) algebraic datatypes & type fam.

* L03: early example of GADTs (Program a)
* L10: use of GADTs and type families
* L11: GADTs and Type families
* L12: Datatypes and families (similar to GADT's)

###Types.HOT: functors, monads and monad transformers

* L03: higher-order types
* L03: Monads
* L04: higher order functions, parser monad
* L05: higher order functions
* L05: Monad repetition
* L05: Transformers: ReaderT, ...
* L06: Transformers: StateT, ErrorT, ...
* L10: use of type families

##Spec: use specification based development techniques

###Spec.Test: formulate and test properties

* L03: Monad laws
* L07: Specification
* L07: Testing (QuickCheck - properties & generators)
* L09: Shrinking of test cases
* L09: Program coverage (the tool hpc)
* L09: Writing QuickCheck generators for a larger program (Compiler example)

###Spec.Prove: reason about correctness of functional programs

* L07: Proving properties of functional programs
* L11: programming with strong invariants
* L12: Curry-Howard-correspondance (program : type) ~= (proof : theorem)
* L12: Agda - a dependently typed language similar to Haskell
* L13: Example: Interval arithmetic

###Spec.Trans: transform programs on the basis of reasoning

* L04: From a simple deep embedding towards an efficient implementation.
* L05: Extending an interpreter step by step
* L06: Extending an interpreter step by step
* L06: Stepwise refinement based on laws
* L09: Monomorphizing the Parser datatype (for testing)
* A01: Extensions to the Turtle language
* A02: Part II: Task 2: Optimising the Replay monad

##Expl: explain and discuss the above topics

* L*: Several lectures contained interactive sessions
* L08: Explicit focus on group solving of exam questions
* L13': Explicit focus on group solving of exam questions
* A0*: Working in pairs -> discussion
* A01: Thoughts and reflections on the Turtle implementation
* A03: Write about the library you chose.
