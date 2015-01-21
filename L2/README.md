# Lecture 2: (Embedded) Domain Specific Languages

## Learning outcomes for L2

This lecture we will focus on the following subset of the full [AFP
course learning outcomes](http://www.cse.chalmers.se/edu/course/afp/index.html#goal):

* DSL: design embedded domain specific languages
    * DSL.Concepts: (abstract) syntax, semantics, ...
    * DSL.Implement: implement EDSLs in Haskell (as combinator libraries)
* Types: read, understand and extend Haskell programs which use advanced type system features
    * Types.Class: type classes, newtypes, deriving, ...
* Expl: explain and discuss the above topics

## Anatomy of an EDSL

* *Types* modelling concepts in the domain: Time, Signal
* *Constructor functions* constructing elements of these types: constS, timeS, ...
* *Combinators* combining and modifying elements: ($$), mapS
* *Run functions* making observations of the elements: sample

With examples expanded:

* *Types* modelling concepts in the domain
```Haskell
type Time      = Double
type Signal a  = Time -> a
```
* *Constructor functions* constructing elements of these types
```Haskell
constS ::  a -> Signal a
timeS  ::       Signal Time
```
* *Combinators* combining and modifying elements
```Haskell
($$) :: Signal (a->b) -> Signal a -> Signal b
mapS ::        (a->b) -> Signal a -> Signal b
```
* *Run functions* making observations of the elements
```Haskell
sample :: Signal a -> (Time -> a)
```

## Primitive and derived operations

* A primitive operation is defined exploiting the definitions of the involved types
```Haskell
timeS :: Signal Time
timeS = \t -> t
```

* A derived operation can be defined purely in terms of other operations
```Haskell
mapS :: (a -> b) -> Signal a -> Signal b
mapS f xs = constS f $$ xs
```

## Think about

* Compositionality
    * Combining elements into more complex ones should be easy and natural
* Abstraction
    * The user shouldn't have to know (or be allowed to explot) the underlying implementation of your types
    * Changing the implementation shouldn't break user code

### Small exercise

Suppose we didn't have ($$) in our Signal language. How would you define 

```Haskell
addS x y = constS (+) $$ x $$ y
```
Answer: awkwardly!

```Haskell
addS x y = mapS (\t -> sample x t + sample y t) timeS
```

## Implementation of an EDSL

* Shallow embedding
    * Represent elements by their semantics (what observations they support)
* Deep embedding
    * Constructor functions and combinators do most of the work, run functions for free
* Or something in-between

## Shallow embedding of Signals

[Look at the Shallow.hs](src/Signal/Shallow.hs)

## Deep embedding of Signals

[Look at the Deep.hs](src/Signal/Deep.hs)



##

```Haskell
```
```Haskell
```
```Haskell
```
```Haskell
```
```Haskell
```
