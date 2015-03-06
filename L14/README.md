# Program derivation: Maximum segment sum (guest lecture)

One of the most important advantages of functional programming
languages is that they allow us to construct programs from
specifications by *calculation*. The expressivity of functional
programming enables the formulation of specifications as (usually very
inefficient) programs, which can then be optimized by equational
reasoning using algebraic identities, in a process reminiscent of
high-school mathematics. In this lecture, we will introduce a number
of such algebraic identities, and use them in the presentation of a
"canonical" example of program calculation: computing a linear-time
solution to the maximum segment sum problem.

Micro-bio: Cezar Ionescu is currently a PostDoc on "Increasingly
Correct Scientific Computing" in the FP group at Chalmers. Before that
he worked for several years at the Potsdam Institute for Climate
Impact Research where he applied AFP in the form of Haskell, C++, Agda
and Idris to Computational Vulnarability Assessment, Scientific
Computing and Economic Models.

Reading:
* Chapter 6 of "Thinking Functionally with Haskell", Richard Bird, Cambridge University Press, 2014
* Chapter 4 of "Introduction to Functional Programming using Haskell second edition", Richard Bird, Prentice Hall, 1998
* "Algebraic Identities for Program Calculation", Richard Bird, The Computer Journal, Vol. 32, No. 2, 1989, pp 122-126

For reference:
* An Agda-derivation of the Maximum Segment Sum is available here
** https://bitbucket.org/scmlab/aopa/src/17652ccd5c661cd0a338531bbc1720d4329e56dd/Examples/MSS/Derivation.agda?at=master
