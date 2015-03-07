> module MSS where

> import Prelude hiding(maximum)

Calculational Programming
=========================


Introduction
------------

Two quotes from Richard Bird, one of the main proponents of
calculational programming in the context of functional programming:

From the introduction to "Pearls of Functional Algorithm Design" (2010):

  [...] I was interested in the specific task of taking a clear but
  inefficient functional program, a program that acted as a
  specification of the problem in hand, and using equational reasoning
  to calculate a more efficient ones.  One factor that stimulated
  growing interest in functional languages in the 1990s was that such
  languages were good for equational reasoning. Indeed, the functional
  language GOFER, invented by Mark Jones, captured this thought as an
  acronym. GOFER was one of the languages that contributed to the
  development of Haskell, the language on which this book is based.
  Equational reasoning dominates everything in this book.

From chapter 5 in "Thinking Functionally with Haskell" (2014):

  The aim is to write down the simplest and clearest specification
  without regard to how efficient the result might be.  That's a key
  difference between functional programming and other forms of program
  construction: we can always begin with a clear and simple, though
  possibly extremely inefficient definition [...], and then use the laws
  of functional programming to massage the computation into one that
  takes acceptable time and space.

Our aim is to illustrate this kind of development using the "canonical"
example of computing the maximum segment sum.

The maximum segment sum
-----------------------

The history of the problem is given in column 7 of "Programming Pearls"
by Jon Bentley.  His statement of the problem is:

  The problem arose in one-dimensional pattern recognition [...]
  The input is a vector X of N real numbers; the output is the maximum
  sum found in any contiguous subvector of the input.  For instance, if
  the input vector is

       [31, -41, 59, 26, -53, 58, 97, -93, -23, 84]
	             ^                ^
                 3                7

  then the program returns the sum of X[3..7], or 187. The problem is
  easy when all the numbers are positive — the maximum subvector is the
  entire input vector. The rub comes when some of the numbers are
  negative: should we include a negative number in hopes that the
  positive numbers to its sides will compensate for its negative
  contribution? To complete the definition of the problem, we'll say
  that when all inputs are negative the maximum sum subvector is the
  empty vector, which has sum zero.

We'll replace "vector" by "list", "real numbers" by "doubles", and
we'll use zero-based indexing.

Test cases:

> test1 :: [Double]
> test1 = [-1,2,-3,5,-2,1,3,-2,-2,-3,6]  -- From Bird's book

> test2 :: [Double]
> test2 = [31, -41, 59, 26, -53, 58, 97, -93, -23, 84] -- From Bentley

A simpler example:

> test3 :: [Double]
> test3 =  [2, -1, 2]

The specification follows the statement of the problem:

> mssSpec :: [Double] -> Double
> mssSpec = maximum . map sum . segs

To make a list of all possible segments, we can first take all the
tails, and then all the initial segments of the tails:

> segs = concat . map inits . tails

> inits [] = [[]]
> inits (x : xs) = [] : map (x:) (inits xs)

> tails [] = [[]]
> tails (x : xs) = (x : xs) : tails xs

The algorithm takes O(n^3) time, but it's not immediately obvious that
this isn't as good as it gets.

Part 1
------

We'll derive a more efficient algorithm in a direct way, leaving some
proofs for part 2.

We have

    maximum . map sum . segs
  =  { def. segs }
    maximum . map sum . concat . map inits . tails

What can we do now?  The first thing to note is that there's no point
trying to work on the |concat . map inits . tails| bit, because the
computation is almost optimal (we generate too many empty lists, but
otherwise you can't do better).

Similarly, |maximum . map sum|: it does appear we have to compute the
sums before we can take the maximum, so optimizing that doesn't seem to
bring much (we could save a traversal of the list).

The major gains appear to be had in not generating all the segments, so
we'd like to somehow push the "computational" part inside.  For example,
examining the type of |map sum . concat|, we have

    map sum . concat :: [[[Double]]] -> [[Double]]

which suggest the following equation:

    map sum . concat = concat . map (map sum)

which is more or less obvious (more about this in part 2).

Therefore:

    maximum . map sum . concat . map inits . tails
  =  { naturality of concat }
    maximum . concat . map (map sum) . map inits . tails

In our quest to push the computation in, we examine |maximum . concat|.
The type is

    maximum . concat :: [[Double]] -> Double

which suggests

    maximum . concat = maximum . map maximum 
  
but if we use |Prelude.maximum| this doesn't hold.  Consider:

  maximum (concat [[], [1]]) = 1

but

  maximum (map maximum [[], [1]]) = undefined

Let's "fix" |maximum|, since we're only using it for |[Double]| anyway:

> maximum :: [Double] -> Double
> maximum []       =  -1 / 0.0
> maximum (x : xs) = x `max` (maximum xs)


We can now prove that

    maximum . concat = maximum . map maximum 

We defer that for part 2.

Up to now we have:

    maximum . concat . map (map sum) . map inits . tails
  =  { maximum . concat = maximum . map maximum }
    maximum . map maximum . map (map sum) . map inits . tails

There is now a very obvious thing to do, which is to fuse the
computation of the three consecutive |map|s.  This should hold; after
all, preservation of composition is a law of |fmap|, and |fmap| for
lists is just |map|.  Still, we should check, and we'll do that in part
2.  For now, we record that

    maximum . map maximum . map (map sum) . map inits . tails
  =  { functor }
    maximum . map (maximum . map sum . inits) . tails

The computation inside the outermost map is O(n^2).  We will now attempt
a direct derivation of a more efficient version.

Derivation 1
------------
    (maximum . map sum . inits) []
  =  { definitions }
    0

    (maximum . map sum . inits) (x : xs)
  =  { def inits }
    maximum (map sum ([] : map (x:) (inits xs)))
  =  { def map }
    maximum (sum [] : map sum (map (x:) (inits xs)))
  =  { def sum }
    maximum (0 : map sum (map (x:) (inits xs)))
  =  { functor }
    maximum  (0 : map (sum . (x:)) (inits xs))
  =  { sum . (x:) = (x + ) . sum }
    maximum . (0 : map ((x+) . sum) (inits xs))
  =  { def. maximum }
    0 `max` (maximum (map ((x+) . sum)) (inits xs))
  =  { + distributes through max }
    0 `max` (x + maximum (map sum (inits xs)))

Therefore

    maximum . map sum . inits = foldr f 0
	   where f x n = 0 `max` (x + n)

thus reducing a O(n^2) computation to linear time.

    maximum . map (maximum . map sum . inits) . tails
  =  { derivation 1 }
    maximum . map (foldr f 0) . tails

We still have an O(n^2) computation because of the mapping of a
|sum|-like function to |tails|.  We attempt another direct attack:

Derivation 2
------------

    map (foldr f 0) (tails [])
  =  
    [0]

	map (foldr f 0) (tails (x : xs))
  =  { def. tails }
    map (foldr f 0)  ((x : head (tails xs)) : (tails xs))
  =  { def. map }
    foldr f 0 (x : head (tails xs)) : map (foldr f 0) (tails xs)
  =  { def. foldr }
    f x (foldr f 0 (head (tails xs))) : map (foldr f 0) (tails xs)
  =  { foldr f 0 (head (tails xs)) = head (map (foldr f 0) (tails xs) }
    g x (map (foldr f 0) (tails xs)) 
	  where g x yss = f x (head ys) : yss

Therefore

    map (foldr f 0) . tails = foldr g [0]

Notice that we haven't used any properties of |f| or |0|, so in fact we
have just shown a more general result:

    map (foldr f e) . tails = foldr g [e] 
	    where g x yss = f x (head ys) : ys

We have therefore also reduced this computation to O(n).

    maximum . map (foldr f 0) . tails
  =  { proof 2}
    maximum . foldr g [0]

We now have a linear time computation for the maximum segment sum!

> mss =  maximum . foldr g [0]
>    where
>    g x (y:ys) = 0 `max` (x + y) : (y : ys)


In fact, we can even get rid of the extra traversal.  However, when we
try to write |mss| as a fold, we have

    mss [] = 0

    mss (x : xs)
  =  {def. mss}
    (maximum . foldr g [0]) (x : xs)
  =  {def. foldr}
    maximum (g x (foldr g [0] xs))
  =  {def. g, using f x y = 0 `max` (x + y)}
    maximum (f x (head (foldr g [0] xs)) : (foldr g [0] xs))
  =  {def. maximum}
    f x (head (foldr g [0] xs)) `max` (maximum (foldr g [0] xs))
  =  {def. mss}
    f x (head (foldr g [0] xs)) `max` (mss xs)
     

For |mss| to be  a fold, we would need to find a function |h| such that

    h x (mss xs) = f x (head (foldr g [0] xs)) `max` (mss xs)

but it is clear that no such function can exist.  The information about
the head of the list constructed by |foldr g [0] xs|, essential in
computing the next candidate segment sum, is not available in the
arguments of |h|.  Only the next element and the best segment sum
discovered so far are available there.

The solution is to preserve this information.  Thus, instead of the
computation of

    mss [x1, x2, x3]

proceeding by succesively constructing

    [0]
    (0 `max` x3) : [0]
    (0  `max` (x2 + 0 `max` x3)) : (0 `max` x3) : [0]
    (0 `max` (x1 + 0 `max` (0 `max` (x2 + 0 `max` x3)) : (0  `max` (x2 + 0 `max` x3)) : (0 `max` x3) : [0]

and then taking the maximum of this list, we would instead successively
compute pairs

    (y0, b0), the initial situation: y0 = 0 (the head of [0]) and 
	                                 b0 = 0 (the "best" so far)
	(y1, b1) where y1 = f x3 y0 = 0 `max` (x3 + 0) 
	               b1 = y1 `max` b0 (check if the new value is "better")
	(y2, b2) where y2 = f x2 y1 = 0`max` (x2 + (0 `max` (x3 + 0)))
	               b2 = y2 `max` b1 (check if the new value is "better")
	(y3, b3) where y3 = f x3 y2 = 0 `max` (x2 + (0`max` (x2 + (0 `max` (x3 + 0)))))
	               b3 = y3 `max` b2 (check if the new value is "better")
  
To get the maximum segment sum, we need to take the second component of
the pair.  Thus, we have:

> mss2 :: [Double] -> Double
> mss2 = snd . foldr h (0, 0)
>  where
>  h x (y, b) = (y', y' `max` b)
>               where y' = 0 `max` (x + y)

Exercises
---------

1.  Derive a linear-time algorithm from the specification

    mssSpec = maximum . map sum . concat . map tails . inits

2.  In general, it is a good idea to do the exercises in the respective
    chapters of Bird's books (see references).


References
----------

1. "Introduction to Functional Programming Using Haskell, 2nd. Ed.",
   Richard Bird, 1998. (MSS is in Chapter 4)
2. "Pearls of Functional Algorithm Design", Richard Bird, 2010.
3. "Thinking Functionally with Haskell", Richard Bird, 2014. (MSS is in
   Chapter 6)
4. "The maximum segment sum problem", David Gries.  In "Formal
   Development of Programs and Proofs", Dijkstra (ed.), 1990.
5. "Programming Pearls", Jon Bentley, 1986 (2nd. Ed. 2000).
