Lecture 14, Part 2 : Additional remarks and proofs
--------------------------------------------------

1. We had

    map sum . concat = concat . map (map sum)

This is a result suggested by the type of |concat|.  In general, if
we have a polymorphic function between functorial structures

    poly : F a -> G a

we can count on the following law to hold:

    fmap f . poly = poly . fmap f

for _any_ f.  This property is known as "naturality".

2. One of the several proof obligations we have is

    maximum . concat = maximum . map maximum

We can show this in the direct way by induction.

Case []:

    maximum (concat [])
  =  {defs }
	-inf

	maximum (map maximum [])
  =  {defs }
	-inf

Case (xs : xss):

For the left-hand side, we have

	maximum (concat (xs:xss))
  =  {def. concat}
	maximum (xs ++ concat xss)

For the right-hand side

	maximum (map maximum (xs:xss))
  =  {def. map}
	maximum (maximum xs : map maximum xss)
  =  {def. maximum}
	maximum xs `max` (maximum (map maximum xss))
  =  {induction hyp.}
	maximum xs `max` (maximum (concat xss))

We need to prove something stronger, namely that

	maximum (xs ++ ys) = maximum xs `max` maximum ys

which we can, again, show by induction:

Case []:

	maximum ys = -inf `max` (maximum ys)

Case (x:xs)

    maximum (x : xs ++ ys) 
  =  {def. ++}
	maximum (x : (xs ++ ys))
  =  {def. maximum}
	x `max` (maximum (xs ++ ys))
  =  {induction hyp.}
	x `max` (maximum xs `max` maximum ys)
  =  {associativity of `max`}
	(x `max` (maximum xs)) `max` maximum ys
  =  {def. maximum}
	maximum (x:xs) `max` maximum ys

This is a rather long-winded proof, and we can do better.

3.
Just as so many of the steps in part 1 involved |foldr|, so many of the
proofs we need involve an application of the following result.

Fusion
------

    f . foldr g a = foldr h b
  <=
    f a = b    and
	f (g x y) = h x (f y)

This results holds for *finite* lists (and for infinite ones, if |f| is
strict), and can easily be shown by induction.

We now use the fact that

< concat = foldr (++) []

and

< map f = foldr ((:) . f) []


For the lhs we have:

    maximum . foldr (++) [] = foldr h b
  <=
    b = -inf   and
	maximum (xs ++ ys) = h xs (maximum ys)

	so take h xs y = max (maximum xs) y

For the rhs:

	maximum . foldr ((:) . maximum) [] = foldr h b
  <=
    b = -inf   and
    maximum (maximum xs : ys) = max (maximum xs) (maximum ys)

which is obviously true.

In fact, the rhs fusion can be made more general:

    foldr f a . map g = foldr h b
	foldr f a . foldr ((:) . g) [] = foldr h b
  <=
    b = a   and
	foldr f a ((:) . g x ys) = h x (foldr f a ys)
	foldr f a (g x : ys) = h x (foldr f a ys)
	f (g x) (foldr f a ys) = h x (foldr f a ys)

	so take h = f . g

Thus, we have the general form of fold-map fusion:

    foldr f a . map g = foldr (f . g) a

Applied to map:

    foldr ((:) . f)  [] . map g = foldr ((:) . f . g) [] = map (f . g)

giving us the functoriality of map in one line.

Thus, we can perhaps understand a bit better the following quote from
"Unifying Structured Recursion Schemes" by Hinze et. al.:

	It has long been understood that explicit recursion is the ‘goto’ of
	pure functional programming, and should be considered harmful to
	program comprehension and analysis. 
	
4.
We could have used map fusion also for the first derivation in Part 1.

First we have another immediate application of fold-map fusion:

    maximum . map sum . inits
  =  { fold-map fusion }
    foldr (max . sum) -inf . inits

Then, we can apply again fusion, using the fact that

< inits = foldr f [[]] where f x xss = [] : map (x:) xss

     foldr (max . sum) -inf . foldr f [[]] = foldr h b
  <=  {fusion}
     b = foldr (max . sum) -inf [[]] = 0  and
	 foldr (max . sum) -inf (f xs xss) = h x (foldr (max . sum) -inf xss)  

We have 

	 foldr (max . sum) -inf (f xs xss)
  =   {def. f}
	 foldr (max . sum) -inf ([] : map (x:) xss) 
  =   {def. foldr}
	 (max . sum) [] (foldr (max . sum) - inf (map (x:) xss))
  =   {computing max . sum []}
	 0 `max` (foldr (max . sum) -inf (map (x:) xss))
  =   { fold-map fusion }
	 0 `max` (foldr (max . sum . (x:)) -inf xss)
  =   { artithmetic }
	 0 `max` (x + foldr (max . sum) -inf xss)

	 so take h x y = 0 `max` (x + y)

But this time the direct derivation seems easier.

Exercises
---------

1.  Use fusion to prove the naturality of concat.

2.  Complete all remaining proof obligations.
