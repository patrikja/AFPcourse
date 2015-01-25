{-# LANGUAGE GADTs #-}
{-|
  A simple embedded language for input/output. Intermediate emb.
-}
module Program.Deep2 where
import Control.Monad       ((>=>))
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

type Input   =  String
type Output  =  String

{-

It is often good to move away a bit from the pure deep embedding
towards some kind of "normal form". In our case we can start by
looking at how Put and Get can be used. The only combinator in our
language is Bind (>>=) so lets looks at the different cases for the
first argument to Bind. 

  Put >>= f

From the types of Put and Bind we note that f must have type 

  () -> m a

which is basically just a value of type (m a). Another way to think
about it is that Put does not really return any useful value (the
actual "putting" is implemented as a "side-effect"). So the function
after bind can ignore its argument. The operator "then":

  (>>) :: Monad m => m a -> m a -> m a

can be used 

  Put c >>= \_ -> p   ==   Put c >> p

We will now give a name to this new combination 

  PutThen c p  ==  Put c >> p

This is a Program which starts by printing c and the behaves like p.

In a similar way we can introduce a new name for the combination of
Get and Bind:

  GetBind f  ==  Get >>= f

The third combination would be ReturnBind

  ReturnBind x f  ==  Return x >>= f

but the first monad law already tells us that this is just (f x) so no
new constructor is needed for this combination.

Finally we would have the fourth combination (of Bind and Bind) but
that can also be simplified away (as we can see below). At this point
we will just define the new datatype, hoping that we can do without
Bind.

-}

data Program a where
  PutThen :: Char -> Program a         -> Program a
  GetBind :: (Maybe Char -> Program a) -> Program a
  Return  :: a                         -> Program a

-- | It turns out that bind can still be defined!
instance Monad Program where
  return  = Return
  (>>=)   = bindP  

-- | Bind takes the first argument apart:
bindP :: Program a -> (a -> Program b) -> Program b
bindP (PutThen c p)  k   =  PutThen c (bindP p k)
bindP (GetBind f)    k   =  GetBind (\x -> bindP (f x) k)
bindP (Return x)     k   =  k x

-- Alt. 
-- bindP (GetBind f)    k   =  GetBind (f >=> k)

{-
  bindP (Return x)    k 
= Def. >>=
  (Return x) >>= k 
=  Law 1.  return x >>= f   ==  f x
  k x
-}

{-
  bindP (GetBind f)   k
= Def. of (>>=)
  (GetBind f) >>=  k
= Def. GetBind  
  (Get >>= f) >>=  k
= Law 3.  (m >>= f) >>= g  ==  m >>= (\x -> f x >>= g)
  with m = Get, f = f, g = k
  Get >>= (\x -> f x >>= k)
= Def. GetBind  
  GetBind (\x -> f x >>= k)
= Def. of (>>=)
  GetBind (\x -> bindP (f x) k)
-}
{-
  bindP (PutThen c p) k
= { Def. of (>>=) }
  (PutThen c p) >>= k
= { Def. of PutThen }
  (Put c >> p) >>= k
=   
  (Put c >>= \_ -> p) >>= k
= Law3 with m = Put c, f = \_->p, g = k
  Put c >>= (\x -> (\_->p) x >>= k)
= simplify  
  Put c >>= (\_ -> p >>= k)
= Def. of >>
  Put c >> (p >>= k)
= Def. of PutThen  
  PutThen c (p >>= k)
-}

--    Law 3.  (m >>= f) >>= g  ==  m >>= (\x -> f x >>= g)

{- We can *calculate* the correct definition of bindP using
   the follwing intuitive meaning of PutThen and GetBind

    @PutThen c p == putC c >> p@
    @GetBind f   == getC >>= f@

 and the monad laws:

    Law 1.  return x >>= f   ==  f x

    Law 2.  m >>= return     ==  m

    Law 3.  (m >>= f) >>= g  ==  m >>= (\x -> f x >>= g)
    Law 3'. (f >=> g) >=> h  ==  f >=> (g >=> h)
      -- Basically associativity of bind

   For instance,

      GetBind f >>= k             { meaning of GetBind }
  ==  (getC >>= f) >>= k          { third monad law }
  ==  getC >>= (\c -> f c >>= k)  { meaning of GetBind }
  ==  GetBind (\c -> f c >>= k)
  ==  GetBind (f >=> k)

-}

-- > (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
-- > f >=> g   =   \a ->  f a >>= g

-- Law 3'. (f >=> g) >=> h  ==  f >=> (g >=> h)
-- Basically associativity of bind

-- | Output a character.
putC :: Char -> Program ()
putC c = PutThen c $ Return ()

-- | Input a character.
getC :: Program (Maybe Char)
getC = GetBind Return


type IOSem a = Input -> (a, Input, Output)
-- | The run function is easier than before
run :: Program a -> IOSem a
run (PutThen c p)  i        =  (x,  i',  c : o)
  where (x, i', o)  =  run p i
run (GetBind f)    []       =  run (f Nothing)   []
run (GetBind f)    (c : i)  =  run (f $ Just c)  i
run (Return x)     i        =  (x,  i,  [])


--------
-- Preparing for the Functor-Applicative-Monad proposal:
--   https://www.haskell.org/haskellwiki/Functor-Applicative-Monad_Proposal

-- | The following instances are valid for _all_ monads:
instance Functor Program where
  fmap = liftM
  
instance Applicative Program where
  pure   = return
  (<*>)  = ap

