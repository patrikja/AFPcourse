module Compiler.Behaviour(Trace(..),(+++),approx,cut,crashed) where

infixr 2 :>

data Trace a
  = Step (Trace a)
  | a :> Trace a
  | End
  | Crash
  | Cut
  deriving (Eq, Show)

(+++) :: Trace a -> Trace a -> Trace a
Step s   +++ t = Step (s +++ t)
(x :> s) +++ t = x :> (s +++ t)
End      +++ t = t
Crash    +++ t = Crash
Cut      +++ _ = Cut

cut :: Integer -> Trace a -> Trace a
cut  0 _ = Cut
cut  n (a :> s) = a :> cut (n - 1) s
cut  n (Step s) = Step $ cut (n - 1) s
cut _n Crash    = Crash
cut _n End      = End
cut _n Cut      = Cut

approx :: Eq a => Integer -> Trace a -> Trace a -> Bool
approx n s t = cut n s == cut n t

crashed :: Trace a -> Bool
crashed Crash     = True
crashed End       = False
crashed Cut       = False
crashed (Step s)  = crashed s
crashed (_ :> s)  = crashed s
