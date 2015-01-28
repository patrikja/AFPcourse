-- Incomplete Agda file intended to type check the correctness of the Parser transformations
module Parsers where

open import Data.List
open import Data.Product
open import Function


* = Set
*1 = Set1

data Parser1 (s : *) : * -> *1 where
  Symbol : Parser1 s s
  Fail   : ∀ {a} -> Parser1 s a
  _+++_ : ∀ {a} -> Parser1 s a -> Parser1 s a -> Parser1 s a
  Return : ∀ {a} -> a -> Parser1 s a
  _>>=_ : ∀ {a b} -> Parser1 s a -> (a -> Parser1 s b) -> Parser1 s b


runParser1 : ∀ {s a} -> Parser1 s a -> List s -> List (a × List s)
runParser1 Symbol [] = []
runParser1 Symbol (x ∷ xs) = [ x , xs ]
runParser1 Fail l = []
runParser1 (y +++ y') l = runParser1 y l ++ runParser1 y' l
runParser1 (Return y) l = [ y , l ] 
runParser1 {s} (_>>=_ {a} {b} y y') l = concatMap inner  (runParser1 y l) 
  where inner : a × List s → List (b × List s)
        inner (x , l) = runParser1 (y' x) l
 
data Parser2 (s : *) (a : *) : *1 where
    SymbolBind : (s -> Parser2 s a) -> (Parser2 s a) -- SymbolBind f == Symbol >>= f
    Return : a -> Parser2 s a
    _+++_ : Parser2 s a -> Parser2 s a -> Parser2 s a
    Fail : Parser2 s a

runParser2 : ∀ {s a} -> Parser2 s a -> List s -> List (a × List s)
runParser2 (SymbolBind y) [] = []
runParser2 (SymbolBind y) (x ∷ xs) = runParser2 (y x) xs -- magic simplification
runParser2 (Return y) l = [ y , l ] 
runParser2 (y +++ y') l = runParser2 y l ++ runParser2 y' l
runParser2 Fail l = []

1to2 : ∀ {s a} -> Parser1 s a -> Parser2 s a
1to2 Symbol = SymbolBind Return 
1to2 Fail = Fail
1to2 (y +++ y') = 1to2 y +++ 1to2 y'
1to2 (Return y) = Return y 
1to2 (Symbol >>= y') = SymbolBind (\c -> 1to2 (y' c)) -- def of SymbolBind
1to2 (Fail >>= y') = Fail -- Parser law. L4.
1to2 ((y +++ y') >>= y0) = 1to2 (y >>= y0) +++ 1to2 (y' >>= y0) -- Parser law. L5
1to2 (Return y >>= y') = 1to2 (y' y) -- monad law, L1
1to2 ((p >>= k') >>= k) = 1to2 (p >>= (\x -> k' x >>= k)) -- monad law, L3



data Parser3 (s : *) (a : *) : *1 where
    SymbolBind : (s -> Parser3 s a) -> (Parser3 s a)
    ReturnChoice : a -> Parser3 s a -> Parser3 s a -- ReturnChoice x p ≡ Return x +++ p
    Fail : Parser3 s a

best : ∀ {s a} -> Parser3 s a -> Parser3 s a -> Parser3 s a
best (SymbolBind y) (SymbolBind y') = SymbolBind (\s -> best (y s) (y' s)) -- L10
best p (ReturnChoice x q) = ReturnChoice x (best p q) -- bag
best (ReturnChoice x q) p  = ReturnChoice x (best p q)
best p Fail = p -- def. of +++
best Fail q = q

2to3 : ∀ {s a} -> Parser2 s a -> Parser3 s a
2to3 (SymbolBind y) = SymbolBind (\s -> 2to3 (y s))
2to3 (Return y) = ReturnChoice y Fail -- def. of returnchoice
2to3 (y +++ y') = best (2to3 y) (2to3 y')
2to3 Fail = Fail 

runParser3 : ∀ {s a} -> Parser3 s a -> List s -> List (a × List s)
runParser3 (SymbolBind y) [] = []
runParser3 (SymbolBind y) (x ∷ xs) = runParser3 (y x) xs
runParser3 (ReturnChoice y y') l = (y , l) ∷ runParser3 y' l
runParser3 Fail l = []

runParser1' : ∀ {s a} -> Parser1 s a -> List s -> List (a × List s)
runParser1' = runParser3 ∘ 2to3 ∘ 1to2

-- CPS transform of 1to2
1to2' : ∀ {s a} -> Parser1 s a -> (∀ {b} -> (a -> Parser2 s b) -> Parser2 s b)
1to2' Symbol k = SymbolBind k
1to2' Fail k = Fail
1to2' (y +++ y') k = 1to2' y k  +++ 1to2' y' k
1to2' (Return y) k = k y
1to2' (p >>= f) k = 1to2' p (\x -> 1to2' (f x) k)

1to2'' : ∀ {s a} -> Parser1 s a -> Parser2 s a
1to2'' p = 1to2' p Return

open import Data.Unit
open import Data.Empty


data _∈_ {a : Set} (x : a) : List a -> Set where
  here : ∀ {xs} -> x ∈ (x ∷ xs)
  there : ∀ {x0 xs} -> x ∈ xs -> x ∈ (x0 ∷ xs)


thm1 : ∀ {a s} {x : a} (p : Parser1 s a) (s ss : List s) -> (x , s) ∈ runParser1 p ss -> (x , s) ∈ runParser2 (1to2 p) ss
thm1 Symbol s [] t = t
thm1 Symbol s (x' ∷ xs) t = t
thm1 Fail s' ss t = t
thm1 (y +++ y') s' ss t = {!!}
thm1 (Return y) s' ss t = t
thm1 (Symbol >>= y') s' ss t = {!!} -- extensionality...
thm1 (Fail >>= y') s' ss t = t
thm1 ((y +++ y') >>= y0) s' ss t = {!!}
thm1 (Return y >>= y') s' ss t = {!!}
thm1 ((y >>= y') >>= y0) s' ss t = {!!}
