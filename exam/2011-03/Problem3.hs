{-# LANGUAGE GADTs #-}
module Problem2 where
import Control.Monad
import Test.QuickCheck

data Parser tok a where
  Zero     ::                                   Parser tok  ()
  One      ::                                   Parser tok  ()
  Check    ::  (tok   -> Bool) ->               Parser tok  tok
  Satisfy  ::  ([tok] -> Bool) ->               Parser tok  [tok]
  Push     ::  tok -> Parser tok a ->           Parser tok  a
  Plus     ::  Parser tok a -> Parser tok b ->  Parser tok  (Either a b)
  Times    ::  Parser tok a -> Parser tok b ->  Parser tok  (a, b)
  Star     ::  Parser tok a ->                  Parser tok  [a]
    
parse :: MonadPlus m => Parser tok a -> [tok] -> m a
 
-- |Zero| always fails.
parse Zero ts = mzero
 
-- |One| matches only the empty string.
parse One []  =  return ()
parse One _   =  mzero
 
-- |Check p| matches a string with exactly one token |t| such that |p t| holds.
parse (Check p) [t]  =  if p t then return t else mzero
parse (Check p) _    =  mzero
 
-- |Satisfy p| matches any string such that |p ts| holds.
parse (Satisfy p) xs = if p xs then return xs else mzero
 
-- |Push t p| matches a string |ts| when |p| matches |(t:ts)|.
parse (Push t p) ts = parse p (t:ts)
 
-- |Plus p q| matches when either |p| or |q| does.
parse (Plus p q) ts =  liftM Left   (parse p ts) `mplus` 
                       liftM Right  (parse q ts)
 
----------------------------------------------------------------
-- Solution to 3a)

-- |Times p q| matches the concatenation of |p| and |q|.
parse (Times p q) ts = parseTimes p q ts
 
-- |Star p| matches zero or more copies of |p|.
parse (Star p) ts = parseStar p ts

parseStar :: MonadPlus m => Parser tok a -> [tok] -> m [a]
parseStar p []      = return []
parseStar p (t:ts)  = do
    (v,vs) <- parse (Times p (Star p)) (t:ts)
    return (v:vs)
    
parseTimes :: MonadPlus m =>
  Parser tok a -> Parser tok b -> [tok] -> m (a, b)
parseTimes p q []      = liftM2 (,) (parse p []) (parse q [])
parseTimes p q (t:ts)  = 
    parse (Times (Push t p) q) ts  `mplus`
    liftM2 (,) (parse p []) (parse q (t:ts))
 
----------------------------------------------------------------
-- Solution to 3b)
    
newtype P m tok a = P {runP :: [tok] -> m a}
 
zero     :: MonadPlus m =>                                   P m tok  ()
one      :: MonadPlus m =>                                   P m tok  ()
check    :: MonadPlus m =>  (tok   -> Bool) ->               P m tok  tok
satisfy  :: MonadPlus m =>  ([tok] -> Bool) ->               P m tok  [tok]
plus     :: MonadPlus m =>  P m tok a -> P m tok b ->  P m tok  (Either a b)
zero    = P (const mzero)
one     = P (\xs -> if null xs then return () else mzero)
check   p = P (\xs -> let n = length xs; x = head xs 
                      in if n==1 && p x then return x else mzero)
satisfy p = P (\xs -> if p xs then return xs else mzero)
plus    (P p) (P q) = P (\xs -> liftM Left (p xs) `mplus` liftM Right (q xs))

----------------------------------------------------------------
-- Below is some testing code and some variant solutions copied from
-- students (not guaranteed to work).

-- Examples: (not part of the exam question)

-- Looping test:
test :: Maybe [()]
test = parse (Star One) "I really must get to the bottom of this..."

token x = Check (x ==)
string xs = Satisfy (xs ==)
 
p   =  Times (token 'a') (token 'b')
p1  =  Times (Star (token 'a')) (Star (token 'b'))
p2  =  Star p1
 
blocks :: (Eq tok) => Parser tok [[tok]]
blocks = Star (Satisfy allEqual)
    where allEqual xs = and (zipWith (==) xs (drop 1 xs))
 
evenOdd = Plus (Star (Times (Check even) (Check odd)))
               (Star (Times (Check odd) (Check even)))
          

----------------
-- a very rudimentary test suite:
test1 xs = parse p xs  ==  if xs == "ab" then Just ('a','b') else Nothing
test2 xs = if null rest 
           then label "Just" $ Just (as, bs) == parse p1 xs
           else label "triv" $ Nothing == parse p1 xs
    where (as, bs') = (takeWhile ('a'==) xs, dropWhile ('a'==) xs)
          (bs, rest)= (takeWhile ('b'==) xs, dropWhile ('b'==) xs)

-- This test depends on the choice / order of matches
test3 = parse p2 "aaabbbbaabbbbbbbaaabbabab" ==  
        Just [("aaa","bbbb"),("aa","bbbbbbb"),("aaa","bb"),("a","b"),("a","b")]
-- This test depends on the choice / order of matches
test4 = parse blocks "aaaabbbbbbbbcccccddd" ==
        Just ["aaaa","bbbbbbbb","ccccc","ddd"]
test5 = parse evenOdd [0..9] ==
        Just (Left [(0,1),(2,3),(4,5),(6,7),(8,9)])
test6 = parse evenOdd [1..10] ==
        Just (Right [(1,2),(3,4),(5,6),(7,8),(9,10)])

main = do quickCheck test1
          quickCheck test2
          print $ test3 && test4 && test5 && test6
          
mconcat :: MonadPlus m => [m a] -> m a
mconcat = foldr mplus mzero

parseTimes2 :: MonadPlus m => 
  Parser tok a -> Parser tok b -> [tok] -> m (a, b)
parseTimes2 p q ts = mconcat as
  where as = [ liftM2 (,) (parse p pre) (parse q suf)
             | (pre,suf) <- preAndSuf ts
             ]

preAndSuf :: [a] -> [([a],[a])]
preAndSuf xs = [ splitAt n xs | n <- [0..length xs] ]

parseTimes3 :: MonadPlus m => 
  Parser tok a -> Parser tok b -> [tok] -> m (a, b)
parseTimes3 p q ts = mconcat [ do a <- parse p (take i ts)
                                  b <- parse q (drop i ts)
                                  return (a, b)
                             | i <- [0..length ts] ]


parseStar2 :: MonadPlus m => Parser tok a -> [tok] -> m [a]
parseStar2 p ts = 
    (if null ts then return [] else mzero) `mplus` 
    liftM (uncurry (:)) (parse (Times p (Star p)) ts)

parseStar3 :: MonadPlus m => Parser tok a -> [tok] -> m [a]
parseStar3 p [] = return []
parseStar3 p ts = mconcat [ do a  <- parse p        (take i ts)
                               as <- parse (Star p) (drop i ts)
                               return (a:as)
                          | i <- [1..length ts] ]


test7 xs = parseTimes3 p q xs == (parseTimes p q xs :: Maybe (Char, Char))
  where p = Check ('o'<)
        q = Check ('o'>=)

test8 xs = parseStar p xs == (parseStar3 p xs :: Maybe String)
  where p = Check ('o'<)

