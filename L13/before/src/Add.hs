{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Add where
-- http://www.haskell.org/haskellwiki/Simonpj/Talk:FunWithTypeFuns

{- Addition in the Num type class has type 
  (Num a) => a -> a -> a

This allows any numeric type to be used for addition _as long as
both arguments have the same type_. In math it is common to
allow addition of, for example, integers and real
numbers. Defining a more flexible addition is a nice little
excerices in associated types (type families).
-}

class Add a b where           -- uses MultiParamTypeClasses
  type AddTy a b              -- uses TypeFamilies as well
  add :: a -> b -> AddTy a b

-- instance Add Integer Double where ... 
-- instance Add Double Integer where ... 
-- instance (Num a) => Add a a where ... -- uses FlexibleInstances
-- instance (Add Integer a) => 
--           Add Integer [a] where ...   -- uses FlexibleContexts


-- --------------
{-   
test :: Double
test = add (3::Integer) (4::Double)

aList :: [Integer]
aList =  [0,6,2,7]

test2 :: [Integer]
test2 = add (1::Integer) aList

test3 :: [Double]
test3 = add (38::Integer) [1700::Double]

-}
-- What is the type of this function?
test4 x y z = add x (add y z)
-- spoiler below








test4 :: (Add b c, Add a (AddTy b c)) => 
  a -> b -> c -> AddTy a (AddTy b c)
