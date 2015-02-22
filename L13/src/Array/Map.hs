----------------------------------------------------------------
{-
  mapArr   :: (a->a) -> Array a -> Array a
  mapArr   = mapArrInt
mapArrInt :: (Int -> Int) -> Array Int -> Array Int
mapArrInt f (ArrInt is) = ArrInt (map f is)

  mapArr   = undefined -- mapArrPair

mapArrPair ::
  (ArrayElem b) => (b -> b) -> Array (b, b) -> Array (b, b)
mapArrPair f (ArrPair (as, bs)) = ArrPair (mapArr f as, mapArr f bs)

-}