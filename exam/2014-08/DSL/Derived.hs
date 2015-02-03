module DSL.Derived (module DSLImpl, module DSL.Derived) where
import DSL.Shallow as DSLImpl
-- import DSL.Deep as DSLImpl


-- Some possible derived operations:
box   :: Dir -> String -> Art
box d s = frame (text d s)

combine :: Dir -> Art -> Art -> Art
combine Hori = beside
combine Vert = above

vertical    :: String -> Art
vertical    = text Vert
horizontal  :: String -> Art
horizontal  = text Hori


aboveL :: Art -> Int -> Art -> Art
aboveL a n b = (a `above` line n Vert) `above` b

aboveS :: Art -> Int -> Art -> Art
aboveS a n b = (a `above` space (n, 1)) `above` b

besideL :: Art -> Int -> Art -> Art
besideL a n b = (a `beside` line n Hori) `beside` b

besideS :: Art -> Int -> Art -> Art
besideS a n b = (a `beside` space (1, n)) `beside` b


----------------
-- Derived run functions:

render :: Art -> ArtStrs
size   :: Art -> ArtSize

render  = snd . run
size    = fst . run
