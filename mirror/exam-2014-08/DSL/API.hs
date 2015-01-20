module DSL.API where
-- DSL: design embedded domain specific languages
--     DSL.Concepts: (abstract) syntax, semantics, ...
--     DSL.Implement: implement EDSLs in Haskell (as combinator libraries) 

-- DSL: design an embedded domain specific language
{-

This assignment is about design and implementation of an embedded
language for ``ASCII art''. The language should be compositional, that
is, enable building complex images by combining simpler images. 
Here is one example of what the language should be able to express (but
you need not implement the rendering).

+-+   +--------+
|1|   |+---+   |
|7|   ||hi!|   |
|3|   |+---+   |
|8|   |        |
+-+   |+------+|
      ||Patrik||
      |+------+|
      +--------+

Typical components are: horizontal text, vertical text, framed boxes,
relative placement (above, beside, etc.).

-}
-- --------------------------------------------------------------
{-
  Design an API for the above embedded language. This should consist
  of: suitable names of types, names and type of operations for
  constructing, combining and ``running''. For each operation,
  describe briefly what it is supposed to do.  Keep the role of each
  operation as simple as possible but add enough combinators to allow
  describing the picture above. (Note that this part does not ask for
  any implementation code, only names and type signatures.)

  Implement the example above in terms of your API.
-}

example :: Art
example = b1 + space + frame (b2 / space / b3)
  where
    b1 = frame (vertical (show 1738)) 
    b2 = frame (horizontal "hi!")
    b3 = frame (horizontal "Patrik")
    space = horizontal "   "
    (+) = beside
    (/) = above

data Art = Dummy -- for the first part
  deriving Eq
data Dir = Hori | Vert
frame :: Art -> Art
text :: Dir -> String -> Art
beside :: Art -> Art -> Art
above  :: Art -> Art -> Art

vertical    :: String -> Art
horizontal  :: String -> Art

-- Some run functions:
-- render the ASCII Art as a String
render :: Art -> String 
-- compute size
size :: Art -> (Int, Int)

-- Just for type checking
frame   = error "TBD"
text    = error "TBD"
beside  = error "TBD"
above   = error "TBD"
render  = error "TBD"
size    = error "TBD"

-- --------------------------------------------------------------
{-
  Which of the operations in your API are primitive and which are
  derived? Give definitions of the derived operations in terms of the
  primitive operations.

-}

----------------
-- Derived operations
vertical    = text Vert
horizontal  = text Hori

-- Some possible derived operations:
box   :: Dir -> String -> Art
box d s = frame (text d s)

combine :: Dir -> Art -> Art -> Art
combine Hori = beside
combine Vert = above

