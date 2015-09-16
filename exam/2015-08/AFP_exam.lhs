% -*- latex -*-
\documentclass[twoside]{article}
\usepackage{a4wide}
\usepackage[utf8]{inputenc}
\usepackage{url}
\usepackage{multicol}
\usepackage{algpseudocode}
\setlength{\parindent}{0ex}
\setlength{\parskip}{1ex plus 1 ex minus 0.5ex}
%\setlength{\parskip}{0em plus 1em minus 0.2em}

% the `doubleequals' macro is due to Jeremy Gibbons
\def\doubleequals{\mathrel{\unitlength 0.01em
  \begin{picture}(78,40)
    \put(7,34){\line(1,0){25}} \put(45,34){\line(1,0){25}}
    \put(7,14){\line(1,0){25}} \put(45,14){\line(1,0){25}}
  \end{picture}}}

\newcommand\todo[1]{\textbf{TODO\{}#1\textbf{\}}}

%include lhs2TeX.fmt
%include forall.fmt
%format == = "\doubleequals"
\title{Advanced Functional Programming TDA342/DIT260}
\author{Anton Ekblad and Patrik Jansson}
\date{2015-08-24}

\newcounter{problem}
\newcounter{subproblem}
\renewcommand\theproblem{\arabic{problem}}
\renewcommand\thesubproblem{(\alph{subproblem})}
\renewcommand\labelenumi{}

\newcommand\nextproblem{\addtocounter{problem}{1}\setcounter{subproblem}{0}}

\newcommand\problem[2]{
  \nextproblem

%  \clearpage
  {
    \rule{\textwidth}{0.3mm}\vskip1mm
    {\bf {\large Problem \theproblem: #1}\marginpar{\textbf{(#2 p)}}}
 %   \rule[1mm]\textwidth{0.3mm}
  }
%  \vskip5mm
  \par
}

\newenvironment{subproblem}[1]{
  \addtocounter{subproblem}{1}
  {%{~}\par\hrule
   \vskip1mm
    \bf \thesubproblem%    \hfill (#1 p) \par
    \marginpar{(#1 p)}%
  }%
%  \begin{enumerate}
%    \item%
}{
%  \vskip2mm\hrule
%  \end{enumerate}
}


\begin{document}
  \maketitle
  \begin{tabular}{p{0.15\textwidth}p{0.8\textwidth}}
    {\bf Contact:} & Anton Ekblad (070 7579 070), Patrik Jansson (031-7725415). \\
    {}\\
    {\bf Result:}  & Announced no later than 2015-09-09 \\
    {}\\

    {\bf Exam check:} & Th 2015-09-10 and Fr 2015-09-11. Both at 12.45-13.10 in EDIT 5468.  \\
    {}\\
    {\bf Aids:} & You may bring up to two pages (on one A4 sheet of paper) of
pre-written notes - a ``summary sheet''. These notes may be typed or
handwritten. They may be from any source.  If this summary sheet is
brought to the exam it must also be handed in with the exam (so make a copy if you want to keep it). \\
    {}\\
    {\bf Grades:} & Chalmers: 3: 24p,~~4: 36p,~~5: 48p,~~max: 60p \\
                  & GU:       G: 24p,~~VG: 48p \\
                  & PhD student: 36p to pass\\
  \multicolumn{2}{l}{} \\
  {}\\
    {\bf Remember:}
      & Write legibly. \\
      & Don't write on the back of the paper. \\
      & Start each problem on a new sheet of paper. \\
      & Hand in the summary sheet (if you brought one) with the exam solutions. \\
  \end{tabular} \\

\newpage

\problem{DSL: design an embedded domain specific language}{20}

%format not = "\Varid{not}"
%format /= = "\not\doubleequals"

In the TV show \emph{Robot Wars}, robots fight each other to death using
various close-range weapons. For this task, you are going to design and
implement a DSL for expressing strategies for those robots. For simplicity,
we assume that robot battles are fought on a flat surface, free of obstacles.

An example of such a strategy:

\begin{algorithmic}
  \While {enemies alive}
    \If {robot in line of sight}
      \If {right in front of robot}
        \State Attack!
        \State Move backward by 0.5 metres.
      \Else
        \State Move forward by 0.1 metres.
      \EndIf
    \Else  
      \State Turn left by 2 degrees.
    \EndIf  
  \EndWhile
\end{algorithmic}

\begin{subproblem}{5}
  Design an API for expressing Robot Wars strategies such as the one above.
  Your API should consist of suitable names of types, as well as names and
  types of functions used to construct and combine strategies.
  For each function, give a short description of its purpose.
  Ensure that your DSL is powerful enough to express the above strategy.

  Use your API to implement the above strategy.
\end{subproblem}

\begin{subproblem}{3}
  State which of the operations in your API are derived and which are primitive.
  Give definitions of the derived operations in terms of the primitive ones.
  Try to minimize the number of primitive operations.
\end{subproblem}

\begin{subproblem}{3}
  State three laws about how the functions of your API relate to each other.
\end{subproblem}

\begin{subproblem}{3}
  Give a deep embedding of the primitive operations in your API.
  Give a type definition and describe what each of the type's constructors
  are supposed to do.
\end{subproblem}

\begin{subproblem}{6}
  You have been given the following low level robot control library:

  \begin{code}
  -- Gives the distance, in metres, to the nearest object in the robot's line of sight.
  -- Returns Nothing if no object is spotted.  
    opticalSensor :: IO (Maybe Double)

  -- Wait the specified number of seconds before resuming execution.
    wait :: Double -> IO ()

  -- Instantly accelerate robot to x metres per second; negative values moves backward.
    setSpeed :: Double -> IO ()

  -- Turn left by the given number of degrees.
    turn :: Double -> IO ()

  -- Gives the total number of live robots left in the arena.
    liveRobots :: IO Int

  -- Attack whatever is in front of the robot.
    attack :: IO ()
  \end{code}

  Use the above library to implement a ``run'' function for your deep embedding,
  and give the type of your run function.
\end{subproblem}

\newpage

\problem{Types: read, understand and extend Haskell programs which use
  advanced type system features}{20}

\emph{Stringly typed} programming is a practice where strings are used to hold
non-string data - numbers or enumeration values, for instance - and functions
operate over these strings rather than over more strictly defined data types.
This practice is common in shell scripts but, thankfully,
not in the functional programming world.

For this task, you are going to implement code to convert functions over an
arbitrary number of |Int|s to stringly typed functions, and back again.
That is, a function |string| which converts an $n$-ary function
|f :: Int -> ... -> Int| into an $n$-ary function
|f' :: String -> ... -> String|, and a function |unstring| which
is the inverse of |string|.

The following code gives a type class |Stringly| which uses associated
types to accomplish this:

\begin{code}
  class Stringly f where
    type Str f
    string   :: f -> Str f
    unstring :: Str f -> f
\end{code}

Additionally, here are some examples and invariants to explain how the type
class is used:

\begin{code}  
  add :: Int -> Int -> Int
  add = (+)
    
  stringly_add :: String -> String -> String
  stringly_add = string add

  unstringly_add :: Int -> Int -> Int
  unstringly_add = unstring stringly_add

  prop_inverse :: (Int -> Int) -> Int -> Bool
  prop_inverse = \f x -> f x == unstring (string f) x

  prop_preserves_semantics :: (Int -> Int) -> Int -> Bool
  prop_preserves_semantics = \f x -> show (f x) == string f (show x)
\end{code}

\begin{subproblem}{10}
  Using associated types, give the |Stringly| instances required to
  implement |string| and |unstring| as specified above.

  \emph{Hint: you should need at most two instances.}
\end{subproblem}

\begin{subproblem}{10}
  In many cases, multiparameter type classes can be used to solve the same
  problems as associated types. The following code gives a type class to provide
  the same functionality as |Stringly|, this time using multiparameter
  type classes:

  \begin{code}
    class Stringly2 f s where
      string2 :: f -> s
      unstring2 :: s -> f
  \end{code}

  Using multiparameter type classes, give the |Stringly2| instances
  required to implement |string2| and |unstring2| as specified
  above.
\end{subproblem}

\newpage

\problem{Spec: use specification based development techniques}{20}

A binary search tree is a binary tree with the following invariant:
the value at the root of the tree is \emph{larger than or equal to}
all values in the \emph{left} subtree, and \emph{smaller than or equal to}
all values in the \emph{right} subtree.

Below is a data type definition for such a tree.

\begin{code}
  data Tree a = Nil | Tree a (Tree a) (Tree a)
\end{code}

\begin{subproblem}{5}
  Write a QuickCheck property which expresses the invariant given above for the
  |Tree| type.

  \emph{Hint: it is easier to check if a list is ordered than to check if a
        tree is...}
\end{subproblem}

\begin{subproblem}{7}
  Write a sized QuickCheck generator
  (|sizedTree :: Ord a => Gen a -> Gen (Tree a)|) for the |Tree| type.
  Generated trees must preserve the invariant given above.
  
  Do \emph{not} just generate arbitrary trees and filter out the ones that
  don't satisfy the invariant.
  If your generator uses |suchThat|, you are on the wrong track.
  
  \emph{Hint: one possible solution involves generating lists of elements,
        sorted or otherwise...}
\end{subproblem}

\begin{subproblem}{3}
  Make |Tree| an instance of QuickCheck's |Arbitrary| type class and write a
  |main| function which uses QuickCheck to test that your generator preserves
  the invariant for trees of integers.
\end{subproblem}

\begin{subproblem}{5}
  Since the |Tree| type is a functor, we add an appropriate
  |Functor| instance for it.

  \begin{code}
    instance Functor Tree where
      fmap _ Nil          = Nil
      fmap f (Tree x l r) = Tree (f x) (fmap f l) (fmap f r)
  \end{code}

  Recall that all functors must obey the two \emph{functor laws}:

  \begin{spec}
    fmap id = id
    
    fmap (f . g) = fmap f . fmap g
  \end{spec}

  Using equational reasoning, show that the functor laws hold for the above
  |Functor| instance.
\end{subproblem}

%\newpage

\appendix
\newpage

%include library_documentation.lhs
\end{document}
