% -*- latex -*-
\documentclass[twoside]{article}
\usepackage{a4wide}
\usepackage{url}
\usepackage{multicol}
\setlength{\parindent}{0ex}
\setlength{\parskip}{1ex plus 1 ex minus 0.5ex}
%\setlength{\parskip}{0em plus 1em minus 0.2em}

% the `doubleequals' macro is due to Jeremy Gibbons
\def\doubleequals{\mathrel{\unitlength 0.01em
  \begin{picture}(78,40)
    \put(7,34){\line(1,0){25}} \put(45,34){\line(1,0){25}}
    \put(7,14){\line(1,0){25}} \put(45,14){\line(1,0){25}}
  \end{picture}}}

%include lhs2TeX.fmt
%include forall.fmt
%format == = "\doubleequals"
\title{Advanced Functional Programming TDA342/DIT260}
\author{Patrik Jansson}
\date{2012-08-28}

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
    {\bf Contact:} & Patrik Jansson, ext 5415. \\
    {}\\
    {\bf Result:}  & Announced no later than 2012-09-16\\
    {}\\

    {\bf Exam check:} & Th 2012-09-06 and Fr 2012-09-07. Both at 12.45-13.10 in EDIT 5468.  \\
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

\problem{DSL: implement embedded domain specific languages}{20} 

A DSL for two-dimensional geometrical shapes has the following
interface (from AFP lecture 2):
\begin{code}
empty   :: Shape
disc    :: Shape    -- disc with radius |1| around the origin
square  :: Shape    -- square between |(0,0)| and |(1,1)|    

translate   :: Vec    ->  Shape -> Shape  -- shift the shape along a vector                  
scale       :: Vec    ->  Shape -> Shape  -- magnify the shape by a vector                   
rotate      :: Angle  ->  Shape -> Shape  -- rotate the shape by an angle (around the origin)
union       :: Shape  ->  Shape -> Shape
intersect   :: Shape  ->  Shape -> Shape
difference  :: Shape  ->  Shape -> Shape

inside      :: Point ->  Shape -> Bool    -- run function: is the point inside the shape?
\end{code}
In your implementations you can assume this import is in scope:
\begin{code}
import Matrix  ( Vec, vecX, vecY  -- | :: Vec -> Double|
               , Angle            -- | = Double|
               , Point            -- | = Vec|
               , sub, divide      -- | :: Point  -> Vec    -> Point|
               , rot              -- | :: Angle  -> Point  -> Point|
               )   
\end{code}
Your task is to implement the following parts of this API for a deep and a
for a shallow embedding.


\begin{subproblem}{10}
  \textbf{Deep:} implement |Shape|, |empty|, |disc|, |square|, |translate|, |union|,
  |intersect| and |inside|.
\end{subproblem}

\begin{subproblem}{10}
  \textbf{Shallow:} implement |Shape|, |disc|, |scale|, |rotate|, |intersect|,
  |difference| and |inside|.
\end{subproblem}

\problem{Spec: use specification based development techniques}{20}

Consider the three (partially applied) type constructors |Either e|,
|((,) e)| and |((->) e)|. 
%
Your implementation should be polymorphic in the given type |e|.

\begin{subproblem}{10}
  Provide a |Functor| instance (thus implement |fmap|) for each of
  them.
%
  Explain which of these are monads.
%
  Do you recognize any of these from Haskell?
\end{subproblem}

\begin{subproblem}{10}
  State the functor laws and prove by equational reasoning that they
  hold for these types.
\end{subproblem}



\problem{Types: read, understand and extend Haskell programs which use advanced type system features}{20}

The |ListT| monad transformer adds ``backtracking'' to a given monad
(if it is commutative).
\begin{code}
newtype ListT m a = ListT { runListT :: m [a] }
instance (Monad m) => Monad (ListT m) where
    return  =  returnLT
    (>>=)   =  bindLT
\end{code}

\begin{subproblem}{10}
Provide type signatures for, and implement, |returnLT| and |bindLT|.
\end{subproblem}
% $

\begin{subproblem}{10}
A \emph{commutative monad} is any monad where we can replace the expression:

%format a1
%format a2
%format m1
%format m2
\fbox{
\begin{minipage}{0.24\linewidth}
\begin{code}
do  a1 <- m1
    a2 <- m2
    f a1 a2
\end{code}
\end{minipage}
}
with
\fbox{
\begin{minipage}{0.24\linewidth}
\begin{code}
do  a2 <- m2
    a1 <- m1
    f a1 a2
\end{code}
\end{minipage}
}
without changing the meaning.

Write a polymorphic QuickCheck property |commutative| which can test
if a monad is commutative.
%
Specialise your property to monomorphic types and to one of the
functors from Problem 2 and implement a generator. Is that functor a
commutative monad?
\end{subproblem}

\appendix
\newpage

%include library_documentation.lhs
\end{document}
