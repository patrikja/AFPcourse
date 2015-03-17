% -*- latex -*-
\documentclass[twoside]{article}
\usepackage{a4wide}
\usepackage[utf8]{inputenc}
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

\newcommand\todo[1]{\textbf{TODO\{}#1\textbf{\}}}

%include lhs2TeX.fmt
%include forall.fmt
%format == = "\doubleequals"
\title{Advanced Functional Programming TDA342/DIT260}
\author{Patrik Jansson}
\date{2015-03-17}

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
    {\bf Result:}  & Announced no later than 2015-04-03 \\
    {}\\

    {\bf Exam check:} & Mo 2015-04-13 and Tu 2015-04-14. Both at 12.45-13.10 in EDIT 5468.  \\
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

The following API (From RWH, chapter 9) is aimed at describing predicates for searching the
file system in the style of the command-line utility \texttt{find}:

\begin{code}
type InfoP a  =  FilePath     -- path to directory entry
              -> Permissions  -- permissions   
              -> Integer      -- file size     
              -> UTCTime      -- last modified 
              -> a

constP    :: a -> InfoP a
liftPc    :: (a -> b -> c) -> InfoP a ->       b -> InfoP c
liftP2    :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftPath  :: (FilePath -> a) -> InfoP a
(&&?)     :: InfoP Bool -> InfoP Bool -> InfoP Bool
(||?)     :: InfoP Bool -> InfoP Bool -> InfoP Bool
pathP     :: InfoP FilePath
sizeP     :: InfoP Integer
(==?)     :: (Eq a)   => InfoP a -> a -> InfoP Bool
(>?)      :: (Ord a)  => InfoP a -> a -> InfoP Bool

type Predicate = InfoP Bool
find :: FilePath -> Predicate -> IO [FilePath]  -- run function
\end{code}

This is an example of the intended use:

\begin{code}
myTest :: InfoP Bool
myTest = (liftPath takeExtension ==? ".hs") &&? (sizeP >? 100000)

test :: IO [FilePath]
test = find "/home/patrikj/src" myTest
\end{code}
where |takeExtension :: FilePath -> String| is from |System.FilePath|.

\begin{subproblem}{12}
Implement the constructors and combinators (but not the run function): 
|constP|, |liftPc|, |liftP2|, |liftPath|, |(&&?)|, |(||||?)|, |pathP|, |sizeP|, |(==?)|, |(>?)|.
\end{subproblem}

One implementation of the run function (|find|) is as follows:
\begin{code}
find path p = getRecursiveContents path >>= filterM check
    where check :: FilePath -> IO Bool
          check = {- ... some code using |p| ... -} 
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents fp = {- ... some code using |forM| ... -}
\end{code}
%$

\begin{subproblem}{8}
  Implement |filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]| and
  |forM :: Monad m => [a] -> (a -> m b) -> m [b]| using just |return|,
  |(>>=)|, |liftM| and |foldr|. (No |do|-notation, no other library
  functions.) Provide the type of the first argument to |foldr| in
  both cases.
\end{subproblem}

%\newpage
\problem{Spec: use specification based development techniques}{20}
%format <== = "\Leftarrow"
The \emph{list-fusion} law is as follows:
\begin{spec}
    f . foldr g b = foldr h c    
  <==                       -- (is implied by)
    f b = c                 -- and
    f (g x y) = h x (f y)   -- for all |x| and |y|
\end{spec}

\begin{subproblem}{10}
  Give the polymorphic types for |f|, |g|, |b|, |h|, |c| and prove
  list-fusion for finite lists by induction.
\end{subproblem}

\begin{subproblem}{10}
  For which |g| and |b| is |map p = foldr g b|?  Use this form and
  list-fusion to prove that |foldr q r . map p| can be computed as a
  single |foldr|. Start by giving the polymorphic types of all the
  one-letter variables.
\end{subproblem}

\problem{Types: read, understand and extend Haskell programs which use
  advanced type system features}{20}

Type classes in Haskell can be ``translated away'' using a concept
called ``dictionaries''. Each monomorphic instance declaration is
translated to a record (called a dictionary) containing the methods of
the type class for this particular instance. The following code shows
a simplified version of a manual ``dictionary translation'' for the
|Eq| class and the instances |Eq Foo| and |Eq a => Eq [a]|.
\begin{code}
type EqT a = a -> a -> Bool
data EqDict  a = EqDict { eq :: EqT a }

-- An arbitrary type just as an example:
data Foo = Foo String (Maybe String)  -- Personal id number + optional comment 

-- instance |Eq Foo| 
eqFoo :: EqT Foo
eqFoo (Foo pa _ca) (Foo pb _cb) = pa == pb

eqFooD :: EqDict Foo
eqFooD = EqDict eqFoo

-- instance |Eq a => Eq [a]| 
eqListD :: EqDict a -> EqDict [a]
eqListD (EqDict eqa) = EqDict (eqLa eqa)
\end{code}

\begin{subproblem}{5}
Implement the function |eqLa :: EqT a -> EqT [a]| which ``lifts''
equality on an element type |a| to equality on lists of |a|.
\end{subproblem}

The same technique can be used also for (a simplified version of) the
|Monad| class as follows.
\begin{code}
{-# LANGUAGE RankNTypes #-}
type RetT   m = forall a.    a -> m a
type BindT  m = forall a b.  m a -> (a -> m b) -> m b   
data MonadDict m = MonadDict { ret :: RetT m, bind :: BindT m}
-- instance Monad Maybe                   
monadMaybeD :: MonadDict Maybe
monadMaybeD = MonadDict returnM bindM

returnM :: RetT Maybe
returnM = Just

bindM :: BindT Maybe
bindM mx f = maybe Nothing f mx
\end{code}

For |StateT| the instance |Monad m => Monad (StateT s m)| gets the following form:

\begin{code}
monadStateT :: MonadDict m -> MonadDict (StateT s m)
monadStateT (MonadDict retm bindm) = MonadDict (retStateT retm) (bindStateT bindm)
retStateT   :: RetT   m   ->  RetT   (StateT s m)
bindStateT  :: BindT  m   ->  BindT  (StateT s m)
  
newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}
\end{code}


\begin{subproblem}{15}
Implement |retStateT| and |bindStateT|.
\end{subproblem}



%\newpage

\appendix
\newpage

%include library_documentation.lhs
\end{document}

