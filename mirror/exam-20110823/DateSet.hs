-- First some interface code (not part of the exam solution)
import qualified Data.Time.Calendar as DTC
import qualified Data.Time.Calendar.OrdinalDate as DTCO
import qualified Data.Time.Calendar.WeekDate as DTCW
import qualified Data.Time.Format as DTF
import qualified System.Locale as SL

type Date = DTC.Day

weekday  :: Date -> Weekday
weekday d = wd  where (_y,_w,wd) = DTCW.toWeekDate d

monthday :: Date -> Monthday
monthday d = md  where (_y,_m,md) = DTC.toGregorian d

yearday  :: Date -> Yearday
yearday = snd . DTCO.toOrdinalDate

readD :: String -> Date 
readD = DTF.readTime SL.defaultTimeLocale "%Y-%m-%d"
  
showD :: Date -> String
showD = DTC.showGregorian  

nextD :: Date -> Date
nextD = succ

epochD = readD "1970-01-01"

----------------
-- Start of the solution to 2b
type Weekday  = Int
type Monthday = Int
type Yearday  = Int

data DateSet = Once Date
             | Daily
             | Weekly  Weekday
             | Monthly Monthday
             | Yearly  Yearday
--             | Filter (Date -> Bool) DateSet  -- not part of exam question
             | Union DateSet DateSet
             | Intersection DateSet DateSet
             | Start Date  
             | End Date
  deriving (Show) -- does not work with Filter (emb. function Date->Bool)

between :: Date -> Date -> DateSet
between start end = Intersection (Start start) (End end)
-- The constructor |Once| is not necessary
once :: Date -> DateSet
once d = between d d

isIn :: Date -> DateSet -> Bool
isIn d (Once d')           = d == d'
isIn d  Daily              = True
isIn d (Weekly  a)         = weekday  d  == a
isIn d (Monthly a)         = monthday d  == a
isIn d (Yearly  a)         = yearday  d  == a
-- isIn d (Filter p ds)       = p d      && isIn d ds
isIn d (Union x y)         = isIn d x || isIn d y
isIn d (Intersection x y)  = isIn d x && isIn d y
isIn d (Start s)           = s <= d
isIn d (End   e)           = d <= e

-- try to locate an upperBound
upperBound :: DateSet -> Date
upperBound (Intersection x y) = min (upperBound x) (upperBound y)
upperBound (Union x y)        = max (upperBound x) (upperBound y)
upperBound (End   e)          = e
-- upperBound (Filter p ds)      = upperBound ds
upperBound (Once d)           = d
upperBound _                  = maxDate

maxDate :: Date
maxDate = readD "9999-12-31"   -- somewhat arbitrary choice

-- and a lowerBound (not part of exam question)
lowerBound :: DateSet -> Date
lowerBound (Intersection x y) = max (lowerBound x) (lowerBound y)
lowerBound (Union x y)        = min (lowerBound x) (lowerBound y)
lowerBound (Start   s)        = s
-- lowerBound (Filter p ds)      = lowerBound ds
lowerBound (Once d)           = d
lowerBound _                  = minDate

minDate :: Date
minDate = readD "0000-01-01"   -- somewhat arbitrary choice

toList :: DateSet -> [Date]
toList ds = filter (`isIn` ds) [lowerBound ds .. upperBound ds]
-- It is OK to use just |epochD| instead of |lowerBound ds|

----------------
-- Example code

test :: DateSet
test = Intersection (between (readD "2011-08-23") 
                             (readD "2011-12-20")) 
                    (Weekly 1)

main = print (toList test)
----------------
-- Part b

{- Question text: Explain briefly the following EDSL terminology in
general: deep embedding, shallow embedding, constructors, combinators
and run function.  Exemplify by referring to or contrasting with your
implementation.  

Possible answers:

* Deep embedding:

A DSL implemented near the syntax, deep down from the sematics of the
domain, is called a "deep embedding". The code above is an example -
the |DateSet| datatype directly captures the abstract syntax of the
domain, not the semantics.

* Shallow embedding:

A DSL implemented near the semantics is called a "shallow
embedding". Often it can be constructed by tupling up suitable
run-functions. The example above is not done that way (mainly because
it is a bit tricky to handle the combination of infinite and finite
lists effectively).

* constructors

Functions which build elements of the domain from "other
data". Examples include |Once|, |Daily|, |Weekly|, |Monthly|,
|Yearly|, |Start|, |End|.

* combinators 

Functions which build elements of the domain from other elements of
the domain (+perhaps some other data). Examples include |Union|,
|Intersection|.

* run functions

Functions from the domain to some semantics. In our example we have
|isIn|, |toList| and |upperBound|.

-}
