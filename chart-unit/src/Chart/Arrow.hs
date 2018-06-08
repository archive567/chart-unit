{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

-- | Charts that depict gradients and similar data, using arrows in positions
module Chart.Arrow
  ( Arrow(..)
  , ArrowHTStyle(..)
  , ArrowOptions(..)
  , defaultArrowOptions
  , normArrows
  , arrows
  , arrowChart
  , arrowChart_
  ) where

import Chart.Core
import Data.Ord (max)
import Diagrams.Prelude hiding (Color, D, project, width)
import GHC.Generics
import NumHask.Pair
import NumHask.Prelude hiding ((&), max)
import NumHask.Range
import NumHask.Rect
import NumHask.Space

-- | ArrowStyles based on diagrams
data ArrowHTStyle a
  = Tri
  | Dart
  | HalfDart
  | Spike
  | Thorn
  | LineHead
  | NoHead
  | Tri2 a
  | Dart2 a
  | HalfDart2 a
  | Spike2 a
  | Thorn2 a
  | Tri'
  | Dart'
  | HalfDart'
  | Spike'
  | Thorn'
  | LineTail
  | NoTail
  | Quill
  | Block
  | Quill2 a
  | Block2 a
  deriving (Show, Eq, Generic)

-- | conversion between unit and diagrams
-- ToDo: abstract ArrowHT usage
arrowHTStyle :: (RealFloat a) => ArrowHTStyle a -> ArrowHT a
arrowHTStyle Tri = tri
arrowHTStyle Dart = dart
arrowHTStyle HalfDart = halfDart
arrowHTStyle Spike = spike
arrowHTStyle Thorn = thorn
arrowHTStyle LineHead = lineHead
arrowHTStyle NoHead = noHead
arrowHTStyle (Tri2 a) = arrowheadTriangle (a @@ deg)
arrowHTStyle (Dart2 a) = arrowheadDart (a @@ deg)
arrowHTStyle (HalfDart2 a) = arrowheadHalfDart (a @@ deg)
arrowHTStyle (Spike2 a) = arrowheadSpike (a @@ deg)
arrowHTStyle (Thorn2 a) = arrowheadThorn (a @@ deg)
arrowHTStyle Tri' = tri'
arrowHTStyle Dart' = dart'
arrowHTStyle HalfDart' = halfDart'
arrowHTStyle Spike' = spike'
arrowHTStyle Thorn' = thorn'
arrowHTStyle LineTail = lineTail
arrowHTStyle NoTail = noTail
arrowHTStyle Quill = quill
arrowHTStyle Block = block
arrowHTStyle (Quill2 a) = arrowtailQuill (a @@ deg)
arrowHTStyle (Block2 a) = arrowtailBlock (a @@ deg)

-- | todo: quite a clunky specification of what an arrow is (or could be)
data ArrowOptions = ArrowOptions
  { minLength :: Double
  , maxLength :: Double
  , minHeadLength :: Double
  , maxHeadLength :: Double
  , minStaffWidth :: Double
  , maxStaffWidth :: Double
  , color :: UColor Double
  , hStyle :: ArrowHTStyle Double
  } deriving (Show, Eq, Generic)

defaultArrowOptions :: ArrowOptions
defaultArrowOptions = ArrowOptions 0.02 0.2 0.01 0.1 0.002 0.005 ublue Dart

-- | Equalize the arrow space width with the data space one.
-- this creates the right arrow sizing in physical chart space
normArrows :: [Arrow] -> [Arrow]
normArrows xs = zipWith Arrow ps as'
  where
    -- data points
    ps = arrowPos <$> xs
    -- arrow vectors
    as = arrowDir <$> xs
    as' =
      (\x ->
         x * width (space $ arrowPos <$> xs :: Rect Double) /
         width (space $ arrowDir <$> xs :: Rect Double)) <$>
      as

-- | An arrow structure contains position, direction and size information
data Arrow = Arrow
  { arrowPos :: Pair Double -- position of arrow tail
  , arrowDir :: Pair Double -- direction and strength of arrow
  } deriving (Show, Eq, Generic)

-- | Rescale data across position, and between position and arrow direction.
--
-- note that, due to this auto-scaling, there is no such thing as a single arrow_ chart
--
-- > arrowsExample :: Chart b
-- > arrowsExample =
-- >   arrows
-- >     ( #maxLength .~ 0.5 $
-- >       #maxHeadLength .~ 0.2 $
-- >       #maxStaffWidth .~ 0.01 $ def)
-- >     [ Arrow (Pair x (sin (5 * x))) (Pair x (cos x))
-- >     | x <- grid MidPos (one :: Range Double) 100
-- >     ]
--
-- ![arrows example](other/arrowsExample.svg)
--
arrows :: (Traversable f) => ArrowOptions -> f Arrow -> Chart b
arrows opts xs = c
  where
    c =
      fcA (acolor $ color opts) $
      position $
      getZipList $
      (\ps' as' hrel' wrel' srel' ->
         ( ps'
         , arrowAt'
             (arropts hrel' wrel')
             (p2 (0, 0))
             ((srel' / norm as') *^ as'))) <$>
      ZipList (toList $ p_ <$> ps) <*>
      ZipList (toList $ r_ <$> as) <*>
      ZipList (toList hrel) <*>
      ZipList (toList wrel) <*>
      ZipList srel
    ps = arrowPos <$> xs
    -- arrow vectors
    as = arrowDir <$> xs
    -- width of the data space
    (Pair dx dy) = width (space ps :: Rect Double)
    -- norm of arrow vectors relative to the data space metric
    anorm = (\(Pair x y) -> sqrt ((x / dx) ** 2 + (y / dy) ** 2)) <$> as
    -- the maximum arrow vector norm
    (Range _ anormMax) = space anorm
    -- the overall size of the arrows, as a proportion to the data space
    arel =
      (\x -> max (anormMax * minLength opts) (x / anormMax * maxLength opts)) <$>
      anorm
    -- size of the head (as a proportion of the data space)
    hrel = (\x -> max (minHeadLength opts) (maxHeadLength opts * x)) <$> arel
    -- widt of the staff
    wrel = (\x -> max (minStaffWidth opts) (maxStaffWidth opts * x)) <$> arel
    -- length of the staff (taking into account the head length)
    srel = zipWith (\la lh -> max 1e-12 (la - lh)) (toList arel) (toList hrel)
    -- diagrams arrow options
    arropts lh lw'' =
      with & arrowHead .~ arrowHTStyle (hStyle opts) & headLength .~ global lh &
      shaftStyle %~
      (lwG lw'' & lcA (acolor $ color opts)) &
      headStyle %~
      (lcA (acolor $ color opts) & fcA (acolor $ color opts))

-- | A chart of arrows
arrowChart ::
     (Traversable f)
  => [ArrowOptions]
  -> Rect Double
  -> Rect Double
  -> [f Arrow]
  -> Chart b
arrowChart optss asp r xss =
  mconcat $
  zipWith
    (\opts xs ->
       arrows opts $
       (\(Arrow d arr) -> Arrow (project r asp d) (project r asp arr)) <$> xs)
    optss
    xss

-- | An arrow chart scaled to its own range
--
-- > arrowChart_Example :: Chart b
-- > arrowChart_Example = arrowChart_ [def] asquare [as]
-- >   where
-- >     as =
-- >       normArrows
-- >         [ Arrow (Pair x y) (Pair (sin 1 / x + 0.0001) (cos 1 / y + 0.0001))
-- >         | x <- grid MidPos (one :: Range Double) 20
-- >         , y <- grid MidPos (one :: Range Double) 20
-- >         ]
--
-- ![arrowChart_ example](other/arrowChart_Example.svg)
--
arrowChart_ ::
    (Traversable f)
  => [ArrowOptions]
  -> Rect Double
  -> [f Arrow]
  -> Chart b
arrowChart_ optss asp xss = arrowChart optss asp r xss
  where
    r = fold (space . map arrowPos <$> xss)
