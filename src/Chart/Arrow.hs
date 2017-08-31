{-# OPTIONS_GHC -Wall #-}

-- | Charts that depict gradients and similar, using arrows

module Chart.Arrow (
    Arrow(..)
  , ArrowOptions(..)
  , normArrows
  , arrows
  , arrowChart
  , arrowChart_
  ) where

import Chart.Core

import NumHask.Prelude hiding (max,(&))
import NumHask.Space
import NumHask.Range
import NumHask.Rect
import NumHask.Pair

import Data.Ord (max)
import Diagrams.Prelude hiding (width, D, Color, project)

data ArrowOptions a = ArrowOptions
    { arrowMinLength :: a
    , arrowMaxLength :: a
    , arrowMinHeadLength :: a
    , arrowMaxHeadLength :: a
    , arrowMinStaffWidth :: a
    , arrowMaxStaffWidth :: a
    , arrowColor :: AlphaColour Double
    , arrowHeadStyle :: ArrowHT a
    }

instance Default (ArrowOptions Double) where
    def = ArrowOptions 0.02 0.2 0.01 0.1 0.002 0.005 ublue dart

-- | equalize the arrow space width with the data space one.
-- this creates the right arrow sizing in physical chart space
normArrows :: [Arrow] -> [Arrow]
normArrows xs =
    zipWith Arrow ps as'
  where
    -- data points
    ps = arrowPos <$> xs
    -- arrow vectors
    as = arrowDir <$> xs
    as' = (\x ->
             x *
             width (space $ arrowPos <$> xs :: Rect Double) /
             width (space $ arrowDir <$> xs :: Rect Double)) <$>
          as

-- | an arrow structure contains position, direction and size information
data Arrow = Arrow
    { arrowPos :: Pair Double -- position of arrow tail
    , arrowDir :: Pair Double -- direction and strength of arrow
    } deriving (Eq, Show)

-- | arrows rescale data across position, and between position and arrow direction
-- note that, due t0 all this scaling stuff, there is no such thing as a single arrow_ chart
--
-- > arrows (def {arrowMaxLength=0.5,arrowMaxHeadLength=0.2,arrowMaxStaffWidth=0.01}) [Arrow (Pair x (sin (5*x))) (Pair x (cos x)) | x<-grid MidPos (one::Range Double) 100]
--
-- ![arrows example](other/arrowsExample.svg)
--
arrows :: (Traversable f) => ArrowOptions Double -> f Arrow -> Chart b
arrows opts xs = c
  where
    c = fcA (arrowColor opts) $ position $  getZipList $
        (\ps' as' hrel' wrel' srel' ->
                    (ps',
                     arrowAt' (arropts hrel' wrel') (p2 (0, 0))
                     ((srel'/norm as') *^ as'))) <$> ZipList
        (toList $ p_ <$> ps) <*> ZipList
        (toList $ r_ <$> as) <*> ZipList
        (toList hrel) <*> ZipList
        (toList wrel) <*> ZipList
        srel
    ps = arrowPos <$> xs
    -- arrow vectors
    as = arrowDir <$> xs
    -- width of the data space
    (Pair dx dy) = width (space ps :: Rect Double)
    -- norm of arrow vectors relative to the data space metric
    anorm = (\(Pair x y) -> sqrt((x/dx)**2+(y/dy)**2)) <$> as
    -- the maximum arrow vector norm
    (Range _ anormMax) = space anorm
    -- the overall size of the arrows, as a proportion to the data space
    arel = (\x -> max (anormMax * arrowMinLength opts)
                 (x / anormMax * arrowMaxLength opts)) <$> anorm
    -- size of the head (as a proportion of the data space)
    hrel = (\x -> max (arrowMinHeadLength opts) (arrowMaxHeadLength opts * x)) <$>
        arel
    -- widt of the staff
    wrel = (\x -> max (arrowMinStaffWidth opts) (arrowMaxStaffWidth opts * x)) <$>
        arel
    -- length of the staff (taking into account the head length)
    srel = zipWith (\la lh -> max 1e-12 (la - lh)) (toList arel) (toList hrel)
    -- diagrams arrow options
    arropts lh lw'' = with & arrowHead .~ arrowHeadStyle opts &
                 headLength .~ global lh &
                 shaftStyle %~ (lwG lw'' & lcA (arrowColor opts)) &
                 headStyle %~ (lcA (arrowColor opts) & fcA (arrowColor opts))

-- | A chart of arrows
arrowChart ::
    (Traversable f) =>
    [ArrowOptions Double] ->
    Aspect ->
    Rect Double ->
    [f Arrow] ->
    Chart b
arrowChart optss (Aspect asp) r xss =
    mconcat $ zipWith (\opts xs -> arrows opts $
    (\(Arrow d arr) ->
        Arrow (project r asp d) (project r asp arr)) <$> xs) optss xss

-- | an arrow chart scaled to its own range
--
-- > let as = normArrows [Arrow (Pair x y) (Pair (sin 1/x+0.0001) (cos 1/y+0.0001)) | x<-grid MidPos (one::Range Double) 20, y<-grid MidPos (one::Range Double) 20]
-- > arrowChart_ [def] asquare [as]
--
-- ![arrowChart_ example](other/arrowChart_Example.svg)
--
arrowChart_ ::
    (Traversable f) =>
    [ArrowOptions Double] ->
    Aspect ->
    [f Arrow] ->
    Chart b
arrowChart_ optss asp xss =
    arrowChart optss asp r xss
  where
    r = fold (space . map arrowPos <$> xss)
