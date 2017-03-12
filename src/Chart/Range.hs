{-# OPTIONS_GHC -Wall #-}

module Chart.Range
  ( Range(..)
  , (Chart.Range....)
  , low
  , high
  , width
  , range
  , toCorners
  , XY
  , toV2
  , toV4
  , fromV4
  , rescaleP
  , rangeR2s
  , scaleR2s
  , rangeRects
  , scaleRects
  , scaleV4s
  , ticksRound
  , ticksExact
  , GridPoints(..)
  , grid
  , gridXY
  , gridRectXY
  , rectsToLine
  ) where

import Tower.Prelude
import Control.Category (id)
import Control.Lens
import Linear.V2
import Linear.V4
import qualified Control.Foldl as L
import Data.Foldable
import Test.QuickCheck

newtype Range a = Range { range_ :: (a, a) }
  deriving (Eq, Ord, Show, Functor)

(...) :: Ord a => a -> a -> Range a
a ... b
  | a <= b = Range (a, b)
  | otherwise = Range (b, a)

low :: Lens' (Range a) a
low = lens (\(Range (l,_)) -> l) (\(Range (_,u)) l -> Range (l,u))

high :: Lens' (Range a) a
high = lens (\(Range (_,u)) -> u) (\(Range (l,_)) u -> Range (l,u))

mid ::
    (BoundedField a) =>
    Lens' (Range a) a
mid =
    lens
    (\(Range (l,u)) -> (l+u)/two)
    (\(Range (l,u)) m -> Range (m - (u-l)/two, m + (u-l)/two))

width ::
    (BoundedField a) =>
    Lens' (Range a) a
width =
    lens
    (\(Range (l,u)) -> (u-l))
    (\(Range (l,u)) r ->
       Range ((l+u)/two - r/two,
                (l+u)/two + r/two))

instance (Ord a, Arbitrary a) => Arbitrary (Range a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        if a < b
           then pure (Range (a, b))
           else pure (Range (b, a))

instance (Ord a) => AdditiveMagma (Range a) where
    plus (Range (l0,u0)) (Range (l1,u1)) = Range (min l0 l1, max u0 u1)

instance (Ord a, BoundedField a) => AdditiveUnital (Range a) where
    zero = Range (infinity,neginfinity)

instance (Ord a) => AdditiveAssociative (Range a)
instance (Ord a) => AdditiveCommutative (Range a)
instance (Ord a, BoundedField a) => Additive (Range a)

-- | times may well be some sort of affine transformation lurking under the hood
instance (BoundedField a) => MultiplicativeMagma (Range a) where
    times a b = Range (m - r/two, m + r/two)
        where
          m = view mid b + (view mid a * view width b)
          r = view width a * view width b

-- | The unital object derives from:
--
-- view range one = one
-- view mid zero = zero
-- ie (-0.5,0.5)
instance (BoundedField a) => MultiplicativeUnital (Range a) where
    one = Range (negate half, half)

-- | natural interpretation of an Range as a number is the mid-point
instance (BoundedField a) =>
    AdditiveHomomorphic (Range a) a where
    plushom (Range (l,u)) = (l+u) / two

instance (BoundedField a) => MultiplicativeAssociative (Range a)

instance (AdditiveGroup a) => Normed (Range a) a where
    size (Range (l, u)) = u-l

instance (Ord a, BoundedField a) => MultiplicativeInvertible (Range a) where
    recip a = case view width a == zero of
      True  -> theta
      False -> Range (m - r/two, m + r/two)
        where
          m = negate (view mid a) * recip (view width a)
          r = recip (view width a)

instance (Ord a, BoundedField a) => MultiplicativeRightCancellative (Range a)
instance (Ord a, BoundedField a) => MultiplicativeLeftCancellative (Range a)

-- | theta is a bit like infinity
theta :: (AdditiveUnital a) => Range a
theta = Range (zero, zero)

two :: (BoundedField a) => a
two = one + one

half :: (BoundedField a) => a
half = one / (one + one)

singleton :: a -> Range a
singleton a = Range (a,a)

-- * XY represents the physical plane on which a chart is drawn.
type XY = V2 (Range Double)

instance MultiplicativeMagma XY where
    (V2 a0 b0) `times` (V2 a1 b1) = V2 (a0 `times` a1) (b0 `times` b1)

instance MultiplicativeUnital XY where
    one = V2 one one
instance MultiplicativeAssociative XY
instance MultiplicativeLeftCancellative XY
instance MultiplicativeInvertible XY where
    recip (V2 a b) = V2 (recip a) (recip b)

instance Semigroup XY where
    (<>) (V2 a b) (V2 a1 b1) = V2 (a+a1) (b+b1)

instance Monoid XY where
    mempty = V2 zero zero
    mappend = (<>)

toCorners :: XY -> [V2 Double]
toCorners xy = [V2 (view low . view _x $ xy)
                    (view low . view _y $ xy),
                V2 (view high . view _x $ xy)
                    (view high . view _y $ xy)]

toV2 :: XY -> V2 Double
toV2 xy = V2 (view mid $ view _x xy) (view mid $ view _y xy)

toV4 :: XY -> V4 Double
toV4 xy =
    V4
    (view low $ view _x xy)
    (view low $ view _y xy)
    (view width $ view _x xy)
    (view width $ view _y xy)

fromV4 :: V4 Double -> XY
fromV4 (V4 x y z w) = V2 (Range (x,x+z)) (Range (y,y+w))

-- * computing Ranges
-- | range of a foldable
range :: (BoundedField a, Foldable f, Ord a) => f a -> Range a
range = L.fold (L.Fold (\x a -> x + singleton a) zero id)

-- | range specialized for a traversable container of R2s
rangeR2 :: (BoundedField a, Traversable f, R2 r, Ord a) => f (r a) -> V2 (Range a)
rangeR2 xys = V2 rxs rys
  where
    rxs = range $ view _x <$> xys
    rys = range $ view _y <$> xys

-- | range specialized to double traversables
rangeR2s :: (BoundedField a, Traversable g, Traversable f, R2 r, Ord a) =>
    g (f (r a)) ->
    V2 (Range a)
rangeR2s qss = foldl1 (\(V2 x y) (V2 x' y') -> V2 (x+x') (y+y')) $ rangeR2 <$> qss

-- | V2 range of a V4 rectangle
rangeRect :: (BoundedField a, Traversable f, Ord a) => f (V4 a) -> V2 (Range a)
rangeRect qs = V2 rx ry
  where
    rx = range $ toList (view _x <$> qs) <> toList (view _z <$> qs)
    ry = range $ toList (view _y <$> qs) <> toList (view _w <$> qs)

-- | V2 range of a double container of V4 rects
rangeRects :: (Ord a, BoundedField a, Traversable f, Traversable g) =>
    g (f (V4 a)) -> V2 (Range a)
rangeRects qss = foldl1 (\(V2 x y) (V2 x' y') -> V2 (x+x') (y+y')) $ rangeRect <$> qss

-- | V4 range of a V4 container
rangeV4 :: (Traversable f) => f (V4 Double) -> V4 (Range Double)
rangeV4 qs = V4 rx ry rz rw
  where
    rx = range $ toList (view _x <$> qs)
    ry = range $ toList (view _y <$> qs)
    rz = range $ toList (view _z <$> qs)
    rw = range $ toList (view _w <$> qs)

-- | V4 range of a V4 double container
rangeV4s :: (Traversable g, Traversable f) =>
    g (f (V4 Double)) -> V4 (Range Double)
rangeV4s qss = foldl1 (\(V4 x y z w) (V4 x' y' z' w') -> V4 (x+x') (y+y') (z+z') (w+w')) $
    rangeV4 <$> toList qss

-- * rescaling - transformations from one scale to another
-- | `rescaleP rold rnew p` rescales a data point from an old range to a new range
-- rescaleP o n (view low o) == view low n
-- rescaleP o n (view high o) == view high n
-- rescaleP a a == id
rescaleP :: (Field b) => Range b -> Range b -> b -> b
rescaleP (Range (l0,u0)) (Range (l1,u1)) p =
    ((p-l0)/(u0-l0)) * (u1-l1) + l1

-- | rescales a container of r2's
rescaleR2 :: (R2 r, Field a, Functor f) =>
    V2 (Range a) -> V2 (Range a) -> f (r a) -> f (r a)
rescaleR2 (V2 rx ry) (V2 rx' ry') qs =
    (over _x (rescaleP rx rx') . over _y (rescaleP ry ry')) <$> qs

-- | scale a double container of r2s from the current range
scaleR2s ::
    (R2 r, BoundedField a, Traversable f, Traversable g, Ord a) =>
    V2 (Range a) -> g (f (r a)) -> g (f (r a))
scaleR2s xy qss = rescaleR2 (rangeR2s qss) xy <$> qss

-- | rescale a V4 Rect point from rold to rnew
rescaleRectP :: (Field a) => V2 (Range a) -> V2 (Range a) -> V4 a -> V4 a
rescaleRectP rold rnew q =
    over _x (rescaleP (rold^._x) (rnew^._x)) $
    over _y (rescaleP (rold^._y) (rnew^._y)) $
    over _z (rescaleP (rold^._x) (rnew^._x)) $
    over _w (rescaleP (rold^._y) (rnew^._y))
    q

-- | rescale a Rect container
rescaleRect :: (BoundedField a, Traversable f) => V2 (Range a) -> V2 (Range a) -> f (V4 a) -> f (V4 a)
rescaleRect rold rnew qs = rescaleRectP rold rnew <$> qs

-- | scale a Rect container from it's current range
scaleRects :: (Ord a, BoundedField a, Traversable g, Traversable f) =>
    V2 (Range a) -> g (f (V4 a)) -> g (f (V4 a))
scaleRects r qss = rescaleRect (rangeRects qss) r <$> qss

-- | rescale a V4 from rold to rnew
rescaleV4P :: V4 (Range Double) -> V4 (Range Double) -> V4 Double -> V4 Double
rescaleV4P rold rnew q =
    over _x (rescaleP (rold^._x) (rnew^._x)) $
    over _y (rescaleP (rold^._y) (rnew^._y)) $
    over _z (rescaleP (rold^._z) (rnew^._z)) $
    over _w (rescaleP (rold^._w) (rnew^._w))
    q

-- | rescale a container of V4s
rescaleV4 :: (Functor f) =>
    V4 (Range Double) -> V4 (Range Double) -> f (V4 Double) -> f (V4 Double)
rescaleV4 rold rnew qs = rescaleV4P rold rnew <$> qs

-- | scale a double container of V4s from the current range
scaleV4s :: (Traversable g, Traversable f) =>
    V4 (Range Double) -> g (f (V4 Double)) -> g (f (V4 Double))
scaleV4s r qss = rescaleV4 (rangeV4s qss) r <$> qss

-- tick construction
ticksRound :: (Fractional a, Ord a, QuotientField a Int, ExpRing a, MultiplicativeGroup a) => Range a -> Int -> [a]
ticksRound (Range (l, u)) n = (first' +) . (step *) . fromIntegral <$> [0..n']
  where
    span' = u - l
    step' = 10 ^^ (floor (logBase 10 (span'/fromIntegral n))::Int)
    err = fromIntegral n / span' * step'
    step
      | err <= 0.15 = 10 * step'
      | err <= 0.35 = 5 * step'
      | err <= 0.75 = 2 * step'
      | otherwise = step'
    first' = step * fromIntegral (ceiling (l/step) :: Int)
    last' = step * fromIntegral (floor (u/step) :: Int)
    n' = round ((last' - first')/step) :: Int

ticksExact :: (Field a) => Range a -> Int -> [a]
ticksExact (Range (l, u)) n = (l +) . (step *) . fromIntegral <$> [0..n]
  where
    step = (u - l)/fromIntegral n

data GridPoints = OuterPoints | LowerPoints | MidPoints

grid :: GridPoints -> Range Double -> Int -> [Double]
grid OuterPoints (Range (l,u)) steps =
    (\a -> l + (u - l) / fromIntegral steps * a) .
    fromIntegral <$>
    [0..steps]
grid LowerPoints (Range (l,u)) steps =
    (\a -> l + (u - l) / fromIntegral steps * a) .
    fromIntegral <$>
    [0..(steps - 1)]
grid MidPoints (Range (l,u)) steps =
    (\a -> 1 / 2 * fromIntegral steps + l + (u - l) / fromIntegral steps * a) .
    fromIntegral <$>
    [0..(steps - 1)]

gridXY :: GridPoints -> XY -> V2 Int -> [V2 Double]
gridXY gp (V2 rX rY) (V2 stepX stepY) =
    [V2 x y | x <- grid gp rX stepX, y <- grid gp rY stepY]

gridRectXY :: XY -> V2 Int -> [V4 Double]
gridRectXY (V2 rX rY) (V2 stepX stepY) =
    [V4 x y sx sy | x <- grid LowerPoints rX stepX, y <- grid LowerPoints rY stepY]
  where
    sx = view width rX / fromIntegral stepX
    sy = view width rY / fromIntegral stepY

rectsToLine :: [V4 Double] -> [V2 Double]
rectsToLine xs = (\(V4 x _ z w) -> V2 ((x+z)/2) w) <$> xs
