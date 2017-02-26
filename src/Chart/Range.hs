module Chart.Range where

import Tower.Prelude
import Tower.Extrema
import Control.Category (id)
import Control.Lens
import Linear.V2
-- import Tower.Extrema
import qualified Control.Foldl as L
import Data.Foldable

singleton a = Extrema (a,a)

rescale (Extrema (l, u)) a = (a-l)/(u-l) - one/(one+one)

type XY = V2 (Extrema Double)

instance MultiplicativeMagma XY where
    (V2 a0 b0) `times` (V2 a1 b1) = V2 (a0 `times` a1) (b0 `times` b1)

instance MultiplicativeUnital XY where
    one = V2 one one
instance MultiplicativeAssociative XY
-- instance MultiplicativeCommutative XY

-- instance Multiplicative XY
instance MultiplicativeInvertible XY where
    recip (V2 a b) = V2 (recip a) (recip b)

-- instance MultiplicativeGroup XY

instance Semigroup XY where
    (<>) (V2 a b) (V2 a1 b1) = V2 (a+a1) (b+b1)

instance Monoid XY where
    mempty = V2 zero zero
    mappend = (<>)

toCorners xy = [(V2 (view low . view _x $ xy)
                    (view low . view _y $ xy)),
                (V2 (view high . view _x $ xy)
                    (view high . view _y $ xy))]

rScale :: (Multiplicative a) => a -> Extrema a -> Extrema a
rScale a r = (a*) <$> r

rScaleX :: (Multiplicative a, R1 r) => a -> r (Extrema a) -> r (Extrema a)
rScaleX a r = over _x (rScale a) r

rScaleY :: (Multiplicative a, R2 r) => a -> r (Extrema a) -> r (Extrema a)
rScaleY a r = over _y (rScale a) r

-- range of a foldable
range :: (BoundedField a, Multiplicative a, Foldable f, Ord a) => f a -> Extrema a
range = L.fold (L.Fold (\x a -> x + singleton a) zero id)

-- | rescale data so that range is equal to unitRange
-- range . unitize === const zero
unitize :: (BoundedField b, Field b, Foldable f, Ord b, Functor f, Fractional b) => f b -> f b
unitize xs = rescale (Chart.Range.range xs) <$> xs

-- | `rescaleP rnew rold p` rescales a data point from an old range to a new range
-- rescaleP n o (view low o) == view low n
-- rescaleP n o (view high o) == view high n
-- rescaleP a a == id
rescaleP :: (Field b, Fractional b) => Extrema b -> Extrema b -> b -> b
rescaleP (Extrema (l,u)) (Extrema (l',u')) q =
    ((q-l')/(u'-l')) * (u-l) + l

-- | rescale data to a new range
-- range . scale' unitExtrema === const unitExtrema
rescale' :: (BoundedField b, Foldable f, Ord b, Functor f, Fractional b) => Extrema b -> f b -> f b
rescale' r qs = rescaleP (Chart.Range.range qs) r <$> qs

-- | rescale data so that range is equal to unitExtrema
-- range . unitize === const unitExtrema
unitize' :: (BoundedField b, Foldable f, Ord b, Functor f, Fractional b) => f b -> f b
unitize' xs = rescale' one xs

-- traversable R2 specalizations
-- range specialized for an R2
rangeR2 :: (BoundedField a, Fractional a, Traversable f, R2 r, Ord a) => f (r a) -> V2 (Extrema a)
rangeR2 qs = V2 xs ys
  where
    xs = Chart.Range.range $ view _x <$> qs
    ys = Chart.Range.range $ view _y <$> qs

-- rescale an R2
rescaleR2 :: (Field a, Functor f, Eq a, Fractional a, R2 r) => V2 (Extrema a) -> f (r a) -> f (r a)
rescaleR2 (V2 rx ry) qs = (over _x (rescale rx) . over _y (rescale ry)) <$> qs

-- convert an R2 traversable so that the range is (V2 unitExtrema unitExtrema)
unitizeR2 :: (BoundedField a, Traversable f, Ord a, Fractional a, R2 r) => f (r a) -> f (r a)
unitizeR2 qs = rescaleR2 (rangeR2 qs) qs

-- rescale an R2
rescaleR2' :: (BoundedField a, Foldable f, Ord a, Functor f, Fractional a, R2 r) => V2 (Extrema a) -> f (r a) -> f (r a)
rescaleR2' (V2 rx ry) qs =
    (over _x (rescaleP rx rx') . over _y (rescaleP ry ry')) <$> qs
  where
    rx' = Chart.Range.range $ view _x <$> qs
    ry' = Chart.Range.range $ view _y <$> qs

-- convert an R2 traversable so that the range is (V2 unitExtrema unitExtrema)
unitizeR2' :: (BoundedField a, Traversable f, Ord a, Fractional a, R2 r) => f (r a) -> f (r a)
unitizeR2' qs = rescaleR2' (V2 one one) qs

-- range specialized to multiple data sets
rangeR2s :: (BoundedField a, Fractional a, Traversable g, Traversable f, R2 r, Ord a) =>
    g (f (r a)) ->
    V2 (Extrema a)
rangeR2s qss = foldl1 (\(V2 x y) (V2 x' y') -> V2 (x+x') (y+y')) $ rangeR2 <$> qss

-- rescale multiple R2 sets
rescaleR2s :: (Field a, Functor g, Functor f, Fractional a, Eq a, R2 r) =>
    V2 (Extrema a) -> g (f (r a)) -> g (f (r a))
rescaleR2s r qss = rescaleR2 r <$> qss

-- convert multiple R2 sets to the unit range
unitizeR2s :: (BoundedField a, Traversable g, Traversable f, Fractional a, Ord a, R2 r) =>
    g (f (r a)) -> g (f (r a))
unitizeR2s qss = rescaleR2s (rangeR2s qss) qss

-- rescale multiple R2 sets
rescaleR2s' :: (BoundedField a, Foldable f, Ord a, Functor g, Functor f, Fractional a, R2 r) =>
    V2 (Extrema a) -> g (f (r a)) -> g (f (r a))
rescaleR2s' r qss = rescaleR2' r <$> qss

-- convert multiple R2 sets to the unit range
unitizeR2s' :: (BoundedField a, Traversable g, Traversable f, Fractional a, Ord a, R2 r) =>
    g (f (r a)) -> g (f (r a))
unitizeR2s' qss = rescaleR2s' (V2 one one) qss

-- truncate values falling outside of a range

truncate :: (Ord a) => Extrema a -> [a] -> [a]
truncate (Extrema (l,u)) qs = filter (\q -> q < l || q > u) qs

-- truncate an R2
truncateR2 :: (Ord a, R2 r) => V2 (Extrema a) -> [r a] -> [r a]
truncateR2 (V2 (Extrema (lx,ux)) (Extrema (ly,uy))) qs =
    filter (\q -> view _x q >= lx && view _x q <= ux &&
                 view _y q >= ly && view _y q <= uy
           ) qs

truncateR2s :: (R2 r, Ord a) => V2 (Extrema a) -> [[r a]] -> [[r a]]
truncateR2s r qss = fmap (truncateR2 r) qss

