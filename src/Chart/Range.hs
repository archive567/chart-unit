module Chart.Range where

import Protolude
import Control.Lens
import Linear.V2

newtype Range a = Range { unRange :: (a,a) } deriving (Show, Eq, Functor)

instance (Ord a) => Semigroup (Range a) where
    (Range (l,u)) <> (Range (l',u')) =
        Range (if l < l' then l else l',if u > u' then u else u')

low :: Lens' (Range a) a
low = lens (\(Range (l,_)) -> l) (\(Range (_,u)) l -> Range (l,u))

high :: Lens' (Range a) a
high = lens (\(Range (_,u)) -> u) (\(Range (l,_)) u -> Range (l,u))

rScale :: (Fractional a) => a -> Range a -> Range a
rScale a r = (a*) <$> r

rScaleX :: (Fractional a, R1 r) => a -> r (Range a) -> r (Range a)
rScaleX a r = over _x (rScale a) r

rScaleY :: (Fractional a, R2 r) => a -> r (Range a) -> r (Range a)
rScaleY a r = over _y (rScale a) r

ur = Range (-0.5, 0.5)

uxy = V2 ur ur

type XY = V2 (Range Double)

instance Semigroup XY where
    (V2 rx ry) <> (V2 rx' ry') = V2 (rx<>rx') (ry<>ry')

-- ur `times` r = r
-- r `div` r = ur
-- r `div` ur = r

