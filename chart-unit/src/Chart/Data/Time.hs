{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | data algorithms related to time
module Chart.Data.Time
  ( parseUTCTime
  , TimeGrain(..)
  , floorGrain
  , ceilingGrain
  , sensibleTimeGrid
  , PosDiscontinuous(..)
  , placedTimeLabelDiscontinuous
  ) where

import Data.Time
import GHC.Base (String)
import NumHask.Prelude
import NumHask.Space

import qualified Control.Foldl as L
import qualified Data.Text as Text
import qualified Protolude as P

-- | parse text as per iso8601
--
-- >>> :set -XOverloadedStrings
-- >>> let t0 = parseUTCTime ("2017-12-05" :: Text)
-- >>> t0
-- Just 2017-12-05 00:00:00 UTC
--
parseUTCTime :: Text -> Maybe UTCTime
parseUTCTime =
  parseTimeM False defaultTimeLocale (iso8601DateFormat Nothing) . Text.unpack

-- | a step in time
data TimeGrain
  = Years Integer
  | Months Int
  | Days Int
  | Hours Int
  | Minutes Int
  | Seconds Double
  deriving (Show, Generic)

grainSecs :: TimeGrain -> Double
grainSecs (Years n) = fromIntegral n * 365.0 * toDouble nominalDay
grainSecs (Months n) = fromIntegral n * 365.0 / 12 * toDouble nominalDay
grainSecs (Days n) = fromIntegral n * toDouble nominalDay
grainSecs (Hours n) = fromIntegral n * 60 * 60
grainSecs (Minutes n) = fromIntegral n * 60
grainSecs (Seconds n) = n

toDouble :: NominalDiffTime -> Double
toDouble t =
    (/1000000000000.0) $
    fromIntegral (P.floor $ t P.* 1000000000000 :: Integer)

toDouble' :: DiffTime -> Double
toDouble' =
  (\x -> x / ((10 :: Double) P.^ (12 :: Integer))) . fromIntegral . fromEnum

fromDouble :: Double -> NominalDiffTime
fromDouble x =
  let d0 = ModifiedJulianDay 0
      days = floor (x/toDouble nominalDay)
      secs = x - fromIntegral days * toDouble nominalDay
      t0 = UTCTime d0 (picosecondsToDiffTime 0)
      t1 = UTCTime (addDays days d0) (picosecondsToDiffTime $ floor (secs / 1.0e-12))
  in diffUTCTime t1 t0

fromDouble' :: Double -> DiffTime
fromDouble' d = toEnum $ fromEnum $ d * ((10 :: Double) P.^ (12 :: Integer))

-- | add a TimeGrain to a UTCTime
--
-- >>> addGrain (Years 1) 5 (UTCTime (fromGregorian 2015 2 28) 0)
-- 2020-02-29 00:00:00 UTC
--
-- >>> addGrain (Months 1) 1 (UTCTime (fromGregorian 2015 2 28) 0)
-- 2015-03-31 00:00:00 UTC
-- 
-- >>> addGrain (Hours 6) 5 (UTCTime (fromGregorian 2015 2 28) 0)
-- 2015-03-01 06:00:00 UTC
-- 
-- >>> addGrain (Seconds 0.001) (60*1000+1) (UTCTime (fromGregorian 2015 2 28) 0)
-- 2015-02-28 00:01:00.001 UTC
-- 
addGrain :: TimeGrain -> Int -> UTCTime -> UTCTime
addGrain (Years n) x (UTCTime d t) =
    UTCTime (addDays (-1) $ addGregorianYearsClip (n*fromIntegral x) (addDays 1 d)) t
addGrain (Months n) x (UTCTime d t) =
    UTCTime (addDays (-1) $ addGregorianMonthsClip (fromIntegral (n*x)) (addDays 1 d)) t
addGrain (Days n) x (UTCTime d t) = UTCTime (addDays (fromIntegral x * fromIntegral n) d) t
addGrain g@(Hours _) x d = addUTCTime (fromDouble (fromIntegral x * grainSecs g)) d
addGrain g@(Minutes _) x d = addUTCTime (fromDouble (fromIntegral x * grainSecs g)) d
addGrain g@(Seconds _) x d = addUTCTime (fromDouble (fromIntegral x * grainSecs g)) d


addHalfGrain :: TimeGrain -> UTCTime -> UTCTime
addHalfGrain (Years n) (UTCTime d t) =
    UTCTime (addDays (-1) $ (if m'==1 then addGregorianMonthsClip 6 else identity) $
             addGregorianYearsClip d' (addDays 1 d)) t
  where
    (d',m') = divMod 2 n
addHalfGrain (Months n) (UTCTime d t) =
    UTCTime (addDays (if m'==1 then 15 else 0) {- sue me -} $
             addDays (-1) $
             addGregorianMonthsClip (fromIntegral d') (addDays 1 d)) t
  where
    (d',m') = divMod 2 n
addHalfGrain (Days n) (UTCTime d t) =
    (if m'== 1 then addUTCTime (fromDouble (0.5 * grainSecs (Days 1))) else identity) $
    UTCTime (addDays (fromIntegral d') d) t
  where
    (d',m') = divMod 2 n
addHalfGrain g@(Hours _) d = addUTCTime (fromDouble (0.5 * grainSecs g)) d
addHalfGrain g@(Minutes _) d = addUTCTime (fromDouble (0.5 * grainSecs g)) d
addHalfGrain g@(Seconds _) d = addUTCTime (fromDouble (0.5 * grainSecs g)) d

-- | compute the floor UTCTime based on the timegrain
--
-- >>> floorGrain (Years 5) (UTCTime (fromGregorian 1999 1 1) 0)
-- 1995-12-31 00:00:00 UTC
--
-- >>> floorGrain (Months 3) (UTCTime (fromGregorian 2016 12 30) 0)
-- 2016-09-30 00:00:00 UTC
--
-- >>> floorGrain (Days 5) (UTCTime (fromGregorian 2016 12 30) 1)
-- 2016-12-30 00:00:00 UTC
--
-- >>> floorGrain (Minutes 15) (UTCTime (fromGregorian 2016 12 30) (fromDouble' $ 15*60+1))
-- 2016-12-30 00:15:00 UTC
--
-- >>> floorGrain (Seconds 0.1) (UTCTime (fromGregorian 2016 12 30) 0.12)
-- 2016-12-30 00:00:00.1 UTC
--
floorGrain :: TimeGrain -> UTCTime -> UTCTime
floorGrain (Years n) (UTCTime d _) = UTCTime (addDays (-1) $ fromGregorian y' 1 1) 0
  where
    (y,_,_) = toGregorian (addDays 1 d)
    y' = fromIntegral $ 1 + n * floor (fromIntegral (y - 1) / fromIntegral n :: Double)
floorGrain (Months n) (UTCTime d _) = UTCTime (addDays (-1) $ fromGregorian y m' 1) 0
  where
    (y,m,_) = toGregorian (addDays 1 d)
    m' = fromIntegral $ (1 + fromIntegral n * floor (fromIntegral (m - 1) / fromIntegral n :: Double) :: Integer)
floorGrain (Days _) (UTCTime d _) = UTCTime d 0
floorGrain (Hours h) u@(UTCTime _ t) = addUTCTime x u
  where
    s = toDouble' t
    x = fromDouble $ fromIntegral ((h * 3600 * fromIntegral ((floor (s / (fromIntegral h*3600)) :: Integer)))) - s
floorGrain (Minutes m) u@(UTCTime _ t) = addUTCTime x u
  where
    s = toDouble' t
    x = fromDouble $ fromIntegral (m * 60 * fromIntegral (floor (s / (fromIntegral m*60)) :: Integer)) - s
floorGrain (Seconds secs) u@(UTCTime _ t) = addUTCTime x u
  where
    s = toDouble' t
    x = fromDouble $ (secs * fromIntegral (floor (s / secs) :: Integer)) - s

-- | compute the ceiling UTCTime based on the timegrain
--
-- >>> ceilingGrain (Years 5) (UTCTime (fromGregorian 1999 1 1) 0)
-- 2000-12-31 00:00:00 UTC
--
-- >>> ceilingGrain (Months 3) (UTCTime (fromGregorian 2016 12 30) 0)
-- 2016-12-31 00:00:00 UTC
--
-- >>> ceilingGrain (Days 5) (UTCTime (fromGregorian 2016 12 30) 1)
-- 2016-12-31 00:00:00 UTC
--
-- >>> ceilingGrain (Minutes 15) (UTCTime (fromGregorian 2016 12 30) (fromDouble' $ 15*60+1))
-- 2016-12-30 00:30:00 UTC
--
-- >>> ceilingGrain (Seconds 0.1) (UTCTime (fromGregorian 2016 12 30) 0.12)
-- 2016-12-30 00:00:00.2 UTC
--
ceilingGrain :: TimeGrain -> UTCTime -> UTCTime
ceilingGrain (Years n) (UTCTime d _) = UTCTime (addDays (-1) $ fromGregorian y' 1 1) 0
  where
    (y,_,_) = toGregorian (addDays 1 d)
    y' = fromIntegral $ 1 + n * ceiling (fromIntegral (y - 1) / fromIntegral n :: Double)
ceilingGrain (Months n) (UTCTime d _) = UTCTime (addDays (-1) $ fromGregorian y' m'' 1) 0
  where
    (y,m,_) = toGregorian (addDays 1 d)
    m' = fromIntegral n * ceiling (fromIntegral m / fromIntegral n :: Double) :: Integer
    (y',m'') = fromIntegral <$> if m' == 12 then (y+1,1) else (y,m'+1)
ceilingGrain (Days _) (UTCTime d t) = if t==0 then UTCTime d 0 else UTCTime (addDays 1 d) 0
ceilingGrain (Hours h) u@(UTCTime _ t) = addUTCTime x u
  where
    s = toDouble' t
    x = fromDouble $ fromIntegral (h * 3600 * fromIntegral (ceiling (s / (fromIntegral h*3600)) :: Integer)) - s
ceilingGrain (Minutes m) u@(UTCTime _ t) = addUTCTime x u
  where
    s = toDouble' t
    x = fromDouble $ fromIntegral (m * 60 * fromIntegral (ceiling (s / (fromIntegral m*60)) :: Integer)) - s
ceilingGrain (Seconds secs) u@(UTCTime _ t) = addUTCTime x u
  where
    s = toDouble' t
    x = fromDouble $ (secs * fromIntegral (ceiling (s / secs) :: Integer)) - s

-- | whether to include lower and upper times
data PosDiscontinuous = PosInnerOnly | PosIncludeBoundaries

-- | dates attached to charts are often discontinuous, but we want to smooth that reality over and show a continuous range on the axis
-- The assumption with getSensibleTimeGrid is that there is a list of discountinuous UTCTimes rather than a continuous range.  Output is a list of index points for the original [UTCTime] and label tuples, and a list of unused list elements.
--
-- >>> placedTimeLabelDiscontinuous PosIncludeBoundaries (Just "%d %b") 2 [UTCTime (fromGregorian 2017 12 6) 0, UTCTime (fromGregorian 2017 12 29) 0, UTCTime (fromGregorian 2018 1 31) 0, UTCTime (fromGregorian 2018 3 3) 0]
-- ([(0,"06 Dec"),(1,"31 Dec"),(2,"28 Feb"),(3,"03 Mar")],[])
--
placedTimeLabelDiscontinuous :: PosDiscontinuous -> Maybe Text -> Int -> [UTCTime] -> ([(Int, Text)], [UTCTime])
placedTimeLabelDiscontinuous posd format n ts = (zip (fst <$> inds') labels, rem')
  where
    l = minimum ts
    u = maximum ts
    (grain, tps) = sensibleTimeGrid InnerPos n (l, u)
    tps' = case posd of
      PosInnerOnly -> tps
      PosIncludeBoundaries -> [l] <> tps <> [u]
    (rem', inds) = L.fold (matchTimes tps') ts
    inds' = laterTimes inds
    fmt = case format of
      Just f -> Text.unpack f
      Nothing -> autoFormat grain
    labels = Text.pack . formatTime defaultTimeLocale fmt . snd <$> inds'

autoFormat :: TimeGrain -> String
autoFormat (Years x)
    | x == 1 = "%b %Y"
    | otherwise = "%Y"
autoFormat (Months _) = "%d %b %Y"
autoFormat (Days _) = "%d %b %y"
autoFormat (Hours x)
    | x > 3 = "%d/%m/%y %R"
    | otherwise = "%R"
autoFormat (Minutes _) = "%R"
autoFormat (Seconds _) = "%R%Q"

matchTimes :: [UTCTime] -> L.Fold UTCTime ([UTCTime], [(Int, UTCTime)])
matchTimes ticks = L.Fold step begin (\(p,x,_) -> (p,reverse x))
  where
    begin = (ticks,[],0)
    step ([], xs, n) _ = ([], xs, n)
    step (p:ps, xs, n) a
        | p == a = step (ps, (n,p):xs, n) a
        | p > a = (p:ps, xs, n + 1)
        | otherwise = step (ps, (n - 1,p):xs, n) a

laterTimes :: [(Int, a)] -> [(Int,a)]
laterTimes [] = []
laterTimes [x] = [x]
laterTimes (x:xs) = L.fold (L.Fold step (x,[]) (\(x0,x1) -> reverse $ x0:x1)) xs
  where
    step ((n,a), rs) (na, aa) = if na == n then ((na,aa),rs) else ((na,aa),(n,a):rs)

-- | compute a sensible TimeGrain and list of UTCTimes
--
-- >>> sensibleTimeGrid InnerPos 2 (UTCTime (fromGregorian 2016 12 31) 0, UTCTime (fromGregorian 2017 12 31) 0)
-- (Months 6,[2016-12-31 00:00:00 UTC,2017-06-30 00:00:00 UTC,2017-12-31 00:00:00 UTC])
--
-- >>> sensibleTimeGrid InnerPos 2 (UTCTime (fromGregorian 2017 1 1) 0, UTCTime (fromGregorian 2017 12 30) 0)
-- (Months 6,[2017-06-30 00:00:00 UTC])
--
-- >>>  sensibleTimeGrid UpperPos 2 (UTCTime (fromGregorian 2017 1 1) 0, UTCTime (fromGregorian 2017 12 30) 0)
-- (Months 6,[2017-06-30 00:00:00 UTC,2017-12-31 00:00:00 UTC])
--
-- >>>sensibleTimeGrid LowerPos 2 (UTCTime (fromGregorian 2017 1 1) 0, UTCTime (fromGregorian 2017 12 30) 0)
-- (Months 6,[2016-12-31 00:00:00 UTC,2017-06-30 00:00:00 UTC])
--
sensibleTimeGrid :: Pos -> Int -> (UTCTime, UTCTime) -> (TimeGrain, [UTCTime])
sensibleTimeGrid p n (l, u) = (grain, ts)
  where
    span = u `diffUTCTime` l
    grain = stepSensibleTime p span n
    first' = floorGrain grain l
    last' = ceilingGrain grain u
    n' = round $ toDouble (diffUTCTime last' first') / grainSecs grain :: Integer
    posns = case p of
      OuterPos -> take (fromIntegral $ n'+1)
      InnerPos -> drop (if first'==l then 0 else 1) . take (fromIntegral $ n' + if last'==u then 1 else 0)
      UpperPos -> drop 1 . take (fromIntegral $ n' + 1)
      LowerPos -> take (fromIntegral n')
      MidPos -> take (fromIntegral n')
    ts = case p of
      MidPos -> take (fromIntegral n') $ addHalfGrain grain . (\x -> addGrain grain x first') <$> [0..]
      _ -> posns $ (\x -> addGrain grain x first') <$> [0..]

-- come up with a sensible step for a grid over a Field
stepSensible ::
     (Fractional a, Ord a, FromInteger a, QuotientField a Integer, ExpField a)
  => Pos
  -> a
  -> Int
  -> a
stepSensible tp span n =
  step +
  if tp == MidPos
    then step / (one + one)
    else zero
  where
    step' = 10 ^^ floor (logBase 10 (span / fromIntegral n))
    err = fromIntegral n / span * step'
    step
      | err <= 0.15 = 10 * step'
      | err <= 0.35 = 5 * step'
      | err <= 0.75 = 2 * step'
      | otherwise = step'

-- come up with a sensible step for a grid over a Field, where sensible means the 18th century
-- practice of using multiples of 3 to round
stepSensible3 ::
     (Fractional a, Ord a, FromInteger a, QuotientField a Integer, ExpField a)
  => Pos
  -> a
  -> Int
  -> a
stepSensible3 tp span n =
  step +
  if tp == MidPos
    then step / (one + one)
    else zero
  where
    step' = 10 ^^ floor (logBase 10 (span / fromIntegral n))
    err = fromIntegral n / span * step'
    step
      | err <= 0.05 = 12 * step'
      | err <= 0.3 = 6 * step'
      | err <= 0.5 = 3 * step'
      | otherwise = step'

-- | come up with a sensible TimeGrain over a NominalDiffTime
stepSensibleTime :: Pos -> NominalDiffTime -> Int -> TimeGrain
stepSensibleTime tp span n
  | yearsstep >= 1 = Years (floor yearsstep)
  | monthsstep >= 1 = Months (fromIntegral $ (floor monthsstep :: Integer))
  | daysstep >= 1 = Days (fromIntegral $ (floor daysstep :: Integer))
  | hoursstep >= 1 = Hours (fromIntegral $ (floor hoursstep :: Integer))
  | minutesstep >= 1 = Minutes (fromIntegral $ (floor minutesstep :: Integer))
  | secondsstep >= 1 = Seconds secondsstep3
  | otherwise = Seconds secondsstep
  where
    sp = toDouble span
    minutes = sp / 60
    hours = sp / (60 * 60)
    days = sp / toDouble nominalDay
    years = sp / 365 / toDouble nominalDay
    months' = years * 12
    yearsstep = stepSensible tp years n
    monthsstep = stepSensible3 tp months' n
    daysstep = stepSensible tp days n
    hoursstep = stepSensible3 tp hours n
    minutesstep = stepSensible3 tp minutes n
    secondsstep3 = stepSensible3 tp sp n
    secondsstep = stepSensible tp sp n
