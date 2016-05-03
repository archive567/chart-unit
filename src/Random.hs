{-# LANGUAGE OverloadedStrings #-}

-- random data sources

module Random where

-- import qualified Control.Foldl as L
import qualified Data.Vector.Unboxed as V
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWCD

mwc0 = MWC.initialize $ V.fromList [42]
{-# INLINE mwc0 #-}

-- random variate sources
rv :: Int -> IO (V.Vector Double)
rv n = do
    mwc <- mwc0
    V.replicateM n (MWCD.standard mwc)
{-# INLINE rv #-}

-- correlated pairs
rvcorr :: Int -> Double -> IO (V.Vector (Double,Double))
rvcorr n c = do
    mwc <- mwc0
    a0 <- V.replicateM n (MWCD.standard mwc)
    a1 <- V.replicateM n (MWCD.standard mwc)
    let a2 = V.zipWith (\x y -> c * x + sqrt (1 - c * c) * y) a0 a1
    return $ V.zip a0 a2
{-# INLINE rvcorr #-}

rvcorrL :: Int -> Double -> IO [(Double,Double)]
rvcorrL n c = do
    mwc <- mwc0
    a0 <- V.replicateM n (MWCD.standard mwc)
    a1 <- V.replicateM n (MWCD.standard mwc)
    let a2 = V.zipWith (\x y -> c * x + sqrt (1 - c * c) * y) a0 a1
    return $ V.toList $ V.zip a0 a2
{-# INLINE rvcorrL #-}
