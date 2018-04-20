{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Chart.Dhall where

import NumHask.Prelude hiding (Type)
import Chart
import Dhall
import Data.HashMap.Strict.InsOrd
import Dhall.Core hiding (Type)

instance (Interpret a) => Interpret (Pair a)
instance (Interpret a) => Interpret (Range a)


{-
dhallRect :: Type a -> Type (Rect a)
dhallRect a = Type extractOut expectedOut
  where
    extractOut (RecordLit fields) =
      Rect <$> ( Data.HashMap.Strict.InsOrd.lookup "_1" fields >>= extract a )
           <*> ( Data.HashMap.Strict.InsOrd.lookup "_2" fields >>= extract a )
           <*> ( Data.HashMap.Strict.InsOrd.lookup "_3" fields >>= extract a )
           <*> ( Data.HashMap.Strict.InsOrd.lookup "_4" fields >>= extract a )
    extractOut _ = Nothing

    expectedOut =
        Record
            (Data.HashMap.Strict.InsOrd.fromList
                [ ("_1", expected a)
                , ("_2", expected a)
                , ("_3", expected a)
                , ("_4", expected a)
                ]
            )

-}
