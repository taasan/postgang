module TH where

import Data.Time
  ( getCurrentTime
  )
import Data.Time.Format.ISO8601
  ( iso8601Show
  )
import Language.Haskell.TH.Syntax
  ( Lift (lift)
  , Q
  , TExp
  , runIO
  , unsafeTExpCoerce
  )

tNow :: Q (TExp String)
tNow = unsafeTExpCoerce $ do
  now <- runIO getCurrentTime
  lift . iso8601Show $ now
