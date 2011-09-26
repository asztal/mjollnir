module ID (
    ID (mkID),
    Unique, newUnique
    ) where

import Data.Unique (Unique, newUnique)

class ID a where
    mkID :: Unique -> a

instance ID Unique where
    mkID = id

