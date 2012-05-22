-- file: ch22/PodTypes.hs
module PodTypes where

data Podcast =
    Podcast {castId :: Integer, -- ^ Numeric ID for this podcast
             castURL :: String  -- ^ Its feed URL
            }
    deriving (Eq, Show, Read)

data Episode = 
    Episode {epId :: Integer,     -- ^ Numeric ID for this episode
             epCast :: Podcast, -- ^ The ID of the podcast it came from
             epURL :: String,     -- ^ The download URL for this episode
             epDone :: Bool       -- ^ Whether or not we are done with this ep
            }
    deriving (Eq, Show, Read)
