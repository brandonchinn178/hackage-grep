module HackageGrep (
  hackageGrep,
  hackageGrep_,

  -- * Options
  HackageGrepOptions (..),
  GrepOrderBy (..),
  defaultGrepOptions,
) where

import Data.Text (Text)

{-- Options --}

data HackageGrepOptions = HackageGrepOptions
  { packageOrderBy :: GrepOrderBy
  , packageLimit :: Maybe Int
  }
  deriving (Show)

data GrepOrderBy = MostDownloads | AToZ
  deriving (Show)

defaultGrepOptions :: HackageGrepOptions
defaultGrepOptions = HackageGrepOptions
  { packageOrderBy = MostDownloads
  , packageLimit = Nothing
  }

{-- Grep --}

-- | Grep for the given pattern and return the packages the pattern is found in with the
-- lines matching the pattern. The list of matched lines is guaranteed to be non-empty.
hackageGrep :: HackageGrepOptions -> Text -> IO [(Text, [Text])]
hackageGrep _ _ = return []

-- | Same as 'hackageGrep', except just returns the package name.
--
-- Faster than 'hackageGrep' as it exits after finding the first match.
hackageGrep_ :: HackageGrepOptions -> Text -> IO [Text]
hackageGrep_ opts pat = map fst <$> hackageGrep opts pat
