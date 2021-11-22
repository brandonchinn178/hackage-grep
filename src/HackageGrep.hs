{-# LANGUAGE ImportQualifiedPost #-}

module HackageGrep (
  hackageGrep,
  hackageGrep_,
  GrepResult (..),
  GrepResultLine (..),
  hackageGrepConduit,
  hackageGrepConduit_,

  -- * Options
  HackageGrepOptions (..),
  GrepOrderBy (..),
  defaultGrepOptions,
) where

import Conduit (ConduitT, (.|))
import Conduit qualified
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

data GrepResult = GrepResult
  { packageName :: Text
  , matchedLines :: [GrepResultLine] -- ^ Guaranteed to be non-empty
  }
  deriving (Show)

data GrepResultLine = GrepResultLine
  { filePath :: Text
  , lineNumber :: Int
  , lineContent :: Text
  }
  deriving (Show)

-- | Grep for the given pattern and return the packages the pattern is found in with the
-- lines matching the pattern.
hackageGrep :: HackageGrepOptions -> Text -> IO [GrepResult]
hackageGrep opts pat =
  Conduit.runConduit $
    hackageGrepConduit opts pat .| Conduit.sinkList

-- | Same as 'hackageGrep', except just returns the package name.
--
-- Faster than 'hackageGrep' as it exits after finding the first match.
hackageGrep_ :: HackageGrepOptions -> Text -> IO [Text]
hackageGrep_ opts pat = map packageName <$> hackageGrep opts pat

-- | Same as 'hackageGrep', except streaming the results back.
hackageGrepConduit :: HackageGrepOptions -> Text -> ConduitT i GrepResult IO ()
hackageGrepConduit _ _ = return ()

-- | Same as 'hackageGrep_', except streaming the results back.
hackageGrepConduit_ :: HackageGrepOptions -> Text -> ConduitT i Text IO ()
hackageGrepConduit_ opts pat = hackageGrepConduit opts pat .| Conduit.mapC packageName
