{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HackageGrep (
  hackageGrep,
  hackageGrep_,
  GrepResult (..),
  GrepResultLine (..),
  hackageGrepConduit,
  hackageGrepConduit_,

  -- * Options
  HackageGrepOptions (..),
  OrderPackagesBy (..),
  defaultGrepOptions,
) where

import Conduit (ConduitT, (.|))
import Conduit qualified
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Read (readMaybe)
import UnliftIO.Process (readProcessWithExitCode)
import UnliftIO.Temporary (withSystemTempDirectory)

{-- Options --}

data HackageGrepOptions = HackageGrepOptions
  { packageOrderBy :: OrderPackagesBy
  , packageLimit :: Maybe Int
  }
  deriving (Show)

data OrderPackagesBy = MostDownloads | AToZ
  deriving (Show)

defaultGrepOptions :: HackageGrepOptions
defaultGrepOptions = HackageGrepOptions
  { packageOrderBy = MostDownloads
  , packageLimit = Nothing
  }

{-- Grep --}

type PackageName = Text
type Pattern = Text

data GrepResult = GrepResult
  { packageName :: PackageName
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
hackageGrep :: HackageGrepOptions -> Pattern -> IO [GrepResult]
hackageGrep opts pat =
  Conduit.runConduit $
    hackageGrepConduit opts pat .| Conduit.sinkList

-- | Same as 'hackageGrep', except just returns the package name.
--
-- Faster than 'hackageGrep' as it exits after finding the first match.
hackageGrep_ :: HackageGrepOptions -> Pattern -> IO [PackageName]
hackageGrep_ opts pat = map packageName <$> hackageGrep opts pat

-- | Same as 'hackageGrep', except streaming the results back.
hackageGrepConduit :: HackageGrepOptions -> Pattern -> ConduitT i GrepResult IO ()
hackageGrepConduit HackageGrepOptions{..} pat = do
  packages <- takeMaybe packageLimit <$> liftIO (getAllPackages packageOrderBy)
  Conduit.yieldMany packages .| Conduit.concatMapMC (flip grepPackage pat)
  where
    takeMaybe Nothing = id
    takeMaybe (Just x) = take x

-- | Same as 'hackageGrep_', except streaming the results back.
hackageGrepConduit_ :: HackageGrepOptions -> Pattern -> ConduitT i PackageName IO ()
hackageGrepConduit_ opts pat = hackageGrepConduit opts pat .| Conduit.mapC packageName

grepPackage :: PackageName -> Pattern -> IO (Maybe GrepResult)
grepPackage package pat =
  withSystemTempDirectory (Text.unpack $ "hackage-grep-" <> package) $ \dir -> do
    downloadPackage dir package
    (_, out, _) <- readProcessWithExitCode "grep" ["-rnI", Text.unpack pat, dir] ""
    let result =
          GrepResult
            { packageName = package
            , matchedLines = map parseResultLine . Text.lines . Text.pack $ out
            }
    return $ if null (matchedLines result) then Nothing else Just result
  where
    parseResultLine line =
      case Text.splitOn ":" line of
        filePath : lineNumberStr : rest
          | Just lineNumber <- readMaybe $ Text.unpack lineNumberStr
          , lineContent <- Text.intercalate ":" rest
          -> GrepResultLine{..}
        _ -> error $ "grep returned output in unknown format: " ++ show line

{-- Hackage API --}

-- TODO
getAllPackages :: OrderPackagesBy -> IO [PackageName]
getAllPackages _ = return []

-- TODO
downloadPackage :: FilePath -> PackageName -> IO ()
downloadPackage _ _ = return ()
