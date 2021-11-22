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
import Data.Aeson (FromJSON (..), eitherDecode, withObject, (.:))
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as TextL
import Data.Text.Lazy.Encoding qualified as TextL
import Network.HTTP.Client (
  Manager,
  Request (..),
  Response (..),
  httpLbs,
  parseUrlThrow,
 )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (hAccept)
import Text.HTML.TagSoup qualified as TagSoup
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
  manager <- liftIO newTlsManager
  packages <- liftIO $ takeMaybe packageLimit <$> getAllPackages manager
  Conduit.yieldMany packages .| Conduit.concatMapMC (\package -> grepPackage manager package pat)
  where
    takeMaybe Nothing = id
    takeMaybe (Just x) = take x

    getAllPackages =
      case packageOrderBy of
        MostDownloads -> getAllPackagesByMostDownloads
        AToZ -> getAllPackagesAlphabetical

-- | Same as 'hackageGrep_', except streaming the results back.
hackageGrepConduit_ :: HackageGrepOptions -> Pattern -> ConduitT i PackageName IO ()
hackageGrepConduit_ opts pat = hackageGrepConduit opts pat .| Conduit.mapC packageName

grepPackage :: Manager -> PackageName -> Pattern -> IO (Maybe GrepResult)
grepPackage manager package pat =
  withSystemTempDirectory (Text.unpack $ "hackage-grep-" <> package) $ \dir -> do
    downloadPackage manager dir package
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

-- https://github.com/haskell/hackage-server/issues/458#issuecomment-975995226
getAllPackagesByMostDownloads :: Manager -> IO [PackageName]
getAllPackagesByMostDownloads manager = do
  resp <- queryHackage manager "/packages/top/"
  let tags = TagSoup.parseTags $ TextL.decodeUtf8 resp
      packageNames = mapMaybe getPackageName $ getElems "tr" tags
  return $ map TextL.toStrict packageNames
  where
    getPackageName row =
      case getElems "a" row of
        [] -> Nothing
        [anchorElem] -> Just $ TagSoup.innerText anchorElem
        _ -> error $ "Unexpectedly got multiple <a> tags in single row: " ++ show row

    getElems name =
      map (takeUntil (TagSoup.isTagCloseName name))
      . TagSoup.partitions (TagSoup.isTagOpenName name)

    takeUntil :: (a -> Bool) -> [a] -> [a]
    takeUntil _ [] = []
    takeUntil f (x:xs)
      | f x = [x]
      | otherwise = x : takeUntil f xs

newtype PackageNameJSON = PackageNameJSON { toPackageName :: PackageName }
  deriving (Show)

instance FromJSON PackageNameJSON where
  parseJSON = withObject "PackageName" $ \o ->
    PackageNameJSON <$> o .: "packageName"

getAllPackagesAlphabetical :: Manager -> IO [PackageName]
getAllPackagesAlphabetical manager = do
  resp <- queryHackage manager "/packages/"
  either error (return . map toPackageName) . eitherDecode $ resp

-- TODO
downloadPackage :: Manager -> FilePath -> PackageName -> IO ()
downloadPackage _ _ _ = return ()

queryHackage :: Manager -> Text -> IO ByteString
queryHackage manager path = do
  reqBase <- parseUrlThrow "https://hackage.haskell.org"
  let req =
        reqBase
          { path = Text.encodeUtf8 path
          , requestHeaders =
              [ (hAccept, "application/json")
              ]
          }
  responseBody <$> httpLbs req manager
