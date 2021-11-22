{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Conduit ((.|))
import Conduit qualified
import Data.Foldable (asum)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Options.Applicative

import HackageGrep

data Options = Options
  { grepOptions :: HackageGrepOptions
  , grepPattern :: Text
  , onlyPackageNames :: Bool
  }

parseOpts :: Parser Options
parseOpts = do
  packageOrderBy <-
    option parsePackageOrderBy . mconcat $
      [ long "order-packages-by"
      , help . unwords $
          [ "The order to grep packages by:"
          , "'downloads' = sort by # of downloads (default),"
          , "'alpha' = sort alphabetically"
          ]
      , metavar "{downloads,alpha}"
      , value MostDownloads
      ]
  packageLimit <-
    asum
      [ flag' Nothing . mconcat $
          [ long "no-limit"
          , help "Search all packages on Hackage (incompatible with --limit)"
          ]
      , fmap Just . option auto . mconcat $
          [ long "limit"
          , help "Specify the max numbers of packages to search"
          , metavar "N"
          , value 1000
          , showDefault
          ]
      ]
  onlyPackageNames <-
    switch . mconcat $
      [ long "packages-with-matches"
      , short 'l'
      , help "Only output the name of the package containing the pattern"
      , showDefault
      ]
  grepPattern <-
    strArgument . mconcat $
      [ help "The pattern to search for"
      , metavar "PATTERN"
      ]

  pure
    Options
      { grepOptions =
          defaultGrepOptions
            { packageOrderBy = packageOrderBy
            , packageLimit = packageLimit
            }
      , ..
      }
  where
    parsePackageOrderBy = maybeReader $ \case
      "downloads" -> Just MostDownloads
      "alpha" -> Just AToZ
      _ -> Nothing

main :: IO ()
main = do
  Options{..} <-
    execParser $
      info
        (helper <*> parseOpts)
        (fullDesc <> progDesc "Grep packages on Hackage")

  let renderResult = if onlyPackageNames then packageName else renderGrepResult

  Conduit.runConduit $
    hackageGrepConduit grepOptions grepPattern .| Conduit.mapM_C (Text.putStrLn . renderResult)

renderGrepResult :: GrepResult -> Text
renderGrepResult GrepResult{..} =
  Text.unlines $
    "===== " <> packageName : map renderGrepResultLine matchedLines
  where
    renderGrepResultLine GrepResultLine{..} =
      filePath <> ":" <> Text.pack (show lineNumber) <> ": " <> lineContent
