{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable (asum)
import Data.Text (Text)
import Options.Applicative

import HackageGrep

parseOpts :: Parser (HackageGrepOptions, Text)
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
  pat <-
    strArgument . mconcat $
      [ help "The pattern to search for"
      , metavar "PATTERN"
      ]

  pure (HackageGrepOptions{..}, pat)
  where
    parsePackageOrderBy = maybeReader $ \case
      "downloads" -> Just MostDownloads
      "alpha" -> Just AToZ
      _ -> Nothing

main :: IO ()
main = do
  (opts, pat) <-
    execParser $
      info
        (helper <*> parseOpts)
        (fullDesc <> progDesc "Grep packages on Hackage")
  print (opts, pat)
  matches <- hackageGrep opts pat
  print matches
