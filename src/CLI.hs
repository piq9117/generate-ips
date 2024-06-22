module CLI
  ( Command (..),
    GenerateIp (..),
    appParser,
  )
where

import Options.Applicative
  ( Parser,
    auto,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    option,
    progDesc,
    strOption,
  )

data Command = Generate GenerateIp
  deriving (Show)

data GenerateIp = GenerateIp
  { filepath :: Maybe FilePath,
    count :: Int
  }
  deriving (Show)

appParser :: IO Command
appParser =
  execParser $
    info
      (commandParser <**> helper)
      ( fullDesc
          <> progDesc "generate IPs"
          <> header "Generate IPv4"
      )

commandParser :: Parser Command
commandParser = Generate <$> generateIpParser

generateIpParser :: Parser GenerateIp
generateIpParser =
  GenerateIp
    <$> (optional $ strOption (long "filepath" <> help "output filepath"))
    <*> option auto (long "count" <> help "count of how many IPs to generate")
