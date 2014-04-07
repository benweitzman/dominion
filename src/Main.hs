import Data.Char
import Dominion hiding (value)
import Dominion.Cards
import Dominion.Strategies
import Options.Applicative
import Control.Monad.Logger

{- data LogLevel 
LevelDebug   
LevelInfo    
LevelWarn    
LevelError   
LevelOther Text  
-}


parseLogLevel :: String -> Maybe LogLevel
parseLogLevel s = lookup (map toLower s) logNames
    where logNames = [("debug", LevelDebug)
                     ,("info", LevelInfo)
                     ,("warn", LevelWarn)
                     ,("error", LevelError)
                     ]

options :: Parser Options
options = Options 
    <$> option
        ( long "iterations"
       <> short 'i'
       <> value 1000
       <> help "Number of games to play"
       <> metavar "ITERATIONS"
        )
    <*> many 
        ( strOption 
        $ long "require"
       <> short 'r'
       <> metavar "CARD"
       <> help "Require that particular card is included in each game"
        )
    <*> switch
        ( long "verbose"
       <> short 'v'
       <> help "Prints information about what each player is doing"
        )
    <*> option
        ( long "log"
       <> short 'l'
       <> value Nothing
       <> help "Log level"
       <> metavar "LOG_LEVEL"
       <> eitherReader (\s -> case parseLogLevel s of
                                Nothing -> Left "couldn't determine log level"
                                x -> Right x
                       )
        )


main = do config <- execParser opts
          dominionWithOpts config ["adit" `uses` bigMoney, "maggie" `uses` stupidStrategy]
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )
