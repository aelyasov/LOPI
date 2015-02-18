module Eu.Fittest.Options where

import System.Console.GetOpt
import System.Environment
import System.FilePath
import System.IO
-- import System.Directory
-- import Data.ByteString.Lazy(ByteString)
-- import qualified Data.ByteString.Lazy as L
-- import LogPointsLib
-- import Control.DeepSeq
-- import Control.Monad
import Debug.Trace

data ToolType = Infer | Rewrite | Compare

data Options = 
  Options { optInference            :: Bool
          , optInferByStep          :: Maybe Int                        
          , optReduction            :: Bool
          , optCrossCheck           :: Bool
          , optInLogsFile           :: FilePath  
          , optInLogs               :: [FilePath]
          , optOutLogs              :: Maybe [FilePath]
          , optRewRules             :: Maybe FilePath
          , optFilterRules          :: Maybe String
          , optAbsLogs              :: Maybe String  
          , optColRewStat           :: Bool
          , optCrossCheckRules      :: [FilePath]
          , optEventArgsAbst        :: Bool
          , optInferAndMerge        :: Bool
          , optCountNegCrossValid   :: Bool
          , optWitnessConcat        :: Bool  
          -- , optHtmlCrossCheckReport :: Bool
          , optGetEvents            :: Bool
          , optEvoInference         :: Bool
          , optEvoStats             :: Bool
          , optGenMutTestRep        :: Bool
          , optCheckInvs            :: Bool                                            
          , optSkipLogStats         :: Bool
          , optNewInference         :: Int
          }
  
opts :: [OptDescr (Options -> IO Options)]
opts = [ Option "i" [] (NoArg oInference) "infer rewrite rules from logs"
       , Option ""  ["inf-step"] (ReqArg oInferByStep "int") "infer rewrite rules spliting the logs by the length given as argument to this option"
       , Option "r" [] (NoArg oReduction) "apply reduction procedure to logs"
       , Option "c" [] (NoArg oCrossCheck) "cross check two set of rewrite rules"  
       , Option ""  ["in-logs"] (ReqArg oInLogs "files") "input log files"
       , Option ""  ["in-logs-file"] (ReqArg oInLogsFile "file") "file containg path to all input log files"  
       , Option ""  ["out-logs"] (ReqArg oOutLogs "files") "output log files"
       , Option ""  ["rew-rules"] (ReqArg oRewRules "file") "output rewrite rules"
       , Option "f" ["filter-rules"] (ReqArg oFilterRules "ffunc") "filter rewrite rules according to a predefined filter function"
       , Option "a" ["abs-logs"] (ReqArg oAbsLogs "afunc") "apply abstraction function afunc to the logs before inference"
       , Option "s" ["col-rew-stats"] (NoArg oColRewStat) "collect rewriting statistics"
       , Option ""  ["cross-check-rules"] (ReqArg oCrossCheckRules "inp1 inp2 out") "cross check two set of inferred rewrite rules against of each-other"
       , Option "e" ["earg-abs"] (NoArg oEventArgsAbst) "apply abstraction to event names"
       , Option "m" ["infer-merge"] (NoArg oInferAndMerge) "infer rules from multiple files and combine results"
       , Option "n" ["count-neg-cross-valid"] (NoArg oCountNegCrossValid) "count negative witnesses after cross validation"
       , Option "" ["witness-concat"] (NoArg oWitnessConcat) "concatenate witnesses in a given file" 
       , Option "" ["get-events"] (NoArg oGetEvents) "extract the set of events from logs"
       , Option  "" ["evo-infer"] (NoArg oEvoInference) "infer new set of rewrite rules by evolving an old one"
       , Option "" ["evo-stats"] (NoArg oEvoStats) "collect evolvability metrics for orcales"
       , Option "" ["gen-mut-report"] (NoArg oGenMutTestRep) "generate mutation testing report"
       , Option "" ["check-invs"] (NoArg oCheckInvs) "check invariants (rewrite-rules) against new logs"
       , Option "" ["skip-log-stats"] (NoArg oSkipLogStats) "print statistics about the number of skip-like events in the log" 
       , Option "" ["new-infer"] (ReqArg oNewInference "N") "exhaustively infer rewrite rules of maximal length N" 
       ]

{-
oToolType :: String -> Options -> IO Options
oToolType s o = do
  case s of
    "i" -> return (o {optToolType = Infer})
    "r" -> return (o {optToolType = Rewrite})
    "c" -> return (o {optToolType = Compare})
    _   -> fail  ("no such an option"  ++ (usageInfo "Usage: toolname <option ...> <log file> ..." opts))
-}

oInference :: Options -> IO Options
oInference o = return (o {optInference = True})

oInferByStep :: String -> Options -> IO Options
oInferByStep s o = return (o {optInferByStep = Just (read s :: Int)})

oReduction :: Options -> IO Options
oReduction o = return (o {optReduction = True})

oCrossCheck :: Options -> IO Options
oCrossCheck o = return (o {optCrossCheck = True})

oInLogs :: FilePath -> Options -> IO Options
oInLogs s o = return (o {optInLogs = words s})

oInLogsFile :: FilePath -> Options -> IO Options
oInLogsFile s o = return (o {optInLogsFile = s})


oOutLogs :: FilePath -> Options -> IO Options
oOutLogs s o = return (o {optOutLogs = Just $ words s})

oRewRules :: FilePath -> Options -> IO Options
oRewRules s o = return (o {optRewRules = Just s})

oFilterRules :: String -> Options -> IO Options
oFilterRules s o = return (o {optFilterRules = Just s})

oAbsLogs :: String -> Options -> IO Options
oAbsLogs s o = return (o {optAbsLogs = Just s})

oColRewStat :: Options -> IO Options
oColRewStat o = return (o {optColRewStat = True})

oCrossCheckRules :: String -> Options -> IO Options
oCrossCheckRules s o = return (o {optCrossCheckRules = words s})

oEventArgsAbst :: Options -> IO Options
oEventArgsAbst o = return (o {optEventArgsAbst = True})

oInferAndMerge:: Options -> IO Options
oInferAndMerge o = return (o {optInferAndMerge = True})

oCountNegCrossValid:: Options -> IO Options
oCountNegCrossValid o = return (o {optCountNegCrossValid = True})

oWitnessConcat:: Options -> IO Options
oWitnessConcat o = return (o {optWitnessConcat = True})

oGetEvents :: Options -> IO Options
oGetEvents o = return (o {optGetEvents = True})

oEvoInference :: Options -> IO Options
oEvoInference o = return (o {optEvoInference = True})

oEvoStats :: Options -> IO Options
oEvoStats o = return (o {optEvoStats = True})

oGenMutTestRep :: Options -> IO Options
oGenMutTestRep o = return (o {optGenMutTestRep = True})

oCheckInvs :: Options -> IO Options
oCheckInvs o = return (o {optCheckInvs = True})

oSkipLogStats :: Options -> IO Options
oSkipLogStats o = return (o {optSkipLogStats = True})

oNewInference :: String -> Options -> IO Options
oNewInference s o = return (o {optNewInference = (read s :: Int)})

defaultOpts :: Options
defaultOpts = 
  Options { optInference          = False
          , optInferByStep        = Nothing
          , optReduction          = False
          , optCrossCheck         = False
          , optInLogs             = []
          , optInLogsFile         = []
          , optOutLogs            = Nothing
          , optRewRules           = Nothing
          , optFilterRules        = Nothing
          , optAbsLogs            = Nothing
          , optColRewStat         = False
          , optCrossCheckRules    = []
          , optEventArgsAbst      = False
          , optInferAndMerge      = False
          , optCountNegCrossValid = False
          , optWitnessConcat      = False
          , optGetEvents          = False
          , optEvoInference       = False
          , optEvoStats           = False
          , optGenMutTestRep      = False
          -- , optHtmlCrossCheckReport = False
          , optCheckInvs          = False
          , optSkipLogStats       = False
          , optNewInference       = 0                          
          }
  
commandlineArgs :: IO Options
commandlineArgs = 
  do args <- getArgs
     let usage = usageInfo "Usage: toolname <option ...> <log file> ..." opts
     case getOpt Permute opts args of
       (op, nop, []) -> foldl (>>=) (return $ defaultOpts) op
       (_, _, errs)  -> do hPutStrLn stderr (unlines errs ++ "\n" ++ usage)
                           return defaultOpts

  -- (op, nop, []) | null nop  -> foldl (>>=) (return $ patch $ defaultOpts) op
  --               | otherwise -> foldl (>>=) (return $ patch $ defaultOpts {optSourceLog = head nop}) op
  -- where 
  --   patch o = if optSourceLog o /= ""
  --             then o { optOutputLog = Just $ replaceExtension (optSourceLog o) ".pat"}
  --             else o
