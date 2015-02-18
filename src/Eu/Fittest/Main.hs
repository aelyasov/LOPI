-- | lopi main file
module Main (main) where

import System.IO
import System.Directory
import Control.Monad
import Eu.Fittest.Options
-- import System.FilePath
import Eu.Fittest.RPInferencer 
import Eu.Fittest.Data
import Data.Function
import Data.Maybe
import Data.List
import qualified Eu.Fittest.Pretty.Events as PPEvt
import Text.PrettyPrint.HughesPJ (render)
import Eu.Fittest.Fittest2SimpleLogParser 
import Eu.Fittest.WitnessSetParser
import Eu.Fittest.EventRewriting
import Eu.Fittest.Filter
import Eu.Fittest.Abstraction
import Eu.Fittest.CrossCheck
import System.FilePath.Posix
import System.IO.Unsafe
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Exts
import Debug.Trace

import Eu.Fittest.Logging.XML.XMLparser
import Eu.Fittest.Logging.XML.ToFITTESTRawLog
import Eu.Fittest.Logging.Mutation.MyLogMutator
import Eu.Fittest.Logging.XML.Event

-- import Eu.Fittest.EventRewriting


main :: IO ()
main = do
  opts <- commandlineArgs

  -- | an auxilary function to read logs from either cmd or file
  let readLogs = if not $ null $ optInLogs opts
                 then return $ optInLogs opts
                 else do fcont <- readFile (optInLogsFile opts)
                         return $ lines fcont

  -- | rewrite rule inference
  -- | ========================================================== 
  when (optInference opts) $ do
    inFiles <- readLogs
    if optEventArgsAbst opts
      then mapM_ (convFittest2SimpleLog True)  inFiles
      else mapM_ (convFittest2SimpleLog False) inFiles
    inLogs <- mapM loadSimpleLog inFiles
    hPutStrLn stderr $ "*** Number of supplied logs: " ++ (show $ length inLogs)
    hPutStrLn stderr $ "*** Total length of the supplied logs: " ++ show (foldr (\x xs -> length x + xs) 0  inLogs)
    hPutStrLn stderr $ "*** The average length of the supplied logs: "
      ++ show ((fromInteger $ toInteger $ foldr (\x xs -> length x + xs) 0  inLogs)
               /
               (fromInteger $ toInteger $ length inLogs))
    let evts = nub $ concatMap getEventList inLogs
    hPutStrLn stderr $ "*** Number of unique events in the logs: " ++ (show $ length evts) 
    let abst = case optAbsLogs opts of
          Nothing   -> idAbs
          Just absF | null absF -> idAbs
                    | otherwise -> readAF absF
        inLogsA = map (runAbsFun abst) inLogs
        filter_ = case optFilterRules opts of
          Nothing       -> doesNotFilter
          Just filtPred -> readCP filtPred
    
        wSet = case optInferByStep opts of
          Just inf_step -> concatWS $ map (inferConAlgRRules (Filter filter_) evts) $ splitByStep inf_step inLogsA
          Nothing       -> if optInferAndMerge opts
                           then inferConAlgRRules (Filter filter_) evts inLogsA
                           else concatWS $ map (\l -> inferConAlgRRules (Filter filter_) evts [l]) inLogsA
        wSetStr = render $ PPEvt.ppWSet wSet
    case optRewRules opts of
      Nothing      -> hPutStrLn stdout wSetStr
      Just outFile -> do hPutStrLn stderr
                           $ "*** Number of inferred patterns: " ++ (show $ length wSet)
                         writeFile outFile wSetStr
                      -- withFile outFile WriteMode . flip hPrint wSetStr

  -- | event sequence reduction
  -- |  ==========================================================
  when (optReduction opts) $ do
    let inFiles = optInLogs opts
        inRules = case optRewRules opts of
          Nothing     -> error "No rewrite rules is supplied"
          Just rrules -> rrules
    if optEventArgsAbst opts
      then mapM_ (convFittest2SimpleLog True)  inFiles
      else mapM_ (convFittest2SimpleLog False) inFiles
    inLogs <- mapM loadSimpleLog inFiles
    rs     <- readFile inRules >>= return . map getRewRulPat . getWitnesses
    let evtLogs       = map (map event) inLogs
        reducedLogs   = map (runRewrite rs) evtLogs
        reducedEvents = map (show . getReducedCEvents) reducedLogs
        reducedInds   = map (show . getReducedID) reducedLogs
        outFiles      = case optOutLogs opts of
          Nothing   -> map (flip replaceExtension "rlog") inFiles
          Just outf | null outf -> map (flip replaceExtension "rlog") inFiles
                    | otherwise -> outf
    zipWithM_ writeFile outFiles reducedEvents
    mapM_ (hPutStrLn stdout) reducedInds
      
    -- need to describe oColRewStat option

  -- | intersect rewrite rules
  -- |  ==========================================================
  when (optCrossCheck opts) $ do
    let iargs = optCrossCheckRules opts
        infArgs@(inp1:inp2:outp:_) = iargs
    if (length infArgs /= 3)
      then fail $ "wrong number of arguments for I " ++
           (show $ length infArgs) ++ " instead of 3"
      else do rls1 <- readFile inp1 >>= return . getWitnesses
              rls2 <- readFile inp2 >>= return . getWitnesses
              let cross_res = crossCheck rls1 rls2
                  violN     = countNegWits cross_res
                  orclN     = length rls1
              hPutStrLn stderr $ "*** Total number of oracles: " ++ show orclN
              hPutStrLn stderr $ "*** Total number of violations: " ++ show violN
              writeFile outp $ concatMap (\x -> show x ++ "\n") cross_res
              when (optCountNegCrossValid opts) $ do
                hPutStrLn stdout $ show $ (((/) `on` (fromInteger . toInteger)) violN orclN)

  -- | merge witnesses resulted from multiple inferences
  -- |  ==========================================================
  when (optWitnessConcat opts) $ do
    rls <- liftM (mergeWS . getWitnesses) $ readFile (fromJust $ optRewRules opts)
    let filter_ = case optFilterRules opts of
          Nothing       -> doesNotFilter
          Just filtPred -> readCP filtPred
    hPutStrLn stdout $ render $ PPEvt.ppWSet $ filterWitnesses (Filter filter_) rls

  -- | extract set of events from logs
  -- |  ==========================================================
  when (optGetEvents opts) $ do
    inLFiles <- readLogs
    inExistFiles <- filterM (doesFileExist . replaceExtension "slog") inLFiles
    mapM_ (convFittest2SimpleLog False) (inLFiles \\ inExistFiles)                  
    inLogs <- mapM loadSimpleLog inLFiles
    hPutStrLn stderr $ "*** Number of supplied logs: " ++ (show $ length inLogs)
    hPutStrLn stderr $ "*** Total length of the supplied logs: " ++ show (foldr (\x xs -> length x + xs) 0  inLogs)
    hPutStrLn stderr $ "*** The average length of the supplied logs: "
      ++ show ((fromInteger $ toInteger $ foldr (\x xs -> length x + xs) 0  inLogs)
               /
               (fromInteger $ toInteger $ length inLogs))
    let evtsGroups = group $ sort $ concatMap (map (conEName . event)) inLogs
        evsGrRep = sortBy (compare `on` snd) $ map (\gr -> (head gr, length gr)) evtsGroups
    hPutStrLn stderr $ "*** Number of unique events in the logs: " ++ (show $ length evsGrRep)
    mapM_ (hPutStrLn stdout . show) evsGrRep
  
  -- | inference by evolution
  -- |  ==========================================================
  when (optEvoInference  opts) $ do
    inLFiles <- readLogs
    inRules  <- case optRewRules opts of
      Nothing     -> return $ error "No rewrite rules is supplied"
      Just rrules -> return $ rrules
    if optEventArgsAbst opts
      then mapM_ (convFittest2SimpleLog True)  inLFiles
      else mapM_ (convFittest2SimpleLog False) inLFiles
    inLogs   <- mapM loadSimpleLog inLFiles
    rules    <- readFile inRules >>= return . map getRewRulPat . getWitnesses
    let checkedWits = concatMap (flip countPats inLogs . (:[])) rules
        violations = filterWitnesses (Filter filterNegative) $ trace (render $ PPEvt.ppWSet checkedWits) checkedWits
    hPutStrLn stderr $ "*** The number of still valid rules: " ++ (show $ length violations)
    return ()

  -- | collect oracles metrics for evolvability measurment
  -- |  ==========================================================
  when (optEvoStats  opts) $ do
    let iargs = optCrossCheckRules opts
        infArgs@(inp1:inp2:_) = iargs
    if (length infArgs /= 2)
      then fail $ "wrong number of arguments for I " ++
           (show $ length infArgs) ++ " instead of 2"
      else do rls1 <- readFile inp1 >>= return . map getRewRulPat . getWitnesses
              rls2 <- readFile inp2 >>= return . map getRewRulPat . getWitnesses
              hPutStrLn stderr $ "*** The number of kept rules: " ++ (show $ length $ intersect rls1 rls2)
              hPutStrLn stderr $ "*** The number of added rules: " ++ (show $ length $ rls2 \\ rls1)
              hPutStrLn stderr $ "*** The number of removed rules: " ++ (show $ length $ rls1 \\ rls2)
    return ()     

  -- | generate mutation testing report
  when (optGenMutTestRep opts) $ do
    let orcFile    = fromJust $ optRewRules opts
        [mlogFile] = optInLogs opts
    oracles <- readFile orcFile >>= return . map getRewRulPat . getWitnesses
    mutLog  <- parseXMLlog mlogFile
    let mutations      = genAllMutants mutLog
        killedMutation = [zip (map getRewRulPat fWit) (repeat mt)
                         | (mt, evs) <- mutations
                         , let sLog = appEventLog2simpleLog $ map unAppEvent evs
                         , let fWit = filterWitnesses (Filter filterNonNegative) $ countPats oracles [sLog]
                         , not $ null fWit]
    hPutStrLn stderr $ "*** The total number of mutations: "
              ++ (show $ length mutations)
    hPutStrLn stderr $ "*** The number of killed mutations: "
              ++ (show $ length $ killedMutation)    
    mapM_ (\x -> print ((head x) ++ ": " ++ (show $ length x)))
      $ group
      $ sort
      $ map (snd . head)  killedMutation
    writeFile "killedMutants.txt" $ show killedMutation  

  -- | check invariants against new logs
  when (optCheckInvs opts) $ do
    let orcFile  = fromJust $ optRewRules opts
        logFiles = optInLogs opts
    oracles <- readFile orcFile >>= return . map (\w -> (getRewRulPat w, getPosWitnessCount w)) . getWitnesses
    logs <- mapM parseXMLlog logFiles
    let violations = [ zip3 (map getRewRulPat fWit) (map getWitnessType fWit) (repeat lfile)
                     | (log_, lfile) <- zip logs logFiles
                     , let sLog = appEventLog2simpleLog $ map unAppEvent log_
                     , let fWit = filterWitnesses (Filter filterNonNegative) $ countPats (map fst oracles) [sLog]
                     , not $ null fWit]
    mapM_ (\(rl, tp, p) -> print $ "Invariant " ++ (render $ PPEvt.ppRewRulPat rl) ++ " (observed #" ++ (show $ fromJust $ lookup rl oracles) ++ " times)" ++ " of type {" ++ (read tp) ++ "} is violated on the file " ++ p) $ concat violations   

  -- | print statistics related to skip-like rules
  when (optSkipLogStats opts) $ do
    let orcFile   = fromJust $ optRewRules opts
        [logFile] = optInLogs opts 
    oracles <- readFile orcFile >>= return . map getRewRulPat . getWitnesses
    log_    <- parseXMLlog logFile
    let evsInLog   = map (conEName . event) $ appEventLog2simpleLog $ map unAppEvent log_
        skipEvs    = map (\(lr :~: rr) -> symEName $ head lr) $ filter isSkip oracles
        skipEvsLog = filter (`elem` skipEvs) evsInLog         
    hPutStrLn stderr $ "*** The log consists of: " ++ (show $ length evsInLog) ++ " events"
    hPutStrLn stderr $ "*** The number of skip events in the log: " ++ (show $ length skipEvsLog)
    hPutStrLn stderr $ "*** The number of skip oracles is: " ++ (show $ length skipEvs)

  -- | new rewrite rule inference
  -- | ========================================================== 
  when (optNewInference opts > 0) $ do
    inFiles <- readLogs
    mapM_ (convFittest2SimpleLog False) inFiles
    inLogs  <- mapM loadSimpleLog inFiles
    hPutStrLn stderr $ "*** Number of supplied logs: " ++ (show $ length inLogs)
    hPutStrLn stderr $ "*** Total length of the supplied logs: " 
                  ++ show (foldr (\x xs -> length x + xs) 0  inLogs)
    hPutStrLn stderr $ "*** The average length of the supplied logs: "
      ++ show ((fromInteger $ toInteger $ foldr (\x xs -> length x + xs) 0  inLogs)
               /
               (fromInteger $ toInteger $ length inLogs))
    let evts    = nub $ concatMap getEventList inLogs
        ruleLen = optNewInference opts
        filter_ = case optFilterRules opts of
            Nothing       -> doesNotFilter
            Just filtPred -> readCP filtPred
        wSet    = inferConAlgRRulesNew ruleLen (Filter filter_) evts inLogs
        wSetStr = render $ PPEvt.ppWSet wSet
    case optRewRules opts of
      Nothing      -> hPutStrLn stdout wSetStr
      Just outFile -> do hPutStrLn stderr
                           $ "*** Number of inferred patterns: " ++ (show $ length wSet)
                         writeFile outFile wSetStr

    hPutStrLn stderr $ "*** Number of unique events in the logs: " ++ (show $ length evts)
                               

splitByStep :: (Eq a) => Int -> [[a]] -> [[[a]]]
splitByStep step = fix (\rec xs -> let (xs', ys) = (splitByStep' step xs)
                                   in  if ys == [] then [xs'] else (xs':rec ys))
  where
    splitByStep' :: Int -> [[a]] -> ([[a]], [[a]])
    splitByStep' _ []       = ([],[])
    splitByStep' 0 ass      = ([], ass)
    splitByStep' i (as:ass) = let iMin          = min (length as) i
                                  asNew         = take iMin as
                                  asRest        =  drop iMin as
                                  assNew        = if null asRest then ass else asRest:ass
                                  (psplit,rest) = splitByStep' (i - iMin) assNew
                              in  (asNew:psplit, rest)


unsafeInterleaveMapIO :: (a -> IO b) -> [a] -> IO [b]
unsafeInterleaveMapIO f (x:xs) = unsafeInterleaveIO $ do
  y <- f x
  ys <- unsafeInterleaveMapIO f xs
  return (y : ys)
unsafeInterleaveMapIO _ [] = return []
