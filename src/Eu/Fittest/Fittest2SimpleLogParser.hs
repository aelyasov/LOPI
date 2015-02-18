{-# LANGUAGE Rank2Types, FlexibleContexts, MultiParamTypeClasses #-}

module Eu.Fittest.Fittest2SimpleLogParser 
       ( convFittest2SimpleLog
       , loadSimpleLog
       , getEventList
       , collectAllEvents
       , appEventLog2simpleLog
       ) where

import Eu.Fittest.Data
-- import Text.ParserCombinators.UU (mapM, P, parse_h)
-- import Text.ParserCombinators.UU.Utils hiding (runParser)
-- import Text.ParserCombinators.UU.Core
-- import Text.ParserCombinators.UU.BasicInstances hiding (Parser)
-- import Text.ParserCombinators.UU.Derived (pList, pMany, pPacked)
-- import Eu.Fittest.Logging.Compression.RawlogParser
import Data.List (isSuffixOf, isPrefixOf)


import Eu.Fittest.Logging.XML.AppEvent
import Eu.Fittest.Logging.Compression.Compress
import Eu.Fittest.Logging.XML.Base
import Eu.Fittest.Logging.XML.Value

import System.IO
import GHC.IO.Handle.Types
import Data.ListLike (ListLike)
import Data.Char
import System.FilePath.Posix
import qualified Data.Text.IO as T
import qualified Data.Text as Text

import Control.Exception.Base


loadSimpleLog :: FilePath -> IO Log
loadSimpleLog path = do log_str <- readFile $ replaceExtension path "slog"
                        return $! (read log_str :: Log)

takeDigits :: String -> (String, String)
takeDigits (s:ss) = let (ds, nds) = takeDigits ss
                    in  if isDigit s 
                        then (s:ds, nds)
                        else ([], s:ss)
                             
-- splitParam :: String -> (String, String, String)
splitParam st = let (lhs, rhs)   = break (\x -> x == '=') st
                    (dig, rest) = takeDigits $ tail rhs 
                in  (lhs ++ "=", dig, rest)


-- | to produce arguments for high-level events such as purchase we can apply special patch
-- | that does provide required abstraction. uncomment  the assignment to raw_log_ast that
-- | uses the function flexstorePatch
-- | the Boolean flag defines weather the event name should be generalized 
convFittest2SimpleLog ::  Bool -> FilePath -> IO ()
convFittest2SimpleLog abst path = 
  do rawLog <- readFile path
     writeFile npath $ show $ pureConversion $ rawLog
  where
      pureConversion log_ = let appEvtLog = rawLog2appEventLog log_
                                simpleLog = appEventLog2simpleLog appEvtLog
                            in  if abst
                                then generalizeLog simpleLog
                                else simpleLog     
      npath = replaceExtension path "slog"


rawLog2appEventLog :: String -> [AppEvent_]
rawLog2appEventLog strLog =
  let (dic, comprStrLog) = strCompress strLog    
      decStrLog = decompressTimeStamp comprStrLog
      appEventLog = uncurry (topRunCLparser (all_ $ parseAppEvent)) (dic, decStrLog) 
  in  appEventLog  

appEventLog2simpleLog :: [AppEvent_] -> Log
appEventLog2simpleLog = map convAppEvent where
  convAppEvent (AppEvent_ _ (Obj_ _ event') (Obj_ _ state')) = LogEntry (convState state') (convEvent event')
  convEvent evt = CEvent (convEvtName evt) (convEvtArgs evt)
  convEvtName (_:(FieldValTy_ _ etarget _):(FieldValTy_ _ etype _):_) = (read etype :: String) ++ (read etarget :: String)
  convEvtArgs (_:_:_:(FieldObj_ _ (Obj_ _ (_:eargs))):[]) = map convEvtArg eargs
  convEvtArg (FieldValTy_ _ earg _) = CEPrimArg earg
  convEvtArg _                      = error "the convertion for event types is not defined"
  convState (_:vars) = map convVar vars
  convVar (FieldValTy_ _ val _) = PrimVar val
  convVar (FieldObj_ _ (Obj_ _ (_:vars))) = ArrVar $ map convVar vars

splitLogs :: String -> [String]
splitLogs log = case span (/='\n') log of
      (ls,x1:x2:lss) -> if x2 == '\n'
                        then (ls:(splitLogs lss))
                        else let (hd:tl) = splitLogs lss
                             in  ((ls ++ [x1] ++ [x2] ++ hd):tl)
      (ls, rs)       -> [ls ++ rs]



-- | Some event names in log have suffixes that were added in their names  
-- | in order to pass extra parameter to the events. Generalization of events
-- | allow us to shift this parameters in to the event argument list.
generalizeLog :: Log -> Log
generalizeLog l = map (\(LogEntry st ev) -> LogEntry st (generalizeEvent ev)) l
  where
    generalizeEvent :: CEvent -> CEvent
    generalizeEvent (CEvent ename eargs) = let (ename', new_arg) = break (isDigit) ename
                                           in if ("" /= new_arg)
                                              then CEvent ename' ((CEPrimArg new_arg):eargs)
                                              else CEvent ename' eargs                                                 
-- | This is a testing function to get the list of all unique event names 
-- | contained in to the log. This list should contain all events that were 
-- | defined in test suite. 
getEventList :: Log -> [AEvent]
getEventList = foldl (\x y -> 
                       let ename = conEName $ event y
                       in if (ename `notElem` x)
                          then ename:x
                          else x
                     ) []


getEventListCountEvEccur :: Log -> [(AEvent, Int)]
getEventListCountEvEccur = foldl (\x y ->
                                   let ename = conEName $ event y
                                       (holds, nholds) = span ((/=ename) . fst) x
                                   in  if (null nholds)
                                       then (ename, 1):holds
                                       else holds ++ (fst $ head $ nholds, (snd $ head $ nholds) + 1):tail nholds
                                 ) []                                   

collectAllEvents :: FilePath -> IO [AEvent]
collectAllEvents path = 
  do log <-loadSimpleLog path
     let events = getEventList log
     writeFile (replaceExtension path "evt") $ show events
     return events 
                         
