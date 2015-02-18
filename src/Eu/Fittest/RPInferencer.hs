{-# LANGUAGE ParallelListComp, TransformListComp, GADTs, PackageImports #-}

module Eu.Fittest.RPInferencer 
       ( skip
       , zero
       , idemp
       , ignore
       , com
       , mkWSet
       , checkRRPat
       , countPats 
       , returnNegWit
       , subseqsOfLog -- hide later on
       , inferConAlgRRules
       , inferConAlgRRulesNew
       ) where

import Eu.Fittest.Data
import Eu.Fittest.Filter
import Data.IntMap (IntMap) 
import qualified Data.IntMap as IntMap
import Debug.Trace
import Data.Maybe
import Data.Function
import GHC.Exts
import Data.List (groupBy, nub, (\\), nubBy)
-- import qualified PrettyEvents as PPEvt
--import qualified PrettyHOL as PPHol
import Eu.Fittest.Substrings
import Text.PrettyPrint.HughesPJ (render)
import "safe" Safe

-- ====================================================      
--        Interfer patterns using trie data structure 
--        (an efficient version of the algorithm)    
-- ==================================================== 

mapFromLog :: Log -> IntMap LogEntry
mapFromLog log_ = IntMap.fromList $ zip [0..] log_ 

subseqsOfLog :: Int -> Log -> [(Log,[Int])]
subseqsOfLog maxLength log_ = subseqsOfSeq maxLength log_

subseqsOfLog_simple :: [AEvent] -> Log -> [Int]
subseqsOfLog_simple as log = subseqsOfLog' as log 0 where
  -- subseqsOfLog' _ [] _ = []
  subseqsOfLog' a l i
    | length l >= length a = let lpref = take (length a) l
                                 lprefEvs = map (conEName . event) lpref
                             in  if (a == lprefEvs)
                                 then i:subseqsOfLog' a (tailNote "subseqsOfLog_simple: then" l) (i+1)
                                 else subseqsOfLog' a (tailNote "subseqsOfLog_simple: else" l) (i+1)
    | otherwise            = []                                




-- | This function is used to construct witness set for a given rewrite rule.
-- | It takes 3 arguments: a rewrite rule, a log and the list of all sublogs
-- | with its starting possitions. 
mkWSet :: RewRulPat -> [Log] -> [(Log,Log)]
mkWSet pat_ logs_ = map (\((_,_,ls),(_,_,ls')) -> (ls, ls')) $ nubBy nubByFun $ mkWSet' pat_ logs_
  where 
    nubByFun ((i11,i12,w1),(i21,i22,w2)) ((i11',i12',w1'),(i21',i22',w2'))
      = ( i11 == i11' && i12 == i12' && w1 == w1' &&
          i21 == i21' && i22 == i22' && w2 == w2'
        ) ||
        ( i11 == i21' && i12  == i22' && w1 == w2' &&
          i21 == i11' && i22' == i12' && w2 == w1')
                                  
    mkWSet' :: RewRulPat -> [Log] -> [((Int, Int,Log),(Int, Int,Log))]
    mkWSet' (pl :~: pr) logs
      | null pr || null pl = [ (lhs, (i,j, [headNote "empty list in mkWSet'" lhs'])) | lhs@(i,j,lhs') <- segments pl]
      | otherwise      = [(lhs, rhs) | lhs@(_,_,lhs') <- segments pl
                                     , rhs@(_,_,rhs') <- segments pr
                                     , lhs /= rhs
                                     , (state $ headNote "empty lhs" lhs') 
                                       == 
                                       (state $ headNote "empty rhs" rhs')]
      where
                    
        subPos :: Pat -> Log -> [Int]
        subPos p l | length l == 1 = []
                   | otherwise     = snd $ headDef ([],[]) [ (seg,pos) 
                                                         | (seg,pos) <- subseqsOfLog 2 l -- subseqsOfLog_simple (map symEName p) l
                                                         , map (conEName . event) seg == map symEName p
                                                         ]
    
        takeSubstr ::  [a] -> Int -> Int -> [a]
        takeSubstr as d i | i + d <= length as && i >= 0 = take d $ drop i as
                          | otherwise         = []

        segments :: Pat -> [(Int, Int, Log)]
        segments p = filter (\(_,_,ls) -> ls /= [])
                     $ concatMap (\(i, l) ->
                                   map (\j ->
                                         (i, j, takeSubstr l (length p + 1) (j - 1))) $  subseqsOfLog_simple (map symEName p) l) $ zip [0 ..] logs  -- subPos p l
    
-- =====================================================================================
-- The code below is commented because the check it introduces make sense only if you
-- match symbolic arguments with the real once. This option can be implemented in the
-- future. Then the 'othrewise' case of the function mkWSet' should be extened with
-- the following condition: checkSegsWithPat' (pl :~: pr)  (tail lhs') (tail rhs')          
-- ===================================================================================== 
-- checkSegsWithPat' :: RewRulPat -> Log -> Log -> Bool
-- checkSegsWithPat' p lseg rseg = 
--   let levs   = map event lseg  
--       revs   = map event rseg
--       symMap = pat2IntMap p
--       conMap = cevents2IntMap (levs ++ revs)
--       conMapLen = IntMap.size conMap - 1
--       symMapLen = IntMap.size symMap - 1
--       addConstr i b = 
--         let (map_before, Just v, _) = IntMap.splitLookup i symMap 
--             index = findWithDefaultValue i v map_before 
--         in b && (conMap IntMap.! i == conMap IntMap.! index)
--   in (foldr addConstr True [0..(min conMapLen symMapLen)])

-- findWithDefaultValue :: Eq a => Int -> a -> IntMap a -> Int
-- findWithDefaultValue def v m = let r = [k | (k, v') <- (IntMap.toList m), v == v']
--                                in case r of
--                                    [] -> def
--                                    _  -> head r
                                    
-- pat2IntMap :: RewRulPat -> IntMap String
-- pat2IntMap (lp :~: rp) = snd $ foldr foldPat (0, IntMap.empty) (lp ++ rp)
--   where
--     foldPat :: SEvent -> (Int, IntMap String) -> (Int, IntMap String)
--     foldPat (SEvent en eargs) (i, map) = (i+2, IntMap.insert (i+1) (symEvtArgs eargs) (IntMap.insert i en map))
    
-- cevents2IntMap :: [CEvent] -> IntMap [String]
-- cevents2IntMap cevs = snd $ foldr foldCEvents (0, IntMap.empty) cevs
--   where
--     foldCEvents :: CEvent -> (Int, IntMap [String]) -> (Int, IntMap [String])
--     foldCEvents (CEvent en eargs) (i, map) = (i+2, IntMap.insert (i+1) eargs (IntMap.insert i [en] map))
-- =====================================================================================
    
checkRRPat :: RewRulPat -> [Log] -> Int -> (Bool, RewRulPat)
checkRRPat pat logs n = 
  let wSet =  [(lhs, rhs)| (lhs, rhs) <- mkWSet pat logs 
                         , (state $ last lhs) 
                           == 
                           (state $ last rhs)
                         ]
  in if (length wSet >= n)
     then (True,  pat)
     else (False, pat)
          
returnNegWit :: RewRulPat -> [Log] -> [(Log, Log)]
returnNegWit pat logs = 
  [(lhs, rhs)| (lhs, rhs) <- mkWSet pat logs
             , (state $ last lhs) 
               /= 
               (state $ last rhs)
             ]

-- Added filltering of completely equal witnesses. They should be counted as one 
-- witness. Because if we assume that corresponding real initial and final state 
-- are the same, it doesn't give us much more confidence to accept given pattern.
countPats :: [RewRulPat] -> [Log] -> [Witness]
countPats pats logs = 
  let countPat (pat, iter) = -- trace ("pattern #" ++ (show iter)) $
        WitnessC 
        (getPattern_ pat) 
        pat
        ( foldr (\(lhs, rhs) (pos, neg) -> 
                  if (state $ lastNote "RPInferencer: last state of lhs: " lhs)
                     ==
                     (state $ lastNote ("RPInferencer: last state of rhs\n"
                                        ++ show lhs
                                        ++ ":~:"
                                        ++ show rhs
                                        ++ "\n"
                                        ++ (show $ mkWSet pat logs)
                                       ) rhs)
                  then (pos+1, neg)
                  else (pos, neg+1)
                ) (0,0) (mkWSet pat logs)
        )
  in map countPat $ zip pats [1..]
     

-- | if it works then try to drop the arguments to make this function more nicely looking
-- | to think about substituting this function into returnWitness
rewRulePatInf :: Show t => ([AEvent] -> [RRPattern RewRulPat t]) -> [AEvent] -> [Log] -> [Witness]
rewRulePatInf pat aevts logs = countPats (map getRule $ pat aevts) logs

returnWitnesses :: (Show t) => Filter -> ([AEvent] -> [RRPattern RewRulPat t]) ->  [AEvent] -> [Log] -> [Witness]
returnWitnesses flt pat aevts logs = filterWitnesses flt $ rewRulePatInf pat aevts logs


inferConAlgRRules :: Filter -> [AEvent] -> [Log] -> Witnesses 
inferConAlgRRules flt evs logs =
  let skpRs          = returnWitnesses flt skip evs logs
      skpEvs         = map (symEName . head . getLhsRewRulPat . getRewRulPat) skpRs
      nonSkpEs       = evs \\ skpEvs
      -- nonSkpEs       = evs
      zrRs           = returnWitnesses flt zero   nonSkpEs logs 
      idpRs          = returnWitnesses flt idemp  nonSkpEs logs 
      -- ignRs          = returnWitnesses flt ignore nonSkpEs logs
      comRs          = returnWitnesses flt com    nonSkpEs logs
      invRs          = returnWitnesses flt inv    nonSkpEs logs
      -- skpRs          = []
      -- zrRs           = []
      -- idpRs          = []
      ignRs          = []
      -- comRs          = []
      -- invRs          = []
  in  concat [skpRs, zrRs, idpRs, ignRs, comRs, invRs]


inferConAlgRRulesNew :: Int -> Filter -> [AEvent] -> [Log] -> Witnesses 
inferConAlgRRulesNew lenN flt evs logs = 
    let rules = genRuleCandidates lenN logs
    in filterWitnesses flt $ countPats (trace ("*** Number of candidates: " ++ (show $ length rules)) rules) logs
    

