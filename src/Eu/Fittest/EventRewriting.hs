module Eu.Fittest.EventRewriting where

import Eu.Fittest.Data
import GHC.Exts (groupWith)
import Data.List (find, nub, delete)
import Debug.Trace


type CEventID = (CEvent, Int)

runRewrite :: [RewRulPat] -> [CEvent] -> [CEventID]
runRewrite rrs event_log =  
  let skrs = map symRl2AlgRl $ getSkipRules rrs
      
      zrrs =  map symRl2AlgRl $ getZeroRules rrs
      zrrsE = zeroOverlap zrrs
      (skrsE, zrrsE') = skipAndZeroOverlap skrs zrrsE
      
      cmrs   = map symRl2AlgRl $ getComRules rrs

      izrrs  = inferIZero zrrsE' cmrs
      izrrsE = zeroOverlap izrrs
      (skrsE', izrrsE') = skipAndZeroOverlap skrsE izrrsE

      makeZeroCls = map (\cl -> (getAZero $ head cl, cl))
                    . groupWith (\(AZero _ y) -> y)         

      zrCls  = makeZeroCls zrrsE'
      izrCls = makeZeroCls izrrsE'

      event_log' = applySkip skrsE' $ zip event_log [0..]
  in 
   fixP ( reverse 
          . fixZeroRewrite izrCls cmrs [] 
          . reverse 
          . fixZeroRewrite zrCls cmrs []
        ) event_log'


getReducedCEvents :: [CEventID] -> [CEvent]
getReducedCEvents = map fst

getReducedID :: [CEventID] -> [Int]
getReducedID = map snd
  
applySkip :: [AlgRule] -> [CEventID] -> [CEventID]
applySkip skrs = foldr (\hd tl ->
                         if any (\(ASkip e) -> e == (conEName $ fst hd)) skrs
                         then tl
                         else hd:tl) []

type Pos   = ([CEventID], CEventID, [CEventID])

type ZClass = (AEvent,[AlgRule])

findFstZeroApp :: [(AEvent,[AlgRule])] -> [CEventID] -> Maybe (ZClass, Pos)
findFstZeroApp _    []  = Nothing
findFstZeroApp zcls (l:ls) = 
  let (CEvent e _) = fst l
      mcl = find (\(x, _) -> x == e) zcls
      findNext = findFstZeroApp zcls ls
  in case mcl of
    Just cl -> Just (cl, ([], l, ls))
    Nothing -> case findNext of
      Just (cl, (u,w,v)) -> Just (cl, (l:u,w,v))
      Nothing            -> Nothing  

applyZeroFix :: [AlgRule] -> [CEventID] -> [CEventID]
applyZeroFix zrs ls = fixP (applyZero zrs) ls
  where
    applyZero :: [AlgRule] -> [CEventID] -> [CEventID]
    applyZero _ [] = [] 
    applyZero rs log@(lg:lgs) = 
      let (CEvent e _) = fst lg
          zrels = map getAZeroElem rs
      in  if (e `elem` zrels)         
          then trace "zero rule application" lgs
          else log
          
fixP :: Eq a => (a -> a) -> a -> a
fixP f = until (\x -> f x == x) f

applyZeroCom :: [AlgRule] -> [AlgRule] -> [CEventID] -> [CEventID]
applyZeroCom zrrs cmrs log = 
  let findZeroCom :: [AlgRule] ->
                     ([CEventID], Maybe CEventID, [CEventID]) ->
                     ([CEventID], Maybe CEventID, [CEventID]) 
      findZeroCom _ (v, x, []) = (v, x, [])
      findZeroCom zrs (u, x, v)  = 
        let zrels = map getAZeroElem zrs
            hd = head v
            (CEvent e _) = fst hd
        in  if (e `elem` zrels) && (comWithAll cmrs e u)
            then trace "zero with com" (u, Just hd, tail v)
            else findZeroCom zrs (u ++ [hd], x, tail v)
      (pre, match, post) = findZeroCom zrrs ([], Nothing, log)
      comWithAll :: [AlgRule] -> AEvent -> [CEventID] -> Bool
      comWithAll crs e l = 
        let checkCom t evs = all (\(CEvent evn _, _) -> any (\(ACom e1 e2) -> 
                                                              (e1 == evn && e2 == t) 
                                                              || 
                                                              (e1 == t && e2 == evn)
                                                            ) crs
                                 ) evs
        in  checkCom e l
  in  case match  of
    Nothing -> pre
    Just m  -> pre ++ post
  
zeroRewrite :: [AlgRule] -> [AlgRule] -> [CEventID] -> [CEventID]
zeroRewrite zrrs cmrs = 
  fixP (applyZeroCom zrrs cmrs . applyZeroFix zrrs)
  
fixZeroRewrite :: [ZClass] -> [AlgRule] -> [CEventID] -> [CEventID] -> [CEventID]
fixZeroRewrite zcls cmrs acc evs = 
  case findFstZeroApp zcls evs of
    Just ((_,zrs),(pref,le,suff)) -> 
      let racc' = zeroRewrite zrs cmrs (reverse (acc ++ pref))
          acc'  = reverse racc' ++ [le]
      in  fixZeroRewrite zcls cmrs acc' suff      
    Nothing -> acc ++ evs  
    

inferIZero :: [AlgRule] -> [AlgRule] -> [AlgRule]
inferIZero zrs crs = foldr (\hr tr -> case checkIZero hr crs of
                               Just (AZero e z) -> (AZero e z):tr
                               Nothing          -> tr
                           )  [] zrs
  where
    checkIZero (AZero e z) comrs = if any (\(ACom c1 c2) -> 
                                            (e == c1 && z == c2)
                                            ||
                                            (e == c2 && z == c1)
                                          ) comrs
                                   then Just (AZero e z)
                                   else Nothing

zeroOverlap :: [AlgRule] -> [AlgRule]
zeroOverlap zrules = zeroOverlap' zrules zrules
  where
    zeroOverlap' :: [AlgRule]-> [AlgRule] -> [AlgRule]
    zeroOverlap' []       zrs1 = zrs1
    zeroOverlap' (zr:zrs) zrs1 = zeroOverlapOne zr zrs1 where
      zeroOverlapOne :: AlgRule -> [AlgRule] -> [AlgRule]
      zeroOverlapOne _ []               = zeroOverlap' zrs zrs1
      zeroOverlapOne r (r1:rs) = case checkZOverlap r r1 of
        Just r' -> if  r' `elem` zrs1
                   then zeroOverlap' zrs zrs1
                   else zeroOverlap (r':zrs1)
        Nothing -> zeroOverlapOne r rs

      checkZOverlap :: AlgRule -> AlgRule -> Maybe AlgRule
      checkZOverlap (AZero e z) (AZero e1 z1) | z  == e1  = Just $ AZero e z1
                                              | z1 == e   = Just $ AZero e1 z
                                              | otherwise = Nothing


skipAndZeroOverlap :: [AlgRule] -> [AlgRule] -> ([AlgRule], [AlgRule])
skipAndZeroOverlap skiprs zerors = skipAndZeroOverlap' skiprs zerors (skiprs, zerors) where
  skipAndZeroOverlap' [] _ res    = res
  skipAndZeroOverlap' (srule:srules) zrules res@(osrules, ozrules) = skipAndZeroOneOverlap srule zrules where
    skipAndZeroOneOverlap :: AlgRule -> [AlgRule] -> ([AlgRule], [AlgRule])
    skipAndZeroOneOverlap _ []       =  skipAndZeroOverlap' srules zrules res
    skipAndZeroOneOverlap sr (zr:zrs) = case checkSkipZeroOverlap sr zr of
      Just (sr', zr') -> if sr' `elem` osrules
                         then skipAndZeroOverlap' srules zrules res   
                         else skipAndZeroOverlap (sr':osrules) (delete zr' ozrules)
      Nothing         -> skipAndZeroOneOverlap sr zrs 

    checkSkipZeroOverlap :: AlgRule -> AlgRule -> Maybe (AlgRule, AlgRule)
    checkSkipZeroOverlap (ASkip e) r@(AZero a b) | e == b    = Just $ (ASkip a, r)
                                                 | otherwise = Nothing

    