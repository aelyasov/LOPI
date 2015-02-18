{-# LANGUAGE TypeFamilies, GADTs, ParallelListComp, RankNTypes, ExistentialQuantification, StandaloneDeriving #-}

module Eu.Fittest.Data where

import Data.List 
import Data.Tuple (swap)
import Data.Hashable
import Debug.Trace
import Data.Function

-- abstact event representation
type AEvent = String

-- concrete event represenation
data CEArg = CEPrimArg String
           | CEArrArg [CEArg]
           deriving (Show, Eq, Read)

type CEArgs = [CEArg] -- use string representation for integers
data CEvent = CEvent { conEName :: AEvent 
                     , conEArgs :: CEArgs
                     }
              deriving (Show, Eq, Read)

-- symbolic event representation
type SEArgName = String
data SEArgs = SEArgs {symEvtArgs :: SEArgName}
           deriving (Show, Read,  Eq, Ord)
--type SEArgs = [SEArg]
data SEvent = SEvent { symEName :: AEvent
                     , symEArgs :: SEArgs
                     }
              deriving (Show, Read, Ord)


data AlgRule = ASkip {getASkip :: AEvent}
             | AZero { getAZeroElem :: AEvent 
                     , getAZero     :: AEvent
                     }
             | ACom  AEvent AEvent
             | AInv  AEvent AEvent  
             | AOther
             deriving (Show, Eq, Ord)
                      
symRl2AlgRl :: RewRulPat -> AlgRule
symRl2AlgRl ([SEvent e (SEArgs _)] :~: []) = ASkip e
symRl2AlgRl ([SEvent d (SEArgs _), SEvent e (SEArgs _)] 
             :~: 
             [SEvent _ (SEArgs _)]
            ) = AZero d e
symRl2AlgRl ([SEvent e (SEArgs _), SEvent d (SEArgs _)] 
                 :~: 
             [SEvent _ (SEArgs _), SEvent _ (SEArgs _)]
            ) = ACom e d
symRl2AlgRl ([SEvent e (SEArgs _), SEvent d (SEArgs _)]
             :~: []
            ) = AInv e d 
symRl2AlgRl _ = AOther

instance Eq SEvent where
  (SEvent en1 _) == (SEvent en2 _) = en1 == en2

-- instance Ord SEvent where
--   compare ev1 ev2 | ev1 == ev2 = EQ
--                   | otherwise  = LT

-- type Var = String
data Var = PrimVar String
         | ArrVar [Var]  
         deriving (Show, Eq, Read)

type State = [Var]

data LogEntry = LogEntry { state :: State 
                         , event :: CEvent
                         }
              deriving (Show, Read)
                       
instance Hashable LogEntry where
  hashWithSalt salt (LogEntry _ (CEvent e _)) = hashWithSalt salt e

instance Eq LogEntry where                       
  (==) (LogEntry _ (CEvent e1 _)) (LogEntry _ (CEvent e2 _)) = e1 == e2 
type Log = [LogEntry]

type Pat = [SEvent]
data RewRulPat = Pat :~: Pat
               deriving (Show, Read, Ord)

lengthPattern :: RewRulPat -> Int 
lengthPattern (lhs :~: rhs) = length lhs + length rhs
                        
getSkipRules :: [RewRulPat] -> [RewRulPat]
getSkipRules rrs = [rr | rr <- rrs, isSkip rr] 

getZeroRules :: [RewRulPat] -> [RewRulPat]
getZeroRules rrs = [rr | rr <- rrs, isZero rr]

getComRules :: [RewRulPat] -> [RewRulPat]
getComRules rrs = [rr | rr <- rrs, isCom rr]

mkZrFrSkp :: [AEvent] -> [AEvent] -> [RewRulPat]
mkZrFrSkp evs zevs = [zeroRP e d | e <- evs, d <- zevs]

mkIdpFrSkp :: [AEvent] -> [RewRulPat]
mkIdpFrSkp zevs = [idempRP e | e <- zevs]

mkIgnFrSkp :: [AEvent] -> [RewRulPat]
mkIgnFrSkp zevs = [ignoreRP e | e <- zevs]

mkComFrSkp :: [AEvent] -> [AEvent] -> [RewRulPat]
mkComFrSkp evs zevs = [comRP e d | e <- zevs, d <- evs]

getLhsRewRulPat :: RewRulPat -> Pat
getLhsRewRulPat (lhs :~: _) = lhs

getRhsRewRulPat :: RewRulPat -> Pat
getRhsRewRulPat (_ :~: rhs) = rhs

instance Eq RewRulPat where
  (==) (l1 :~: r1) (l2 :~: r2) = (l1 == l2 && r1 == r2) || 
                                 (l1 == r2 && r1 == l2)

-- The encoding below as GADTs should be reconsidered in the future.
-- I have a fillng that with data famalies we could join all pattern types 
-- in one data type and apply rrpi algorithm to it. Now we have to handle each
-- type of rr-s individually. 
  
data Skip = Skip
          deriving Show
data Zero = Zero
          deriving Show
data Idemp = Idemp
           deriving Show
data Unknown = Unknown
             deriving Show
data Ignore = Ignore
            deriving Show
data Com = Com
          deriving Show
data Inv = Inv
           deriving Show

data RRPattern p t where
  SkipLike       :: Show p => p -> RRPattern p Skip
  ZeroLike       :: Show p => p -> RRPattern p Zero
  IdempLike      :: Show p => p -> RRPattern p Idemp 
  IgnoreLike     :: Show p => p -> RRPattern p Ignore
  ComLike        :: Show p => p -> RRPattern p Com
  InvLike        :: Show p => p -> RRPattern p Inv
  UnknownPattern :: Show p => p -> RRPattern p Unknown
                   
deriving instance Show (RRPattern p t)
  
  
-- Function rrp2Pair and rrp2List might be useful for union of all rrp.
-- They give kind of common base for all rrp.
rrp2Pair :: RRPattern p t -> (p, t)
rrp2Pair (SkipLike p)   = (p, Skip)
rrp2Pair (ZeroLike p)   = (p, Zero)
rrp2Pair (IdempLike p)  = (p, Idemp)
rrp2Pair (IgnoreLike p) = (p, Ignore)
rrp2Pair (ComLike p)    = (p, Com)
rrp2Pair (InvLike p)    = (p, Inv)
rrp2Pair _ = error  "there is no such a pattern"

rrp2List :: [RRPattern p t] -> [(p, t)]
rrp2List rrps = map rrp2Pair rrps 

getPattern :: RRPattern p t -> t
getPattern (SkipLike   _)   = Skip
getPattern (ZeroLike   _)   = Zero
getPattern (IdempLike  _)   = Idemp
getPattern (IgnoreLike _)   = Ignore
getPattern (ComLike    _)   = Com
getPattern (InvLike    _)   = Inv
getPattern (UnknownPattern _) = error "Type of pattern is not deffined"

getPattern_ :: RewRulPat -> String
getPattern_ p = case normRRP p of
                  pN | isSkip  pN -> "Skip"
                     | isZero  pN -> "Zero"
                     | isMZero pN -> "MZero"
                     | isCom   pN -> "Com"
                     | isInv   pN -> "Inv"
                     | isEqv   pN -> "Eqv"
                     | otherwise  -> "Unk"


normRRP :: RewRulPat -> RewRulPat
normRRP pat@(lhs :~: rhs) | length lhs >= length rhs = pat
                          | otherwise                = rhs :~: lhs         



isMZero :: RewRulPat -> Bool
isMZero ([SEvent e (SEArgs q), SEvent _ (SEArgs _)] 
        :~: 
        [SEvent e1 (SEArgs q1)]
       ) = e == e1 && q == q1 
isMZero _ = False           


isEqv :: RewRulPat -> Bool
isEqv ([SEvent e (SEArgs q)] 
        :~: 
        [SEvent e1 (SEArgs q1)]
       ) = e == e1 && q == q1 
isEqv _ = False

getRule :: RRPattern p t -> p
getRule (SkipLike   p)      = p
getRule (ZeroLike   p)      = p
getRule (IdempLike  p)      = p
getRule (IgnoreLike p)      = p
getRule (ComLike    p)      = p
getRule (InvLike    p)      = p
getRule (UnknownPattern _ ) = error "There is no rule defined"

skipRP :: AEvent -> RewRulPat
skipRP e = [SEvent e (SEArgs "p")] :~: []

isSkip :: RewRulPat -> Bool
isSkip ([SEvent _ (SEArgs _)] :~: []) = True 
isSkip _                              = False 

skipLike :: AEvent -> RRPattern RewRulPat Skip
skipLike e = SkipLike $ skipRP e

skip :: [AEvent] -> [RRPattern RewRulPat Skip]
skip es = [ skipLike e| e <- es ]


zeroRP :: AEvent -> AEvent -> RewRulPat
zeroRP e d =  [SEvent d (SEArgs "p"), SEvent e (SEArgs "q")] 
              :~: 
              [SEvent e (SEArgs "q")]

-- isZero rule also returns true for idempotent patterns
isZero :: RewRulPat -> Bool
isZero ([SEvent _ (SEArgs _), SEvent e (SEArgs q)] 
        :~: 
        [SEvent e1 (SEArgs q1)]
       ) = e == e1 && q == q1 
isZero _ = False           

zeroLike :: AEvent -> AEvent -> RRPattern RewRulPat Zero
zeroLike e d = ZeroLike $ zeroRP e d

zero :: [AEvent] -> [RRPattern RewRulPat Zero]
zero es = [zeroLike e d | e <- es, d <- es, e /= d]

idempRP :: AEvent -> RewRulPat
idempRP e = [SEvent e (SEArgs "p"), SEvent e (SEArgs "q")] 
            :~: 
            [SEvent e (SEArgs "q")]
              
idempLike :: AEvent -> RRPattern RewRulPat Idemp
idempLike e = IdempLike $ idempRP e
              
idemp :: [AEvent] -> [RRPattern RewRulPat Idemp]
idemp  es = [idempLike e | e <- es]

ignoreRP :: AEvent -> RewRulPat
ignoreRP e = [SEvent e (SEArgs "p")] 
             :~: 
             [SEvent e (SEArgs "q")]

ignoreLike :: AEvent -> RRPattern RewRulPat Ignore 
ignoreLike e = IgnoreLike $ ignoreRP e

ignore :: [AEvent] -> [RRPattern RewRulPat Ignore]
ignore es = [ignoreLike e| e <- es]

comRP :: AEvent -> AEvent -> RewRulPat
comRP e d = [SEvent e (SEArgs "p"), SEvent d (SEArgs "q")] 
            :~: 
            [SEvent d (SEArgs "q"), SEvent e (SEArgs "p")]

isCom :: RewRulPat -> Bool
isCom ([SEvent e (SEArgs p), SEvent d (SEArgs q)] 
       :~: 
       [SEvent d1 (SEArgs q1), SEvent e1 (SEArgs p1)]
      ) = e == e1 && p == p1 && d == d1 && q == q1  
isCom _ = False

comLike :: AEvent -> AEvent -> RRPattern RewRulPat Com
comLike e d = ComLike $ comRP e d
              

com :: [AEvent] -> [RRPattern RewRulPat Com]
com es = let allComPairs = [(e, d) |e <- es, d <- es, e /= d]
             fillterComPairs = foldr (\x y -> 
                                       if (swap x) `elem` y 
                                       then y 
                                       else (x:y)
                                     ) 
                               [] 
                               allComPairs
         in [comLike e d | (e, d) <- fillterComPairs]

invRP :: AEvent -> AEvent -> RewRulPat
invRP e d = [SEvent e (SEArgs "p"), SEvent d (SEArgs "q")] :~: []

isInv :: RewRulPat -> Bool
isInv ([SEvent _ (SEArgs _), SEvent _ (SEArgs _)] :~: []) = True 
isInv _                                                   = False 

invLike :: AEvent -> AEvent -> RRPattern RewRulPat Inv
invLike e d = InvLike $ invRP e d

inv :: [AEvent] -> [RRPattern RewRulPat Inv]
inv es = [ invLike e d | e <- es , d <- es]


-- it is how I would like to write join rrp of all types
-- allRRPats :: [AEvent] -> RRPattern RewRulPat t
-- allRRPats es = concat $ map (flip ($) es) $ rrp2List [skip, zero]


-- | Possibly change Witness to a datatype
data Witness where
  WitnessC :: Show a =>  a -> RewRulPat -> (Int, Int) -> Witness
  
instance Show Witness where
  show (WitnessC t rr wc) = show rr ++ show wc ++ show t

instance Eq Witness where
   (WitnessC _ rr1 _) ==  (WitnessC _ rr2 _) = rr1 == rr2

type Witnesses = [Witness]

getRewRulPat :: Witness -> RewRulPat
getRewRulPat (WitnessC _ r _) = r

getWitnessCount :: Witness -> (Int, Int)
getWitnessCount (WitnessC _ _ c) = c

getPosWitnessCount :: Witness -> Int
getPosWitnessCount (WitnessC _ _ (p, _)) = p

getWitnessType :: Witness -> String
getWitnessType (WitnessC t _ _) = show t
                                   
type Template = [[RewRulPat]] 

data Result = Result { getMatchWit   :: [RewRulPat]
                     , getFPMatchWit :: [RewRulPat]
                     , getFNMatchWit :: [RewRulPat]
                     }

sumRes :: Result -> (Int, Int, Int)
sumRes (Result mat fp fn) = (length mat, length fp, length fn) 

matchWthTempl :: [RewRulPat] -> [RewRulPat] -> Result 
matchWthTempl twt wt = 
  let pred    = \x y -> x == y 
      match   = intersectBy pred twt wt 
      matchFP = deleteFirstsBy pred wt twt
      matchFN = deleteFirstsBy pred twt wt
  in  (Result match matchFP matchFN)

appendWS :: Witnesses -> Witnesses -> Witnesses
appendWS ws1 ws2 = foldr (\x xs -> cons x xs) ws1 ws2 where
  cons w []      = [w]
  cons w1@(WitnessC t1 rr1 (p1,n1)) (w2@(WitnessC _ rr2 (p2,n2)):ws)
    | rr1 == rr2 = (WitnessC t1 rr1 (p1+p2,n1+n2)):ws
    | otherwise  = w2:(cons w1 ws)

concatWS :: [Witnesses] -> Witnesses
concatWS = mergeWS . concat

mergeWS :: Witnesses -> Witnesses
mergeWS = foldr (\x xs -> cons x xs) []
  where
    cons w []      = [w]
    cons w1@(WitnessC t1 rr1 (p1,n1)) (w2@(WitnessC _ rr2 (p2,n2)):ws)
      | rr1 == rr2 =  (WitnessC t1 rr1 (p1+p2,n1+n2)):ws
      | otherwise  = w2:(cons w1 ws)
 
-- | reverse is applied to make rules non decreasing from left to right
genRulesFromEventsN :: Int -> [AEvent] -> [RewRulPat]
genRulesFromEventsN maxLen events = map mkRewRulPat $ genEventsPairs $ reverse  ([]:trace (show $ length genEventsMaxLength) genEventsMaxLength)
    where
      genEvents = [x ++ [ev] | x <- [] : genEvents, ev <- events]
      genEventsMaxLength = takeWhile (\x -> length x < maxLen) genEvents
      
      genEventsPairs []     = []
      genEventsPairs (a:as) = map ((,)a) as ++ genEventsPairs as

      mkRewRulPat (evNames1, evNames2) = mkPat evNames1 :~: mkPat evNames2
      mkPat = map mkSEvent
      mkSEvent evName = SEvent evName $ SEArgs ""

 
integers = [x ++ [i] | x <- [] : integers, i <- [1..4]]


genRuleCandidates :: Int -> [Log] -> [RewRulPat]
genRuleCandidates k logs = map mkRewRulPat 
                           $ genEventsPairs 
                           $ sortBy ((flip compare) `on` length) 
                           $ []:(nub $ concatMap (genRuleCandidates' k) $ map getEvs logs)
    where
      getEvs :: Log -> [AEvent]  
      getEvs logs_ = map (conEName . event) logs_
                     
      genRuleCandidates' :: Int -> [AEvent] -> [[AEvent]]
      genRuleCandidates' k []  = []
      genRuleCandidates' k evs = let k' = k `min` (length evs) 
                                 in (takeUpto k' evs) ++ genRuleCandidates' k' (tail evs)


      takeUpto :: Int -> [AEvent] -> [[AEvent]]
      takeUpto 0 _    = [] 
      takeUpto k evs = (take k evs):takeUpto (k-1) evs
                       
      genEventsPairs :: [[AEvent]] -> [([AEvent], [AEvent])]
      genEventsPairs []     = []
      genEventsPairs (a:as) = map ((,)a) as ++ genEventsPairs as
                              
      mkRewRulPat ::([AEvent], [AEvent]) -> RewRulPat
      mkRewRulPat (evNames1, evNames2) = mkPat evNames1 :~: mkPat evNames2
      mkPat = map mkSEvent
      mkSEvent evName = SEvent evName $ SEArgs ""
                                                   

combinationsWRep xs n = filter ((n==).length.nub) $ mapM (const xs) [1..n]
