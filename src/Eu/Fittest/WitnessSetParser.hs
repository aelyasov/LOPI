{-# LANGUAGE FlexibleContexts, TupleSections #-}

module Eu.Fittest.WitnessSetParser (getWitnesses, getRewPattern) where

import Eu.Fittest.Data

import Data.Char

import Text.ParserCombinators.UU.Core hiding (Zero)
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Derived
import Text.ParserCombinators.UU.Utils 

pWitnesses :: Parser Witnesses
pWitnesses = pMany (pWitness <* pLF) <* pEnd

pWitness :: Parser Witness
pWitness = (\l r [p,n] t -> WitnessC t (l :~: r) (p,n)) <$> 
               pBrackets pPat
           <* pSymbol "~"
           <*> pBrackets pPat
           <*> tupleParser pIntegerRaw
           -- <*> (pSkip <|> pZero <|> pIdemp <|> pIgnore <|> pCom <|> pInv) <* pSym '\n'
           <*> (pMany $ pSatisfy (\x -> isAscii x && x /= '\n') (Insertion "insert" undefined  5))
  where
    -- str2pat :: String -> a
    -- str2pat str = case str of
    --   "Skip"   -> Skip
    --   "Zero"   -> Zero
    --   "Idemp"  -> Idemp
    --   "Ignore" -> Ignore
    --   "Com"    -> Com
    --   "Inv"    -> Inv
    --   _        -> error "cann't find mattching pattern type"

pRewRulPat :: Parser RewRulPat
pRewRulPat = (:~:) <$> pBrackets pPat <* pSymbol "~" <*> pBrackets pPat

pPat :: Parser Pat
pPat = pListSep (pSymbol ";") pSEvent

pSEvent :: Parser SEvent
pSEvent = (\en earg -> SEvent en (SEArgs earg)) <$> pId <*> (pParens pId)

getWitnesses :: String -> Witnesses
getWitnesses = runParser "error" pWitnesses

getRewPattern :: String -> RewRulPat
getRewPattern = runParser "error" pRewRulPat

pId :: Parser String
pId = pMany (pLetter <|> pDigit <|> pSym '_')

-- pSkip :: Parser Skip
-- pSkip = const Skip <$> pToken "Skip"

-- pZero :: Parser Zero
-- pZero = const Zero <$> pToken "Zero"

-- pIdemp :: Parser Idemp
-- pIdemp = const Idemp <$> pToken "Idemp"

-- pIgnore :: Parser Ignore
-- pIgnore = const Ignore <$> pToken "Ignore"

-- pCom :: Parser Com
-- pCom = const Com <$> pToken "Com"

-- pInv :: Parser Inv
-- pInv = const Inv <$> pToken "Inv"







