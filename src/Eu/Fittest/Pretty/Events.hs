module Eu.Fittest.Pretty.Events ( ppLog
                                , ppRewRulPat
                                , ppRewRulPats
                                , ppWSet
                                , ppCEvents
                                ) where

import Eu.Fittest.Data
import Text.PrettyPrint.HughesPJ

ppLog :: Log -> Doc
ppLog log = vcat $ map ppLogEntry log

ppLogEntry :: LogEntry -> Doc
ppLogEntry (LogEntry st ev) = ppCEvent ev <+> text "->" <+> ppState st

ppState :: State -> Doc
ppState = parens . hcat . punctuate semi . map ppVar 
-- ppState (State st) = parens $ ppArgs $ map ppVar st

-- ppArgs :: [Doc] -> Doc
-- ppArgs []     = empty
-- ppArgs (a:as) = a <> foldr (\arg -> ((comma <> arg) <>)) empty as

ppVar :: Var -> Doc
ppVar (PrimVar v)  = text v
ppVar (ArrVar vrs) = brackets $ hcat $ punctuate semi $ map ppVar vrs

ppCEvents :: [CEvent] -> Doc
ppCEvents evts = vcat $ map ppCEvent evts

ppCEvent :: CEvent -> Doc
ppCEvent (CEvent en args) = text en <> (parens $ hcat $ punctuate semi $ map ppCEArg args)

ppCEArg :: CEArg -> Doc
ppCEArg (CEPrimArg arg) = text arg
ppCEArg (CEArrArg args) = brackets $ hcat $ punctuate semi $ map ppCEArg args

ppWSet :: [Witness] -> Doc
ppWSet w = (vcat $ map ppETuple w) <> char '\n'

ppETuple :: Witness -> Doc
ppETuple (WitnessC  t pat (pos, neg)) = ppRewRulPat pat 
                                        <+> lparen 
                                        <> int pos 
                                        <> comma 
                                        <> int neg 
                                        <> rparen 
                                        <+> (text $ show t)
                                
ppRewRulPats :: [RewRulPat] -> Doc
ppRewRulPats ps = vcat $ map ppRewRulPat ps

ppRewRulPat :: RewRulPat -> Doc
ppRewRulPat (lp :~: rp) = ppPat lp <+> char '~' <+> ppPat rp

ppPat :: Pat -> Doc
ppPat ps = brackets $ hcat $ punctuate semi $ map ppSEvent  ps

ppSEvent :: SEvent -> Doc
ppSEvent (SEvent en eargs) = text en <> lparen <> ppSEArgs eargs <> rparen

ppSEArgs :: SEArgs -> Doc
ppSEArgs (SEArgs args) = text args

