{-# LANGUAGE RankNTypes, TypeOperators, NoStarIsType, DataKinds, ViewPatterns #-}
module GHC.TypeLits.Induction.Solver {-(plugin)-} where
import GhcPlugins
import GHC.TypeLits
import Class      (Class, classMethods, className, classTyCon)
import FamInst    (tcInstNewTyCon_maybe)
import FastString (fsLit)
import Id         (idType)
import InstEnv    (instanceDFunId,lookupUniqueInstEnv)
import Module     (mkModuleName)
import OccName    (mkTcOcc)
import Outputable (Outputable (..), (<+>), integer, text, vcat)
import Panic      (panic, pgmErrorDoc)
import Plugins    (Plugin (..), defaultPlugin)
import PrelNames  (knownNatClassName)
import TcEvidence (EvTerm (..), EvExpr, evDFunApp, mkEvCast, mkTcSymCo, mkTcTransCo)
import TcPluginM  (TcPluginM, tcLookupClass, getInstEnvs, zonkCt)
import TcRnTypes  (Ct, CtEvidence (..), TcPlugin(..), TcPluginResult (..),
                   ctEvidence, ctEvPred, isWanted, ctEvExpr)
import TcTypeNats (typeNatAddTyCon, typeNatMulTyCon, typeNatExpTyCon)
import Type       (PredTree (ClassPred), TyVar, classifyPredType, dropForAlls,
                   funResultTy, tyConAppTyCon_maybe, mkNumLitTy, mkTyVarTy,
                   mkTyConApp)
import TyCoRep    (Type (..), TyLit (..))
import Var        (DFunId)

import GHC.TcPluginM.Extra          (lookupModule, lookupName, newWanted, tracePlugin)
import GHC.TcPluginM.Extra          (flattenGivens, mkSubst', substType)

--plugin :: Plugin
--plugin = defaultPlugin {
--    tcPlugin = const $ Just peanoPlugin
--}

data PluginCtx = PluginCtx {
    peanoInductionClass :: Class
}
install :: [CommandLineOption]->[CoreToDo]->CoreM [CoreToDo]
install _ todo = do
    putMsgS "GHC.TypeLits.Induction.Solver"
    return todo

--peanoPlugin :: TcPlugin
--peanoPlugin = tracePlugin "ghc-typelits-induction" 
--    TcPlugin {
--        tcPluginInit = lookupPeanoInductions,
--        tcPluginSolve = solvePeanoInductions,
--        tcPluginStop = const (return ())
--    }

--lookupPeanoInduction :: TcPluginM PluginCtx
--lookupPeanoInduction = do
--    let peanoInductionModule = mkModuleName "GHC.TypeLits.Induction.PeanoInduction"
--    let peanoPackage = fsLit "ghc-typelits-induction"
--    mdName <- lookupModule peanoInductionModule peanoPackage
--    peano<-tcLookupClass =<< lookupName mdName (mkTcOcc "PeanoInduction")
--    return $ PluginCtx peano

--type KnConstraint = (Ct, Class, Type, Orig Type)
--type PiConstraint = (Ct, Class, Type, Orig Type)
--solvePeanoInductions :: PluginCtx->[Ct]->[Ct]->[Ct]->TcPluginM TcPluginResult
--solvePeanoInductions defs givens _deriveds wanteds = do
--    let wanteds' = filter (isWanted . ctEvidence) wanteds
--    let subst = map fst $ mkSubst' givens
--    let pi_wanteds = map (\(x,y,z,orig)->(x,y,substType subst z, orig)) $ mapMaybe (toPiConstraint defs)
--    --let given_map = map toGivenEntry (flattenGivens givens)
--    return undefined
--toPiConstraint :: PluginCtx->Ct->Maybe PiConstraint
--toPiConstraint ctx ct = 
--    case classifyPredType $ ctEvPred $ ctEvidence ct of
--        ClassPred cls [ty] 
--            | className cls == className (peanoInductionClass ctx)
--            -> Just (ct, cls, ty, Orig ty)
--        _ -> Nothing

--toGivenEntry :: Ct->(CType, EvExpr)
--toGivenEntry (ctEvidence->ev) = (CType $ ctEvPred ct, ctEvExpr ct)