{-# LANGUAGE RankNTypes, DataKinds, ScopedTypeVariables, TypeOperators, KindSignatures, ViewPatterns, FlexibleInstances, TypeFamilies, UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction, AllowAmbiguousTypes, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module GHC.TypeLits.Induction.PeanoInduction where
import GHC.TypeLits
import Data.Proxy
newtype PeanoBase p (k::Nat) = PeanoBase {baseT :: p k}
newtype PeanoStep p = PeanoStep {stepT :: forall m. KnownNat m => p m->p (1+m)}
class (KnownNat n)=>PeanoInduction (x::Bool) (n::Nat) where
    inducePeano' :: Proxy x->Proxy n->PeanoStep p-> PeanoBase p 0 -> PeanoBase p n

type family F (n::Nat) :: Bool where
    F 0 = False
    F x = True


instance PeanoInduction 'False 0 where
    inducePeano' _ _ (stepT->_step) (baseT->base) = PeanoBase base
instance (KnownNat m, m~(1+n), KnownNat n, PeanoInduction (F n) n)=>PeanoInduction 'True m where
    inducePeano' (px :: Proxy True) (pn :: Proxy (1+n)) (s::PeanoStep p) b =
        let (baseT->b')=((inducePeano' (Proxy :: Proxy (F n)) (Proxy :: Proxy n) s b)) in PeanoBase $ (stepT s) $ b'


inducePeano1 :: (KnownNat n, PeanoInduction (F n) n)=>Proxy n->PeanoStep p-> PeanoBase p 0 -> PeanoBase p n
inducePeano1 (pn :: Proxy n) = inducePeano' (Proxy :: Proxy (F n)) pn

inducePeano0 :: (KnownNat n, PeanoInduction (F n) n)=>PeanoStep p->PeanoBase p 0->PeanoBase p n
inducePeano0 = inducePeano1 Proxy

inducePeanoWithPF :: (KnownNat n, PeanoInduction (F n) n)=>(forall m. KnownNat m=>p m->p (1+m))->p 0->p n
inducePeanoWithPF f p0 = baseT $ inducePeano0 (PeanoStep f) (PeanoBase p0)
