{-# LANGUAGE RankNTypes, TypeOperators, NoStarIsType, DataKinds, KindSignatures #-}
module GHC.TypeLits.Induction where
    import GHC.TypeLits
    
    inducePeano :: KnownNat m => (forall n. KnownNat n => p n -> p (1 + n)) -> p 0 -> p m
    inducePeano _ _ = undefined

