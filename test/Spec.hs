{-# LANGUAGE GADTs, TypeOperators, DataKinds, KindSignatures, RankNTypes, ScopedTypeVariables, NoMonomorphismRestriction #-}
import GHC.TypeLits.Induction
import GHC.TypeLits

data Vec (n::Nat) a where
    Nil :: Vec 0 a
    Cons :: a->Vec n a->Vec (1+n) a 

toList :: Vec n a->[a]
toList Nil = []
toList (Cons x xs) = x :(toList xs)
instance (Show a)=>Show (Vec n a) where
    show = show . toList
newtype MapV a b n = MapV {mapV' :: Vec n a->Vec n b}

g' :: (a->b)->(forall n. KnownNat n=>MapV a b n->MapV a b (1+n))
g' f =  (\mapper->MapV $ (\va-> let (Cons x xs) = va in Cons (f x) $ (mapV' mapper) xs)) 
mapV5 :: (a->b)->Vec 5 a->Vec 5 b
mapV5 (f::a->b) = mapV' $ let (g::_)=g' in (((g f)) $ ((g f)) $ ((g f)) $ ((g f)) $ ((g f)) $  ((MapV $ const Nil) :: MapV a b 0))

main :: IO ()
main = do
    let v = Cons 1 $ Cons 1 $ Cons 4 $ Cons 5 $ Cons 1 Nil
    let v' = mapV5 (+10) v
    print v'
    putStrLn "Test suite not yet implemented"
