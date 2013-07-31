{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Foldable (Foldable(foldr))
import Prelude (Functor(fmap), Show(show), Int, succ, (.), ($))

data B = T | F deriving (Show)

infixr 8 \|/, /|\
infixl 7 *
infixl 6 +

-- NOR. Everything else is implemented in terms of this.
(\|/) :: B -> B -> B
F \|/ F = T
_ \|/ _ = F

not a = a \|/ a
a + b = not (a \|/ b)
a*b = not a \|/ not b
a /|\ b = not a*b
a `xor` b = (not a)*b + a*(not b)
mux s a b = a*(not s) + b*s
dmux s x = (x*(not s), x*s)


data Z
data Succ n

type family Sum n m
type instance Sum Z nat = nat
type instance Sum (Succ a) b = Succ (Sum a b)

type N1 = Succ Z
type N2 = Sum N1 N1
type N4 = Sum N2 N2
type N8 = Sum N4 N4
type N16 = Sum N8 N8
type N32 = Sum N16 N16
type N64 = Sum N32 N32

data Vec n e where
    VNil    :: Vec Z e
    VCons   :: e -> Vec n e -> Vec (Succ n) e

vconc :: Vec n a -> Vec m a -> Vec (Sum n m) a
vconc VNil VNil = VNil
vconc (VCons x xs) ys = VCons x (vconc xs ys)

instance Functor (Vec n) where
    fmap _ VNil = VNil
    fmap f (VCons x xs) = VCons (f x) (fmap f xs)

vzipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
vzipWith f VNil         VNil         = VNil
vzipWith f (VCons x xs) (VCons y ys) = VCons (f x y) (vzipWith f xs ys)

vzip = vzipWith (,)

type Bus n = Vec n B

multiNot :: Bus n -> Bus n
multiNot = fmap not

multiAnd :: Bus n -> Bus n -> Bus n
multiAnd = vzipWith (*)

multiOr = vzipWith (+)
