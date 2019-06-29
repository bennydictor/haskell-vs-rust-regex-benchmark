{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE BangPatterns          #-}
module Lib
    ( eps, sym, alt, seq, rep
    , match
    ) where

import Prelude hiding ((+), (*), seq)
import Data.List
import Data.Semiring


data Reg c s r = Reg
    { empty :: !s
    , final :: !s
    , re :: !(r c s) }

data    Eps     c s = Eps
newtype Sym     c s = Sym (c -> s)
data    Alt p q c s = Alt !(Reg c s p) !(Reg c s q)
data    Seq p q c s = Seq !(Reg c s p) !(Reg c s q)
data    Rep r   c s = Rep !(Reg c s r)

eps :: Semiring s => Reg c s Eps
eps = Reg { empty = one, final = zero, re = Eps }
{-# INLINE eps #-}

sym :: Semiring s => (c -> s) -> Reg c s Sym
sym p = Reg { empty = zero, final = zero, re = Sym p }
{-# INLINE sym #-}

alt :: Semiring s => Reg c s p -> Reg c s q -> Reg c s (Alt p q)
alt p q = Reg { empty = empty p + empty q, final = final p + final q, re = Alt p q }
{-# INLINE alt #-}

seq :: Semiring s => Reg c s p -> Reg c s q -> Reg c s (Seq p q)
seq p q = Reg { empty = empty p * empty q, final = final q + empty q * final p, re = Seq p q }
{-# INLINE seq #-}

rep :: Semiring s => Reg c s r -> Reg c s (Rep r)
rep r = Reg { empty = one, final = final r, re = Rep r }
{-# INLINE rep #-}


class Semiring s => Re c s r where
    shift :: s -> c -> r c s -> Reg c s r

instance Semiring s => Re c s Eps where
    shift _ _ _ = eps

instance Semiring s => Re c s Sym where
    shift s c (Sym p) = (sym p) { final = p c * s }

instance (Semiring s, Re c s p, Re c s q) => Re c s (Alt p q) where
    shift s c (Alt p q) = alt (shift s c $ re p) (shift s c $ re q)

instance (Semiring s, Re c s p, Re c s q) => Re c s (Seq p q) where
    shift s c (Seq p q) = seq (shift s c $ re p) (shift (final p + empty p * s) c $ re q)

instance (Semiring s, Re c s r) => Re c s (Rep r) where
    shift s c (Rep r) = rep (shift (final r + s) c $ re r)

match :: (Semiring s, Re c s r) => Reg c s r -> [c] -> s
match r [] = empty r
match r (c:cs) = final $ foldl' (\r c -> shift zero c $ re r) (shift one c $ re r) cs
