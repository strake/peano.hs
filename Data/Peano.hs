module Data.Peano (Peano (..), infinity) where

import Data.Data
import Data.Function (on)
import Data.Ix (Ix (..))
import Data.Typeable

data Peano = Zero | Succ Peano deriving (Eq, Ord, Read, Show, Typeable, Data)

instance Enum Peano where
    succ = Succ

    pred (Succ n) = n
    pred Zero = error "pred Zero"

    toEnum   = fromInteger . toEnum

    fromEnum = fromEnum . toInteger

instance Bounded Peano where
    minBound = Zero
    maxBound = infinity

instance Ix Peano where
    range = uncurry enumFromTo
    index (l, u) n = fromEnum (n - l)
    inRange (l, u) n = l <= n && u >= n
    rangeSize (l, u) = fromEnum (Succ u - l)

instance Num Peano where
    Zero   + n = n
    Succ m + n = Succ (m + n)

    m      - Zero   = m
    Succ m - Succ n = m - n

    Zero   * n = Zero
    Succ m * n = n + m * n

    abs = id

    signum Zero = Zero
    signum _    = Succ Zero

    fromInteger n | n <  0 = error "fromInteger n | n < 0"
                  | n == 0 = Zero
                  | n >  0 = Succ (fromInteger (n - 1))

instance Real Peano where
    toRational = toRational . toInteger

instance Integral Peano where
    toInteger Zero = 0
    toInteger (Succ n) = toInteger n + 1

    Zero `quotRem` Zero   = error "0/0"
    m    `quotRem` n      = case compare m n
                            of LT -> (Zero, m)
                               _  -> let (q, r) = quotRem (m - n) n in (Succ q, r)

    divMod = quotRem

infinity :: Peano
infinity = Succ infinity
