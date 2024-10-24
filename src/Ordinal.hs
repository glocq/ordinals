{-# LANGUAGE OverloadedLists #-}

module Ordinal
  ( Ordinal
  , OrdinalType
  , (.+.)
  , zeroOrd, fromNat, omega, omega2, omegaSquared, omegaOmega
  , isValid
  , normalize
  , omegaPower
  , ordinalType
  , successor
  )
where

-- We will use `fst` and `snd` on strict pairs, so we hide the default ones:
import Prelude hiding (fst, snd)

import Data.Functor ((<&>))
import Data.Strict.Sequence (Seq(..), sort)
import Data.Strict.Tuple (Pair(..), fst, snd)
import Numeric.Natural (Natural)
import qualified Ordinal.Util as Util


-- | Ordinals less than epsilon_epsilon_epsilon_...
-- We represent ordinals using the Cantor normal form (CNF) whenever possible,
-- i.e. when the number is not an epsilon number.
-- For a given ordinal, there are many ways to express it as a member of this
-- type. Thus we consider only a subset of values to be valid representations
-- of ordinals; this subset is in one-to-one correspondence with the set of
-- ordinals which we are considering here. The validity of a representation is
-- determined by the (total) `isValid` function.
-- The implementation of this type will not be exposed, and (assuming no error
-- on my part) the functions in this module only produce valid ordinal
-- representations, so it shouldn't be possible for a user to accidentally
-- produce an invalid representation.
--
-- A note to implementers:
--   - We will generally use the letters `b` and `c` to refer respectively to
--     ordinals and natural numbers appearing in a CNF
--   - `Seq` is like a strict list, except you can pattern match on it both
--     from the left, using `:<|`, and from the right, using `:|>`
--   - `Pair` is like a strict tuple, with the `:!:` being the counterpart of
--     the comma.
data Ordinal = CNFOrd !CNF | Epsilon !Ordinal
  -- | The `Eq` instance makes the distinction between two representations of
  -- the same ordinal, but will be correct assuming we are comparing to valid
  -- representations:
  deriving Eq

type CNF = Seq (Pair Ordinal Natural)

-- | Like `Eq`, the `Ord` instance will only be correct assuming the compared
-- values are valid represenations of an ordinal.
instance Ord Ordinal where
  compare (CNFOrd []) (CNFOrd []) = EQ
  compare (CNFOrd []) (CNFOrd _ ) = GT
  compare (CNFOrd ((b1 :!: c1) :<| terms1)) (CNFOrd ((b2 :!: c2) :<| terms2)) =
    case compare b1 b2 of
      EQ -> case compare c1 c2 of
        EQ   -> compare terms1 terms2
        comp -> comp
      comp -> comp
  -- `CNFOrd [e :!: 1]` is not a valid ordinal, but we make an exception just
  -- here so we can reduce the problem to a case we've treated before:
  compare cnf@(CNFOrd _) e@(Epsilon _) = compare cnf (CNFOrd [e :!: 1])
  compare (Epsilon ord1) (Epsilon ord2) = compare ord1 ord2
  -- All remaining cases are flipped versions of already treated cases:
  compare ord1 ord2 = Util.flipOrder $ compare ord2 ord1

isValid :: Ordinal -> Bool
isValid cnf@(CNFOrd terms) = not (isEpsilonInDisguise cnf)         &&
                          isStrictlyDecreasing (fmap fst terms) &&
                          all (/= 0) (fmap snd terms)
isValid (Epsilon e) = isValid e
 
isEpsilon :: Ordinal -> Bool
isEpsilon (Epsilon _) = True
isEpsilon _ = False

isEpsilonInDisguise :: Ordinal -> Bool
isEpsilonInDisguise (CNFOrd [b :!: 1]) = isEpsilon b || isEpsilonInDisguise b
isEpsilonInDisguise _ = False

isStrictlyDecreasing :: Ord a => Seq a -> Bool
isStrictlyDecreasing (x :<| y :<| xs) = x > y && isStrictlyDecreasing (y :<| xs)
isStrictlyDecreasing _ = True



-- -- | Take an ordinal that may not be well-formed, and get it into the right form.
-- -- You shouldn't need this, unless you touch this module's implementation.
normalize :: Ordinal -> Ordinal
normalize (Epsilon o) = Epsilon $ normalize o
normalize (CNFOrd s) =
  shallowCollapse $ CNFOrd $ shallowMerge . sort . Util.mapFst normalize $ s
  where
  -- | Given an *ordered* sequence of omega-terms where not every term is
  -- necessarily distinct, merge the terms that are the same
  shallowMerge :: CNF -> CNF
  shallowMerge Empty = []
  shallowMerge (t :<| Empty) = [t]
  shallowMerge (t1@(b1 :!: c1) :<| cnf'@((b2 :!: c2) :<| cnf''))
    | b1 == b2  = shallowMerge $ (b1 :!: c1 + c2) :<| cnf''
    | otherwise = t1 :<| cnf'

-- | If the input ordinal is a simple epsilon number wrapped in a `CNF`
-- constructor, unwrap it
shallowCollapse :: Ordinal -> Ordinal
shallowCollapse (CNFOrd ((Epsilon o :!: 1) :<| Empty)) = Epsilon o
shallowCollapse o = o

-- | Take omega to the power of this ordinal
omegaPower :: Ordinal -> Ordinal
omegaPower eps@(Epsilon _) = eps -- omega to the power of an epsilon number equals that epsilon number
omegaPower ord = CNFOrd [ord :!: 1]


successor :: Ordinal -> Ordinal
-- | CNF with a unit term
successor (CNFOrd (cnf' :|> (CNFOrd [] :!: c))) =
  CNFOrd $ cnf' :|> (CNFOrd [] :!: c + 1)
-- | CNF with no unit term
successor (CNFOrd s) = CNFOrd $ s :|> (CNFOrd [] :!: 1)
-- | Epsilon number
successor o = CNFOrd [o :!: 1, CNFOrd [] :!: 1]

-- Some "standard" ordinals

zeroOrd :: Ordinal
zeroOrd = CNFOrd []

fromNat :: Natural -> Ordinal
fromNat n = case n of
  0 -> zeroOrd
  _ -> CNFOrd [zeroOrd :!: n]

omega :: Ordinal
omega = CNFOrd [fromNat 1 :!: 1]

-- | Omega times 2
omega2 :: Ordinal
omega2 = CNFOrd [fromNat 1 :!: 2]

omegaSquared :: Ordinal
omegaSquared = CNFOrd [fromNat 2 :!: 1]

-- | Omega to the power of omega
omegaOmega :: Ordinal
omegaOmega = CNFOrd [omega :!: 1]


-- | The type of answers we get when asking for the predecessor of an ordinal:
-- either the ordinal is `Zero` (so no predecessor), or it is a `Successor`
-- ordinal (so one predecessor), or it is the `Limit` of an infinite list of
-- ordinals (its *limit sequence*)
data OrdinalType = ZeroType
                 -- ^ Just zero
                 | Successor Ordinal
                 -- ^ Predecessor of the ordinal if it is a successor
                 | Limit (Util.InfiniteList Ordinal)
                 -- ^ Limit sequence of the ordinal if it is a limit ordinal

-- | Gives the predecessor of a given ordinal, if applicable. If it is a limit
-- ordinal, give a sequence that has it as a limit instead.
ordinalType :: Ordinal -> OrdinalType
ordinalType (CNFOrd Empty) = ZeroType
ordinalType (CNFOrd (cnf' :|> (b :!: c))) = case ordinalType b of
  ZeroType -> case c of
    0 -> Successor $ CNFOrd cnf'
    _ -> Successor $ CNFOrd $ cnf' :|> (b :!: c - 1)
  Successor bPred -> Limit $ case c of
    0 -> Util.nats <&> \n ->
      CNFOrd $ cnf' :|> (bPred :!: n)
    _ -> Util.nats <&> \n ->
      CNFOrd $ cnf' :|> (b :!: c - 1) :|> (bPred :!: n)
  Limit limitSequence -> Limit $ case c of
    0 -> limitSequence <&> \bn ->
      CNFOrd $ cnf' :|> (bn :!: 1)
    _ -> limitSequence <&> \bn ->
      CNFOrd $ cnf' :|> (b :!: c - 1) :|> (bn :!: 1)
ordinalType (Epsilon o) = Limit $ case ordinalType o of
  ZeroType -> Util.iterateInfinite omegaPower zeroOrd
  Successor oPred -> Util.iterateInfinite omegaPower $ successor $ Epsilon oPred
  Limit limitSequence -> Epsilon <$> limitSequence

-- | Ordinal sum
(.+.) :: Ordinal -> Ordinal -> Ordinal
CNFOrd Empty .+. o = o
o .+. CNFOrd Empty = o
CNFOrd ((b1 :!: c1) :<| terms1) .+. o2@(CNFOrd ((b2 :!: _) :<| _)) =
  case compare b1 b2 of
    LT -> o2
    _  -> case (b1, c1, CNFOrd terms1 .+. o2) of
      (Epsilon _, 1, CNFOrd Empty) -> b1
      (_, _, CNFOrd terms3) -> CNFOrd $ (b1 :!: c1) :<| terms3
      (_, _, e@(Epsilon _)) -> case compare b1 e of
        LT -> e
        EQ -> CNFOrd [b1 :!: c1 + 1]
        GT -> CNFOrd [b1 :!: c1, e :!: 1]
-- `CNFOrd [o :!: 1]` is not a valid ordinal, but we make an exception in the
-- following two lines so we can reduce the problem to a case we've treated
-- before:
o1@(CNFOrd _) .+. o2@(Epsilon _) = o1 .+. CNFOrd [o2 :!: 1]
o1@(Epsilon _) .+. o2@(CNFOrd _) = CNFOrd [o1 :!: 1] .+. o2
Epsilon o1 .+. Epsilon o2 = case compare o1 o2 of
  LT -> o2
  EQ -> CNFOrd [o1 :!: 2]
  GT -> CNFOrd [o1 :!: 1, o2 :!: 1]
