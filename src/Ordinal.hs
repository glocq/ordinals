module Ordinal
  ( Ordinal
  , OrdinalType(..)
  , fromNat, omega, omega2, omegaSquared, omegaOmega
  , normalize
  , omegaPower
  , ordinalType
  , successor
  , (*+*)
  )
where

import Data.Functor ((<&>))
import Data.Strict.Sequence (Seq(..), empty, singleton, sort)
import Data.Strict.Tuple (Pair(..))
import Data.Types.Injective (to)
import Numeric.Natural (Natural)
import qualified Ordinal.Util as Util


-- | Ordinals less than epsilon_epsilon_epsilon_...
--
-- We make use of the Cantor Normal Form (CNF). We will generally use:
-- - The letter b to refer to the ordinals inside a given CNF,
-- - The letter c to refer to the naturals inside a given CNF.
-- 
-- By convention, `CNF []` represents zero.
--
-- - Important note: The following conditions are assumed to be fulfilled:
--   - In the CNF constructor, the sequence of ordinals is assumed to be
--     strictly decreasing.
--   - `Epsilon` values are not wrapped alone in a `CNF` constructor
--     (i.e. the following is NOT valid, whatever the value of `x`:)
--     ```
--     CNF (singleton (Epsilon x :!: Util.nznOne))
--     ```
--   - omega is never explicitly raised to the power of an epsilon number
--     (since omega^eps = eps for any epsilon number eps, we systematically
--     favour the latter)
--   Of course, those conditions need to be fulfilled no matter how deeply
--   nested the culprit is.
--   If you have a value of this type, but you are not sure whether it fulfils
--   the above conditions, you can apply the function `normalize` to it.
--
-- - Reminder to implementers:
--   - `Seq` is like a strict list, except you can pattern match on it both
--     from the left, using `:<|`, and from the right, using `:|>`
--   - `Pair` is like a strict tuple, with the `:!:` being the counterpart of
--     the comma.
data Ordinal = CNF (Seq (Pair Ordinal Util.NonZeroNat)) | Epsilon Ordinal
  deriving Eq -- the `Eq` instance heavily relies on the above conditions being fulfilled

type CNFSeq = Seq (Pair Ordinal Util.NonZeroNat)

cnfMap :: (Ordinal -> Ordinal) -> CNFSeq -> CNFSeq
cnfMap f = fmap $ \(b :!: c) -> (f b :!: c)


-- | Take an ordinal that may not be well-formed, and get it into the right form.
-- You shouldn't need this, unless you touch this module's implementation.
normalize :: Ordinal -> Ordinal
normalize (Epsilon o) = Epsilon $ normalize o
normalize (CNF s) = collapse $ CNF $ merge . sort . cnfMap normalize $ s
  where
  -- | Given an *ordered* sequence of omega-terms where not every term is
  -- necessarily distinct, merge the terms that are the same
  merge :: CNFSeq -> CNFSeq
  merge Empty = Empty
  merge (t :<| Empty) = t :<| Empty
  merge (t1@(b1 :!: c1) :<| cnf'@((b2 :!: c2) :<| cnf''))
    | b1 == b2  = merge $ (b1 :!: Util.nznPlus c1 c2) :<| cnf''
    | otherwise = t1 :<| cnf'
  -- | If the input ordinal is a simple epsilon number wrapped in a `CNF`
  -- constructor, unwrap it
  collapse :: Ordinal -> Ordinal
  collapse o@(CNF ((Epsilon alpha :!: c) :<| Empty)) = if c == Util.nznOne
    then Epsilon alpha
    else o
  collapse o = o

-- | Take omega to the power of this ordinal
omegaPower :: Ordinal -> Ordinal
omegaPower eps@(Epsilon _) = eps -- omega to the power of an epsilon number equals that epsilon number
omegaPower ord = CNF $ singleton $ ord :!: Util.nznOne

-- | Sum two ordinals. This function heavily relies on certain conditions being
-- fulfilled by the representations of `o1` and `o2`; see the documentation of
-- the `Ordinal` datatype.
(*+*) :: Ordinal -> Ordinal -> Ordinal
(CNF Empty) *+* o2 = o2
o1 *+* (CNF Empty) = o1
o1 *+* o2 = CNF $ cnfPlus (cnf o1) (cnf o2)
        -- | Addition for CNF
  where cnfPlus :: CNFSeq -> CNFSeq -> CNFSeq
        cnfPlus Empty s2 = s2
        cnfPlus s1 Empty = s1
        cnfPlus cnf1@((b1 :!: c1) :<| cnf1') cnf2@((b2 :!: c2) :<| cnf2') =
          case compare b1 b2 of
            GT -> (b1 :!:         c1        ) :<| (cnf1' `cnfPlus` cnf2 )
            LT -> (b2 :!:         c2        ) :<| (cnf1  `cnfPlus` cnf2')
            EQ -> (b1 :!: Util.nznPlus c1 c2) :<| (cnf1' `cnfPlus` cnf2')

-- | Gets the Cantor Normal Form of an ordinal. To draw attention of the fact
-- that this does not necessarily output a valid representation of an ordinal
-- (if an epsilon number is given as input, the result will be a wrapped
-- epsilon number), we don't wrap the result in a `CNF` constructor of the
-- `Ordinal` type.
cnf :: Ordinal -> Seq (Pair Ordinal Util.NonZeroNat)
cnf ord = case ord of
  CNF     l -> l
  Epsilon _ -> singleton $ ord :!: Util.nznOne


successor :: Ordinal -> Ordinal
-- | CNF with a unit term
successor (CNF (cnf' :|> (CNF Empty :!: c))) =
  CNF $ cnf' :|> (CNF Empty :!: Util.successor c)
-- | CNF with no unit term
successor (CNF s) = CNF $ s :|> (CNF Empty :!: Util.nznOne)
-- | Epsilon number
successor o = successor $ CNF $ cnf o

-- Some "standard" ordinals

zeroOrd :: Ordinal
zeroOrd = CNF Empty

fromNat :: Natural -> Ordinal
fromNat n = case to n of
  Nothing -> CNF empty
  Just nonZeroN -> CNF $ singleton $ CNF empty :!: nonZeroN

omega :: Ordinal
omega = CNF $ singleton $ fromNat 1 :!: Util.nznOne

omega2 :: Ordinal
omega2 = CNF $ singleton $ fromNat 1 :!: Util.PlusOne 1

omegaSquared :: Ordinal
omegaSquared = CNF $ singleton $ fromNat 2 :!: Util.nznOne

-- | Omega to the power of omega
omegaOmega :: Ordinal
omegaOmega = CNF $ singleton $ omega :!: Util.nznOne


-- | Output type for `ordinalType`: the type of ordinal, and associated data
data OrdinalType = ZeroType                          -- ^ Just zero
                 | Successor Ordinal                 -- ^ Predecessor of the ordinal if it is a successor
                 | Limit (Util.InfiniteList Ordinal) -- ^ Limit sequence of the ordinal if it i a limit ordinal

-- | Gives the predecessor of a given ordinal, if applicable. If it is a limit
-- ordinal, give a sequence that has it as a limit instead.
ordinalType :: Ordinal -> OrdinalType
ordinalType (CNF Empty) = ZeroType
ordinalType (CNF (cnf' :|> (b :!: c))) = case ordinalType b of
  ZeroType -> case Util.predecessor c of
    Nothing -> Successor $ CNF cnf'
    Just cPred -> Successor $ CNF $ cnf' :|> (b :!: cPred)
  Successor bPred -> Limit $ case Util.predecessor c of
    Nothing -> Util.nznList <&> \n ->
      CNF $ cnf' :|> (bPred :!: n)
    Just cPred -> Util.nznList <&> \n ->
      CNF $ cnf' :|> (b :!: cPred) :|> (bPred :!: n)
  Limit limitSequence -> Limit $ case Util.predecessor c of
    Nothing -> limitSequence <&> \bn ->
      CNF $ cnf' :|> (bn :!: Util.nznOne)
    Just cPred -> limitSequence <&> \bn ->
      CNF $ cnf' :|> (b :!: cPred) :|> (bn :!: Util.nznOne)
ordinalType (Epsilon o) = Limit $ case ordinalType o of
  ZeroType -> Util.iterateInfinite omegaPower zeroOrd
  Successor oPred -> Util.iterateInfinite omegaPower $ successor $ Epsilon oPred
  Limit limitSequence -> Epsilon <$> limitSequence


instance Ord Ordinal where
  compare (CNF Empty) (CNF Empty) = EQ
  compare (CNF Empty) (CNF _    ) = GT
  compare (CNF _    ) (CNF Empty) = LT
  compare (CNF ((ord1 :!: digit1) :<| xs1))
          (CNF ((ord2 :!: digit2) :<| xs2)) = case compare ord1 ord2 of
    EQ -> case compare digit1 digit2 of
      EQ   -> compare xs1 xs2
      comp -> comp
    comp -> comp
  compare (Epsilon ord1) (Epsilon ord2) = compare ord1 ord2
  compare ord1 ord2 = compare (cnf ord1) (cnf ord2)
