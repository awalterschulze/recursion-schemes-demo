{-# LANGUAGE DeriveFunctor #-}

import Prelude hiding (concat, or)

newtype Fix f = Fix (f (Fix f))

wreckit :: Fix f -> f (Fix f)
wreckit (Fix x) = x

type Regex = Fix RegexF

data RegexF r = EmptySet
  | EmptyString
  | Character Char
  | Concat r r
  | ZeroOrMore r
  | Or r r
  deriving Functor

emptySet :: Regex
emptySet = Fix EmptySet

emptyString :: Regex
emptyString = Fix EmptyString

character :: Char -> Regex
character c = Fix (Character c)

concat :: Regex -> Regex -> Regex
concat a b = Fix (Concat a b)

zeroOrMore :: Regex -> Regex
zeroOrMore a = Fix (ZeroOrMore a)

or :: Regex -> Regex -> Regex
or a b = Fix (Or a b)

type FAlgebra f r = f r -> r

cata :: Functor f => FAlgebra f r -> Fix f -> r
cata alg = alg . fmap (cata alg) . wreckit

infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

type NullableFAlgebra = FAlgebra RegexF Bool

nullable' :: NullableFAlgebra
nullable' EmptySet = False
nullable' EmptyString = True
nullable' Character{} = False
nullable' (Concat a b) = a && b
nullable' ZeroOrMore{} = True
nullable' (Or a b) = a || b

nullable :: Regex -> Bool
nullable = cata nullable'

type RAlgebra f r = f (Fix f, r) -> r

para :: (Functor f) => RAlgebra f r -> Fix f -> r
para alg f =
    wreckit f
    |> fmap (\x -> (x, para alg x))
    |> alg

type DeriveRAlgebra = RAlgebra RegexF Regex

deriv' :: Char -> DeriveRAlgebra
deriv' _ EmptyString = emptySet
deriv' _ EmptySet = emptySet
deriv' c (Character a) = if a == c 
    then emptyString else emptySet
deriv' c (Concat (r, dr) (s, ds)) =
  if nullable r
  then (dr `concat` s) `or` ds
  else dr `concat` s
deriv' _ (ZeroOrMore (r, dr)) =
  dr `concat` zeroOrMore r
deriv' _ (Or (_, dr) (_, ds)) =
  dr `or` ds

deriv :: Regex -> Char -> Regex
deriv expr c = para (deriv' c) expr

match :: Regex -> String -> Bool
match expr string = nullable (foldl deriv expr string)

main = putStrLn $ show $ match ((character 'a') `concat` (zeroOrMore (character 'b'))) "ab"