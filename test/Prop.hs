{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Proxy
import Data.Word (Word8)

import Control.Lens
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck

import Control.Lens.Cons.Extras

main :: IO ()
main = defaultMain $ testGroup "Properties"
  [ testProperty "[a] -> [a]" (prop_id :: String -> Bool)
  , testProperty "T -> T" (prop_id :: T.Text -> Bool)
  , testProperty "TL -> TL" (prop_id :: TL.Text -> Bool)
  , testProperty "B -> B" (prop_id :: B.ByteString -> Bool)
  , testProperty "L -> L" (prop_id :: L.ByteString -> Bool)
  , testProperty "Str -> T -> Str" (prop_roundtrip (Proxy :: Proxy T.Text) :: String -> Bool)
  , testProperty "Str -> TL -> Str" (prop_roundtrip (Proxy :: Proxy TL.Text) :: String -> Bool)
  , testProperty "T -> TL -> T" (prop_roundtrip (Proxy :: Proxy TL.Text) :: T.Text -> Bool)
  , testProperty "[W8] -> L -> [W8]" (prop_roundtrip (Proxy :: Proxy L.ByteString) :: [Word8] -> Bool)
  , testProperty "[W8] -> B -> [W8]" (prop_roundtrip (Proxy :: Proxy B.ByteString) :: [Word8] -> Bool)
  , testProperty "B -> L -> B" (prop_roundtrip (Proxy :: Proxy L.ByteString) :: B.ByteString -> Bool)
  , testProperty "[a] -> List a -> [a]" (prop_roundtrip (Proxy :: Proxy (List Int)) :: [Int] -> Bool)
  ]

prop_id :: (Cons s s a a, Monoid s, Eq s) => s -> Bool
prop_id s = view recons s == s

prop_roundtrip
  :: forall s1 s2 a. (Cons s1 s1 a a, Cons s2 s2 a a, Monoid s1, Monoid s2, Eq s1)
  => Proxy s2 -> s1 -> Bool
prop_roundtrip _ s1 = view recons (view recons s1 :: s2) == s1


-- Let's implement a type *without* rewrite rules to ensure that the
-- default implementation is correct
--
data List a = Nil | Cons a (List a)
  deriving (Eq)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil ys = ys
  Cons x xs `mappend` ys = Cons x (xs `mappend` ys)

instance Cons (List a) (List b) a b where
  _Cons = prism (uncurry Cons) f
    where
      f Nil = Left Nil
      f (Cons x xs) = Right (x, xs)
