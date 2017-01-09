-- This file is part of hs-concise
-- Copyright (C) 2016  Fraser Tweedale
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.
--
--     * Neither the name of Author name here nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE NoImplicitPrelude #-}

module Control.Lens.Cons.Extras
  (
    recons
  , unfoldr
  ) where

import Data.Function ((.), id)
import Data.Monoid (Monoid(..))
import Data.Maybe (Maybe, maybe)
import Data.String (String)
import Data.Word (Word8)

import Control.Lens.Cons (Cons, cons, uncons)
import Control.Lens.Iso (Iso', iso, lazy, strict)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lens (packedBytes, unpackedBytes)
import qualified Data.Text as T
import Data.Text.Lens (packed, unpacked)
import qualified Data.Text.Lazy as TL

{-# NOINLINE [2] recons #-}
recons :: (Cons s1 s1 a a, Cons s2 s2 a a, Monoid s1, Monoid s2) => Iso' s1 s2
recons = iso (unfoldr uncons) (unfoldr uncons)

unfoldr :: (Cons s2 s2 a a, Monoid s2) => (s1 -> Maybe (a, s1)) -> s1 -> s2
unfoldr f = maybe mempty (\(a, s') -> cons a (unfoldr f s')) . f

{-# RULES
"recons/id"
  recons = id
"recons/string-text"
  recons = packed :: Iso' String T.Text
"recons/text-string"
  recons = unpacked :: Iso' T.Text String
"recons/string-lazytext"
  recons = packed :: Iso' String TL.Text
"recons/lazytext-string"
  recons = unpacked :: Iso' TL.Text String
"recons/text-strict"
  recons = strict :: Iso' TL.Text T.Text
"recons/text-lazy"
  recons = lazy :: Iso' T.Text TL.Text
"recons/list-bs"
  recons = packedBytes :: Iso' [Word8] B.ByteString
"recons/bs-list"
  recons = unpackedBytes :: Iso' B.ByteString [Word8]
"recons/list-lazybs"
  recons = packedBytes :: Iso' [Word8] L.ByteString
"recons/lazybs-list"
  recons = unpackedBytes :: Iso' L.ByteString [Word8]
"recons/bs-strict"
  recons = strict :: Iso' L.ByteString B.ByteString
"recons/bs-lazy"
  recons = lazy :: Iso' B.ByteString L.ByteString ;
 #-}
