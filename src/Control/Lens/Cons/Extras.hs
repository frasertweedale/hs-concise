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

module Control.Lens.Cons.Extras
  (
    recons
  , unfoldr
  ) where

import Data.Word (Word8)
import Control.Lens
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lens (packedBytes)
import qualified Data.Text as T
import Data.Text.Lens (packed)
import qualified Data.Text.Lazy as TL

{-# NOINLINE [2] recons #-}
recons :: (Cons s1 s1 a a, Cons s2 s2 a a, Monoid s1, Monoid s2) => Iso' s1 s2
recons = iso (unfoldr uncons) (unfoldr uncons)

unfoldr :: (Cons s2 s2 a a, Monoid s2) => (s1 -> Maybe (a, s1)) -> s1 -> s2
unfoldr f = maybe mempty (\(a, s') -> cons a (unfoldr f s')) . f

{-# RULES "recons/id" recons = id #-}

{-# RULES "recons/string-text" recons = reconsStringText #-}
{-# RULES "recons/text-string" recons = from reconsStringText #-}
reconsStringText :: Iso' String T.Text
reconsStringText = packed

{-# RULES "recons/string-lazytext" recons = reconsStringLazyText #-}
{-# RULES "recons/lazytext-string" recons = from reconsStringLazyText #-}
reconsStringLazyText :: Iso' String TL.Text
reconsStringLazyText = packed

{-# RULES "recons/text-strict" recons = reconsStrictText #-}
{-# RULES "recons/text-lazy" recons = from reconsStrictText #-}
reconsStrictText :: Iso' TL.Text T.Text
reconsStrictText = strict

{-# RULES "recons/list-bs" recons = reconsListByteString #-}
{-# RULES "recons/bs-list" recons = from reconsListByteString #-}
reconsListByteString :: Iso' [Word8] B.ByteString
reconsListByteString = packedBytes

{-# RULES "recons/list-lazybs" recons = reconsListLazyByteString #-}
{-# RULES "recons/lazybs-list" recons = from reconsListLazyByteString #-}
reconsListLazyByteString :: Iso' [Word8] L.ByteString
reconsListLazyByteString = packedBytes

{-# RULES "recons/bs-strict" recons = reconsStrictText #-}
{-# RULES "recons/bs-lazy" recons = from reconsStrictByteString #-}
reconsStrictByteString :: Iso' L.ByteString B.ByteString
reconsStrictByteString = strict
