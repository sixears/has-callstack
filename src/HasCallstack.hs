{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module HasCallstack
  ( HasCallstack( callsitelist, callstack, stackhead ) )
where

-- base --------------------------------

import Data.Bifunctor  ( first )
import Data.Function   ( ($), (&), const, id )
import Data.Functor    ( fmap )
import Data.Maybe      ( Maybe )
import Data.String     ( String )
import GHC.Stack       ( CallStack, SrcLoc, fromCallSiteList, getCallStack )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- lens --------------------------------

import Control.Lens  ( Lens', lens, view )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens     ( (⊣), (⊢) )
import Data.MoreUnicode.Text     ( 𝕋 )

-- safe --------------------------------

import Safe  ( headMay )

-- text --------------------------------

import Data.Text  ( pack )

--------------------------------------------------------------------------------

class HasCallstack α where
  -- we can't call this callStack because that's already defined in GHC.Stack
  callstack    ∷ Lens' α CallStack
  -- all lowercase for consistency with callstack
  callsitelist ∷ Lens' α [(String,SrcLoc)]
  callsitelist = lens (\ a → getCallStack $ a ⊣ callstack)
                      (\ a csl → a & callstack ⊢ (fromCallSiteList csl))
  -- all lowercase for consistency with callstack
  stackhead ∷ α → Maybe (𝕋,SrcLoc)
  stackhead = fmap (first pack) ∘ headMay ∘ getCallStack ∘ view callstack


instance HasCallstack CallStack where
  callstack = id

instance HasCallstack [(String,SrcLoc)] where
  callstack = lens fromCallSiteList (const getCallStack)

----------------------------------------

-- that's all, folks! ----------------------------------------------------------
