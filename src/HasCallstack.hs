{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module HasCallstack
  ( HasCallstack( callsitelist, callstack, renderCS, renderCSNE, renderCS0
                , renderCS0NE, stackhead )
  , renderCCS, renderCCSNE, renderCCS0, renderCCS0NE
  )
where

import Prelude  ( (+), fromIntegral )

-- base --------------------------------

import GHC.Foreign  as  GHC

import Control.Applicative  ( pure )
import Control.Monad        ( return )
import Data.Bifunctor       ( first )
import Data.Bool            ( otherwise )
import Data.Foldable        ( Foldable, maximum )
import Data.Function        ( ($), (&), const, id )
import Data.Functor         ( Functor, fmap )
import Data.List            ( intercalate, reverse )
import Data.List.NonEmpty   ( NonEmpty, nonEmpty )
import Data.Maybe           ( Maybe( Just, Nothing ), fromMaybe )
import Data.String          ( String )
import Data.Tuple           ( fst, snd )
import Foreign.Ptr          ( Ptr, nullPtr )
import GHC.Stack            ( CallStack, CostCentre, CostCentreStack, SrcLoc
                            , ccsCC, ccsParent, ccLabel, ccModule, ccSrcSpan
                            , fromCallSiteList, getCallStack, srcLocEndCol
                            , srcLocEndLine, srcLocFile, srcLocModule
                            , srcLocPackage, srcLocStartCol, srcLocStartLine
                            )
import System.IO            ( IO, utf8 )
import Text.Show            ( show )

-- base-unicode-symbols ----------------

import Data.Bool.Unicode      ( (∧) )
import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- lens --------------------------------

import Control.Lens  ( Lens', lens, view )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳), (⩺) )
import Data.MoreUnicode.Lens     ( (⊣), (⊢) )
import Data.MoreUnicode.Monad    ( (≪) )
import Data.MoreUnicode.Monoid   ( ю )
import Data.MoreUnicode.String   ( 𝕊 )
import Data.MoreUnicode.Text     ( 𝕋 )

-- natural -----------------------------

import Natural  ( ℕ, length )

-- safe --------------------------------

import Safe  ( headMay )

-- strings -----------------------------

import Data.Strings  ( Str, strPadRight )

-- text --------------------------------

import Data.Text  ( pack )

--------------------------------------------------------------------------------

rPad ∷ Str σ ⇒ ℕ → σ → σ
rPad = strPadRight ' ' ∘ fromIntegral

----------------------------------------

renderStackLine ∷ (ℕ,ℕ,ℕ,ℕ) → (𝕊,SrcLoc) → 𝕊
renderStackLine (nme_len,pkg_len,mod_len,fle_len) (fname,loc) =
  let to   x y = x ⊕ "→" ⊕ y
      toS  x y = to (show x) (show y)
      col  l c = l ⊕ "[" ⊕ c ⊕ "]"
      colS l c = col (show l) (show c)
      pkg = rPad pkg_len $ srcLocPackage   loc
      mod = rPad mod_len $ srcLocModule    loc
      fn  = rPad fle_len $ srcLocFile      loc
      sc  = srcLocStartCol  loc
      sl  = srcLocStartLine loc
      ec  = srcLocEndCol    loc
      el  = srcLocEndLine   loc
      st  = colS sl sc
      ed  = colS el ec
      src = ю [ pkg, ":", mod, ":" ⊕ fn ]
      lc = if sl ≡ el
           then ю [ col (show sl) (sc `toS` ec) ]
           else st `to` ed
   in -- nme_len+2 for the surrounding diareses
      ю [ (rPad (2 + nme_len) (ю [ "«", fname, "»" ]))
        , " (", src, "#", lc, ")" ]

------------------------------------------------------------

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

  {- | Render a callstack as a list of lines, with aligned package, module,
       file. -}
  renderCS ∷ α → [𝕊]
  renderCS (view callsitelist → css) =
    let nme_len = maximum $ length ∘ fst ⊳ css
        mlen ∷ (Foldable ψ, Functor ψ) ⇒ (SrcLoc → 𝕊) → ψ (α,SrcLoc) → ℕ
        mlen f = maximum ∘ (length ∘ f ∘ snd ⊳)
        pkg_len = mlen srcLocPackage css
        mod_len = mlen srcLocModule  css
        fle_len = mlen srcLocFile    css
     in renderStackLine (nme_len,pkg_len,mod_len,fle_len) ⊳ css

  {- | Render a callstack as a list of lines, with aligned package, module,
       file; an empty callstack returns an explicit "NO CALLSTACK". -}
  renderCSNE ∷ α → NonEmpty 𝕊
  renderCSNE = fromMaybe (pure "NO CALLSTACK") ∘ nonEmpty ∘ renderCS

  {- | Render a callstack as a list of lines, with no alignment. -}
  renderCS0 ∷ α → [𝕊]
  renderCS0 (view callsitelist → css) = renderStackLine (0,0,0,0) ⊳ css

  {- | Render a callstack as a list of lines, with no alignment.  An empty
       callstack returns an explicit "NO CALLSTACK". -}
  renderCS0NE ∷ α → NonEmpty 𝕊
  renderCS0NE = fromMaybe (pure "NO CALLSTACK") ∘ nonEmpty ∘ renderCS0

instance HasCallstack CallStack where
  callstack = id

instance HasCallstack [(String,SrcLoc)] where
  callstack = lens fromCallSiteList (const getCallStack)

------------------------------------------------------------

{- | Source Locations as reported by the profiler -}
data ProfSrcLoc = ProfSrcLoc { _label ∷ 𝕊, _mod ∷ 𝕊, _src ∷ 𝕊 }

{-| find the max length of each of the label and mod of a list of ProfSrcLocs -}
srcLocLengths ∷ NonEmpty ProfSrcLoc → (ℕ,ℕ)
srcLocLengths sls =
  (maximum $ length ∘ _label ⊳ sls, maximum $ length ∘ _mod ⊳ sls)

srcLocToStr ∷ (ℕ,ℕ) → ProfSrcLoc → 𝕊
srcLocToStr (label_width,module_width) srcLoc =
  intercalate " " [ rPad label_width (_label srcLoc)
                  , (rPad module_width (_mod srcLoc))
                  , (_src srcLoc)
                  ]

mkProfSrcLoc ∷ Ptr CostCentre → IO ProfSrcLoc
mkProfSrcLoc cc =
  if cc ≡ nullPtr
  then return $ ProfSrcLoc "NULL CC PTR" "NULL CC PTR" "NULL CC PTR"
  else do lbl ← GHC.peekCString utf8 ≪ ccLabel cc
          mdl ← GHC.peekCString utf8 ≪ ccModule cc
          loc ← GHC.peekCString utf8 ≪ ccSrcSpan cc
          return $ ProfSrcLoc lbl mdl loc

ccsToProfSrcLocs ∷ Ptr CostCentreStack → IO [ProfSrcLoc]
ccsToProfSrcLocs ccs0 = go ccs0 []
  where
    go ccs acc
     | ccs ≡ nullPtr = return acc
     | otherwise = do
        cc  <- ccsCC ccs
        sl  ← mkProfSrcLoc cc
        parent <- ccsParent ccs
        if "MAIN" ≡_mod sl ∧ "MAIN" ≡ _label sl
        then return acc
        else go parent (sl : acc)

{- | Render a callstack to a list of strings.  The strings will be aligned on
     label & module. -}
renderCCS ∷ Ptr CostCentreStack → IO [𝕊]
renderCCS ccs = reverse ⊳ do
  srcLocs ← ccsToProfSrcLocs ccs
  case nonEmpty srcLocs of
    Nothing  → return []
    Just sls → let (label_len, module_len) = srcLocLengths sls
                 in return $ srcLocToStr (label_len, module_len) ⊳ srcLocs

{- | Render a callstack to a list of strings.  The strings will be aligned on
     label & module.  An empty callstack returns an explicit message to that
     effect.
 -}
renderCCSNE ∷ Ptr CostCentreStack → IO (NonEmpty 𝕊)
renderCCSNE =
  let nostack = "NO STACK AVAILABLE (was this built with profiling enabled?)"
   in fromMaybe (pure nostack) ∘ nonEmpty ⩺ renderCCS

{- | Render a callstack to a list of strings with no alignment. -}
renderCCS0 ∷ Ptr CostCentreStack → IO [𝕊]
renderCCS0 ccs = reverse ⊳ do
  srcLocs ← ccsToProfSrcLocs ccs
  case nonEmpty srcLocs of
    Nothing → return []
    Just _  → return $ srcLocToStr (0,0) ⊳ srcLocs

{- | Render a callstack to a list of strings with no alignment.    An empty
     callstack returns an explicit message to that effect. -}
renderCCS0NE ∷ Ptr CostCentreStack → IO (NonEmpty 𝕊)
renderCCS0NE =
  let nostack = "NO STACK AVAILABLE (was this built with profiling enabled?)"
   in fromMaybe (pure nostack) ∘ nonEmpty ⩺ renderCCS0

-- that's all, folks! ----------------------------------------------------------
