-----------------------------------------------------------
-- |
-- module:                      GL.MissingH.TH
-- copyright:                   (c) 2016 sighingnow
-- license:                     MIT
-- maintainer:                  sighingnow@gmail.com
--
-- Template Haskell utilities:
--
-- 1. Raw string literals and simple template render using quasiquotation.
--
{-# LANGUAGE TemplateHaskell #-}

module GL.MissingH.TH ( raw ) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote  ( QuasiQuoter(..) )

raw :: QuasiQuoter
raw = QuasiQuoter { quoteExp = litE . stringL
                  , quotePat = error "Not Supported"
                  , quoteType = error "Not Supported"
                  , quoteDec = error "Not Supported"
                  }
