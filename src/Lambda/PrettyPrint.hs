module Lambda.PrettyPrint (
  module Lambda.PrettierPrettyPrint
  ) where

import Lambda.Untyped
import Lambda.PrettierPrettyPrint
import Text.PrettyPrint.ANSI.Leijen

instance Show Lambda where
  show = ($ "") . displayS . renderSmart 1 40 . pretty
