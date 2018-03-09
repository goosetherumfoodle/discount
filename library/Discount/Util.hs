module Discount.Util where

import           Control.Exception         (throwIO)
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc (Doc, pretty)

throwLeft :: Either String a -> IO a
throwLeft = either handleException return
  where handleException = throwIO . userError

prettyTxt :: Text -> Doc ann
prettyTxt = pretty
