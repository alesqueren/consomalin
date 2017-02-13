module Drive.Re
  ( module Drive.Re
  , Regex
  ) where

import           Prelude                (String)
import           Protolude

import           Control.Error          (rightZ)
import           Text.Regex.TDFA        (Regex, defaultCompOpt, defaultExecOpt)
import           Text.Regex.TDFA.String (compile)


data InvalidREException = InvalidREException deriving (Show, Typeable)
instance Exception InvalidREException

compileRe :: String -> Either String Regex
compileRe = compile defaultCompOpt defaultExecOpt

compileReM :: (MonadPlus m) => String -> m Regex
compileReM = rightZ . compileRe
