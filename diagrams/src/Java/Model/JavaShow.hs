module Java.Model.JavaShow (
    JavaShow(..)
  ) where

import Data.List (intercalate)

class JavaShow a where
  javaShow :: a -> String

instance JavaShow () where
  javaShow = show

instance JavaShow Int where
  javaShow = show

instance JavaShow Double where
  javaShow = show

instance JavaShow Bool where
  javaShow False = "false"
  javaShow True  = "true"

instance JavaShow a => JavaShow [a] where
  javaShow xs = "[" ++ intercalate ", " (fmap javaShow xs) ++ "]"
