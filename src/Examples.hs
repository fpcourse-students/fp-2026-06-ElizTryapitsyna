-- | Данный код для чтения в задаче 1.1. Менять его не нужно.
module Examples where

data LogLevel = Error | Warning | Debug | Info

instance Show LogLevel where
  show Error = "ERROR"
  show Warning = "WARNING"
  show Debug = "DEBUG"
  show Info = "INFO"

render :: Show a => a -> String
render x = "[" ++ show x ++ "]"

data Labelled = Labelled String Int
  deriving Show

instance Eq Labelled where
  Labelled _ x == Labelled _ y = x == y
