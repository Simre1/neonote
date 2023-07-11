module Data.Functor.Empty where
import GHC.Generics (Generic)

data Empty a = Empty deriving (Show, Eq, Ord, Generic)
