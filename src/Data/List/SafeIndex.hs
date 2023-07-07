module Data.List.SafeIndex where

(!?) :: [a] -> Int -> Maybe a
[] !? _ = Nothing
(a : _) !? 0 = Just a
(_ : as) !? i = as !? (i - 1)
