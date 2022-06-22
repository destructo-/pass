module Domain.Utils.ListUtils (
    maybeHead,
    safeTail
) where


maybeHead :: [a] -> Maybe a
maybeHead (x : xs) = Just x
maybeHead []       = Nothing


safeTail :: [a] -> [a]
safeTail []       = []
safeTail (x : xs) = xs