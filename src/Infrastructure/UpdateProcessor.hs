module Infrastructure.UpdateProcessor (process) where


import Domain.Record (Name, Mark)


process :: Name -> Maybe Mark -> IO ()
process name maybeMark = putStrLn "upd"