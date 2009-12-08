safeHead [] = Nothing
safeHead xs = Just (head xs)

safeTail [] = Nothing
safeTail xs = Just (tail xs)

safeLast [] = Nothing
safeLast xs = Just (last xs)

safeInit [] = Nothing
safeInit xs = Just (init xs)
