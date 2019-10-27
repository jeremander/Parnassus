-- Utility

removeSingleChords :: Music -> Music
removeSingleChords = foldMusic go
    where
        go (Chord [(n,_)] d p) = Note n d p
        go x                   = x

notImpl :: String -> a
notImpl a = error $ "Not implemented: " ++ a

intLog2 :: (Integral a, Integral b) => a -> b
intLog2 = truncate . logBase 2 . fromIntegral

-- | Separate a duration into a sequence of undotted durations with some number of dots
separateDots :: Duration -> [DottedDuration]
separateDots (Duration r)
    | r == 0       = []
    | r >= 16      = (Duration 8, 0) : separateDots (Duration r - 8)
    | diff == 0    = [(Duration r, 0)]
    | n' == d' - 1 = [(Duration b, intLog2 d')]
    | otherwise    = (Duration b, 0) : separateDots (Duration diff)
    where
        (n, d) = (numerator r, denominator r)
        b = (2 ^ (intLog2 $ fromInteger n)) % d
        diff = r - b
        q = diff / b
        (n', d') = (numerator q, denominator q)

infixl <=>
a <=> b = sep [a,b]
