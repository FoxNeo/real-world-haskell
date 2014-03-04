-- file ch03/MySecond.hs
-- errors happen sometimes, but they crash the program
mySecond :: [a] -> a
mySecond xs = if null (tail xs)
              then error "list too short"
              else head (tail xs)

-- This is safer, return a Maybe, so then the caller can deal with it
safeSecond :: [a] -> Maybe a
safeSecond[] = Nothing
safeSecond xs = if null (tail xs)
                then Nothing
                else Just (head (tail xs))

-- But we can make it even cleaner:
tidySecond :: [a] -> Maybe a
tidySecond (_:x:_) = Just x
tidySecond _ = Nothing
