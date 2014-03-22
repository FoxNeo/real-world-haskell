-- file: ch04/Exercises.hs
import Data.Char (digitToInt)
import Data.List

-- Safe versions of partial list functions
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a:_) = Just a

safeTail :: [a] -> Maybe [a]
safeTail a = case a of
                [] -> Nothing
                [x] -> Nothing 
                (_:xs) -> Just xs

safeLast :: [a] -> Maybe a
safeLast list
    | null list = Nothing
    | otherwise = Just (last list)

safeInit :: [a] -> Maybe [a]
safeInit a = case a of
                [] -> Nothing
                [x] -> Nothing
                (x:xs) -> Just (x: (safeRemain (safeInit xs)))
                where safeRemain rem = case rem of
                                        Nothing -> []
                                        Just r -> r
-- we need the safeRemain to convert the recurive remainder from Mabye [a] to [a] in order to concat it

-- but this is a better, more 'haskellish' way to solve it
safeListFunc func [] = Nothing
safeListFunc func xs = Just (func xs)

betterSafeHead = safeListFunc head
betterSafeTail = safeListFunc tail
betterSafeLast = safeListFunc last
betterSafeInit = safeListFunc init

-- splitWith takes any type of predicate and a list of any time, and splits when the predicate is false
-- probably not the best/cleanest way to write it, but it seems to work
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith func list = case (break func list) of
                        (x, []) -> [x] 
                        ([], y) -> splitWith func (tail y)
                        (x, y) -> x : (splitWith func (tail y))


-- Rewriting asInt using fold
asInt_fold :: String -> Integer
asInt_fold ('-':xs) = -1 * (asInt_fold xs)
asInt_fold s = foldl' step 0 s
               where step acc char = acc*10 + toInteger (digitToInt char)

-- Rewriting concat using foldr
myConcat :: [[a]] -> [a]
myConcat  = foldr (++) [] 

-- versions of takeWhile using explicit recursion and foldr
takeWhileR p [] = []
takeWhileR p (x:xs) | p x = x : (takeWhileR p xs)
                    | otherwise = [] 

takeWhileF p = foldr step []
               where step x acc | p x = x : acc 
                                | otherwise = []



