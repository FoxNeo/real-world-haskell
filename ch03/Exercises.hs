-- file: ch03/Exercises.hs
import Data.List

-- Function that returns the length of a list
listLength :: [a] -> Int
listLength (x:xs) = 1 + listLength xs
listLength [] = 0

-- Compute the mean of a list
listMean xs  
    | length xs == 0 = 0
    | otherwise = sum(xs) / n
        where
            n = fromIntegral(listLength xs)
            sum (y:ys)
                | ys == [] = y
                | otherwise = y + sum(ys)

-- Turn a list into a palindrome
palindrome xs
    | length xs == 0 = []
    | otherwise = xs ++ (rev xs)
                    where
                        rev (y:ys)
                            | ys == [] = [y]
                            | otherwise = (rev ys) ++ [y]

-- Palindrome checker
isPalindrome xs
    | length xs == 0 = True
    | otherwise = (head xs) == (last xs) 
        && isPalindrome (take ((length xs) - 2) (tail xs)) 

-- Sort by length
lengthSort xs = sortBy compareByLength xs
                    where compareByLength a b = compare (length a) (length b)

