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

-- Join a list together using separator
-- in Interpserse.hs

-- Function that calculates the height of a tree
-- in Tree.hs

-- Direction data type
data Direction = DLeft 
               | DRight 
               | DStraight
                 deriving (Show)

-- Given three 2D coordinates A, B, and C, 
-- calculate the direction that it goes when going from A->B to B->C
data Point2D = Point2D Double Double
               deriving (Show)

turn :: Point2D -> Point2D -> Point2D -> Direction
turn a b c
    | crossProd a b c > 0 = DLeft
    | crossProd a b c == 0 = DStraight
    | crossProd a b c < 0 = DRight
    where crossProd (Point2D x1 y1) (Point2D x2 y2) (Point2D x3 y3)
            = (x2 - x1)*(y3 - y1) - (y2 - y1)*(x3 - x1)

-- Given a list of points, it returns a list of the turns made on the path
calcTurns :: [Point2D] -> [Direction]
calcTurns points
    | length points < 3 = []
calcTurns (a:b:c:points) = (turn a b c):(calcTurns (b:c:points))

-- Graham's Scan Algorithm
-- 1. Start with point, P,  with lowest Y coordinate (also lowest X if multiple with same Y)
-- 2. Sort points in increasing order based on angle to P
-- 3. Go through the points, discarding those which make a right turn (means it is not convex hull)

