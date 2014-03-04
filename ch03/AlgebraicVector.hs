-- file: ch03/AlgebraicVector.hs
-- x and y coordinates or lengths
data Cartesian2D = Cartesian2D Double Double
                   deriving (Eq, Show)

-- Angle and Distance (magnitude)
data Polar2D = Polar2D Double Double
               deriving (Eq, Show)
