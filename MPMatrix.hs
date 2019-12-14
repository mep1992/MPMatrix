module MPMatrix 
( mAdd
, mSub
, scalarMult
) where

data Row a = Row [a] deriving (Show)
data Matrix a = Matrix [Row a] deriving (Show)

mAdd :: (Num a) => Matrix a -> Matrix a -> Matrix a
mAdd x y = combineMatrix (+) x y

mSub :: (Num a) => Matrix a -> Matrix a -> Matrix a
mSub x y = combineMatrix (-) x y

scalarMult :: (Num a) => a -> Matrix a -> Matrix a
scalarMult x (Matrix rows) = Matrix (map (scalarRowMult x) rows)

scalarRowMult ::(Num a) => a -> Row a -> Row a
scalarRowMult x (Row xs) = Row (map (*x) xs)

prependRow :: (Num a) => Row a -> Matrix a -> Matrix a
prependRow row (Matrix rows) = Matrix (row:rows)

combineMatrix :: (Num a) => (a -> a -> a) -> Matrix a -> Matrix a -> Matrix a
combineMatrix _ (Matrix []) (Matrix []) = Matrix []
combineMatrix f (Matrix (x:xs)) (Matrix (y:ys)) = prependRow (combineRows f x y) (combineMatrix f (Matrix xs) (Matrix ys))

combineRows :: (Num a) => (a -> a -> a) -> Row a -> Row a -> Row a
combineRows f (Row xs) (Row ys) = Row (combineList f xs ys)

combineList :: (Num a) => (a -> a -> a) -> [a] -> [a] -> [a]
combineList _ [] [] = []
combineList f (x:xs) (y:ys) = (f x y) : (combineList f xs ys)