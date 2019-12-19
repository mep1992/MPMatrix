module MPMatrix 
( mAdd
, mSub
, scalarMult
, Row(..)
, Matrix(..)
) where

data Row a = Row [a] deriving (Eq)
data Matrix a = Matrix [Row a] deriving (Eq)

showRow :: (Num a, Show a) => Row a -> String
showRow (Row x) = "( " ++ (init $ tail $ show x) ++ " )"

showMatrix :: (Num a, Show a) => Matrix a -> String
showMatrix (Matrix rows) = tail $ foldl (\x y -> x ++ "\n" ++ (show y)) "" rows

instance (Num a, Show a) => Show (Row a) where
    show = showRow

instance Functor Row where
    fmap f (Row a) = Row $ map f a

instance Functor Matrix where
    fmap f (Matrix rows) = Matrix $ map (fmap f) rows

instance (Num a, Show a) => Show (Matrix a) where
    show = showMatrix

mAdd :: (Num a) => Matrix a -> Matrix a -> Matrix a
mAdd x y = combineMatrix (+) x y

mSub :: (Num a) => Matrix a -> Matrix a -> Matrix a
mSub x y = combineMatrix (-) x y

scalarMult :: (Num a) => a -> Matrix a -> Matrix a
scalarMult x m = fmap (*x) m

prependRow :: (Num a) => Row a -> Matrix a -> Matrix a
prependRow row (Matrix rows) = Matrix (row:rows)

combineMatrix :: (Num a) => (a -> a -> a) -> Matrix a -> Matrix a -> Matrix a
combineMatrix _ (Matrix []) (Matrix []) = Matrix []
combineMatrix f (Matrix (x:xs)) (Matrix (y:ys)) = prependRow (zipRow f x y) (combineMatrix f (Matrix xs) (Matrix ys))

zipRow :: (Num a) => (a -> a -> a) -> Row a -> Row a -> Row a
zipRow f (Row xs) (Row ys) = Row (zipWith f xs ys)