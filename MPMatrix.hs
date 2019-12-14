data Matrix a = Matrix [[a]] deriving (Show)

mAdd :: (Num a) => [[a]] -> [[a]] -> [[a]]
mAdd as bs = combineMatrix (+) as bs

mSub :: (Num a) => [[a]] -> [[a]] -> [[a]]
mSub as bs = combineMatrix (-) as bs

combineMatrix :: (Num a) => (a -> a -> a) -> [[a]] -> [[a]] -> [[a]]
combineMatrix _ [] [] = []
combineMatrix f (a:as) (b:bs) = (combineList f a b) : (combineMatrix f as bs)

combineList :: (Num a) => (a -> a -> a) -> [a] -> [a] -> [a]
combineList _ [] [] = []
combineList f (x:xs) (y:ys) = (f x y) : (combineList f xs ys)