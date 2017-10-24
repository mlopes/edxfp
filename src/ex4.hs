import Data.Char

main:: IO ()
main = print 'a'

pyths n
  = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n],
      x ^ 2 + y ^ 2 == z ^ 2]

factors :: Int -> [Int]
factors n =
  [x | x <- [1..n], n `mod` x == 0]

perfects n = [x | x <- [1..n], isPerfect x]
  where
    isPerfect num = sum (init (factors num)) == num

find :: (Eq a) =>  a -> [(a, t)] -> [t]
find k t = [v | (k', v) <- t, k == k']

positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..n])
  where
    n = length xs -1

scalarproduct xs ys = sum [x * y | (x, y) <- xs `zip` ys]

let2int:: Char -> Int
let2int c
  | isLower c = ord c - ord 'a'
  | otherwise = ord c - ord 'A'

int2let :: Int -> Char
int2let n
  | n < ord 'a' = chr (ord 'A' + n)
  | otherwise = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
  | isAlpha c = int2let ((let2int c + n) `mod` 26)
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
