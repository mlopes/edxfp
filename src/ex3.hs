import Prelude hiding ((||), (&&))

main:: IO ()
main = print 'a'

halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
  where
    n = length xs `div` 2

safetail :: [t] -> [t]
safetail [x] = [x]
safetail (_ : xs) = xs

b || False = b
_ || True = True

a && b = if b then a else False

remove n xs = take n xs ++ drop (n +1) xs
