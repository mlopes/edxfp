main:: IO ()
main = print 'a'

qsort [] = []
qsort xs = x : qsort larger ++ qsort smaller
  where x = maximum xs
        smaller = [a | a <- xs, a < x]
        larger = [b | b <- xs, b >= x ]
