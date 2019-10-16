-- Lets break this function down
-- type signature
sumOfN :: Int -> Int
-- pattern matching
sumOfN 0 = 0 -- recursion !
sumOfN n = n + sumOfN (n-1)


add :: Int -> Int -> (Int -> Int)
add x y z = x + y + z

addBy2 :: Int -> Int
addBy2 = add 2