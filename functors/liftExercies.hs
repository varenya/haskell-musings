a = fmap (+1) $ read "[1]" :: [Int]

b = ((fmap . fmap)  (++ "lol")) $ Just ["hi, ", "Hello "]

c = (*2) . (\x -> x-2)

d x = do
   let output = (\x -> [x, 1..3])
   return $ ((\x -> "1" ++ x) . show) $ output x

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap read $ fmap (("123"++) . show) ioi
    in  fmap (*3) changed

