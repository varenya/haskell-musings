sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello " ++ x ++ "!")

circleArea :: Float -> Float
circleArea radius = pi * radius * radius
