data Employee = Coder | Manager | Veep | CEO deriving (Eq,Ord,Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' = putStrLn $ show e ++ " is is the boss of " ++ show e'

employeeRank :: Employee -> Employee -> IO ()
employeeRank e e' =
    case compare e e' of
        EQ -> putStrLn "Neither employee is the boss"
        GT -> reportBoss e e'
        LT -> (flip reportBoss) e e'


