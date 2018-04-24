import Data.Monoid

newtype Mem s a = Mem
  { runMem :: s -> (a, s)
  }

combineMem :: Monoid c => (d -> (c,d)) -> (d -> (c,d)) -> d -> (c,d)
combineMem f g arg =  (finalX,finalY)
            where 
                (x1,y1) = g arg
                (x2,finalY) = f y1
                finalX = (mappend x1 x2)

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem {runMem = (\s -> (mempty, s))}
  Mem {runMem = f} `mappend` Mem {runMem = g} =
    Mem {runMem = combineMem f g}

f' :: Mem Int String
f' = Mem $ \s -> ("hi",s+1)

main :: IO ()
main = do
    let rmzero = runMem mempty 0
        rmleft = runMem (f' <> mempty) 0
        rmright = runMem (mempty <> f') 0
    print $ rmleft
    print $ rmright
    print $ (rmzero :: (String,Int))
    print $ rmleft == runMem f' 0
    print $ rmright == runMem f' 0