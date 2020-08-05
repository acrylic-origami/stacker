{-# LANGUAGE MultiWayIf #-}

foo x | Just y <- x = case y of
  z -> if | Nothing <- x -> 1
          | () <- y -> const 2 $ do
           a <- Nothing
           Nothing
foo _ = 3

a !! b = ()

main :: IO ()
main = pure ()