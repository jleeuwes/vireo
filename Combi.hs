module Main where

-- Produceer commando's om de sudoku-nummers in alle combinaties te tilen tot
-- één plaatje.

main
    = mapM_ putStrLn $ zipWith mkCommand combis bitPats
  where
    combis  = sequence [[0,x] | x <- [1..9]]
    bitPats = map (sum . map ((2^) . pred) . filter (>0)) combis

    mkCommand cs bitPat = unwords
      ( "montage"
      : map ((++ ".png") . show) cs
     ++ ["-geometry", "+1+1", "-tile", "3x3", "pat" ++ show bitPat ++ ".png"]
      )
