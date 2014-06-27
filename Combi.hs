module Main where

-- Produce shell commands to tile the tiny sudoku numbers in all possible
-- combinations. From directory public/images, run:
--
--   runhaskell ../../Combi.hs | sh
--
-- Of course, this should be done by sbt and ideally in Scala.
-- Haskell is just so nice to use for 'scripting'.

main
    = mapM_ putStrLn $ zipWith mkCommand combis bitPats
  where
    -- All lists of length 9 in which the i'th element is either 0 or i.
    combis  = sequence [[0,x] | x <- [1..9]]
    -- Sum the 2-powers of each non-zero element, so we get the bit patterns
    -- used in React.
    bitPats = map (sum . map ((2^) . pred) . filter (>0)) combis

    mkCommand combi bitPat = unwords
      ( "montage"
      : map ((++ ".png") . show) combi
     ++ ["-geometry", "+1+1", "-tile", "3x3", "pat" ++ show bitPat ++ ".png"]
      )
