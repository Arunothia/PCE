module Main where
import PCE
import Data.List.Split
import Data.List

readInt :: IO [Int]
readInt = fmap (Prelude.map read. Prelude.words) getLine

main = do
        putStrLn "*** PCE Implemented in Haskell ***"
        putStrLn "Enter value of Vocabulary 'n' (Int n > 0)"
        nInput <- getLine
        let n = read nInput :: Int
        putStrLn "Enter E_ref (Each clause is space seperated literals) and the clauses are seperated by ','"
        inputERef <- getLine
        let tmp  = splitOneOf "," inputERef
        let eRef = (Prelude.map ((Prelude.map read).words) tmp)
        let phi  = map (map (* (-1))) eRef
        let answer = pce eRef phi
        _  <- print answer
        print $length answer
