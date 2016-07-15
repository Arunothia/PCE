module Main where
import PCE
import Data.List.Split
import Data.List

readInt :: IO [Int]
readInt = fmap (Prelude.map read. Prelude.words) getLine

main = do
	putStrLn "Enter \n1: Debug dnfToCNF"
        putStrLn "ANY OTHER NUMBER: PCE"
        debugInput <- getLine
        let debug = read debugInput :: Int
	if debug == 1 then do
		putStrLn "*** DEBUG dnfToCNF ***"
		putStrLn "Enter value of Vocabulary 'n' (Int n > 0)"
        	nInput <- getLine
        	let n = read nInput :: Int
		putStrLn "Enter DNF formula (Each clause is space seperated literals) and the clauses are seperated by ','"
                inputERef <- getLine
                let tmp  = splitOneOf "," inputERef
                let e = (Prelude.map ((Prelude.map read).words) tmp)
		print $dnfToCNF n e

	else do
        	putStrLn "*** PCE Implemented in Haskell ***"
		putStrLn "Enter the variables of interest (list of integers)"
                intLst <- readInt
		putStrLn "Enter E_ref (Each clause is space seperated literals) and the clauses are seperated by ','"
                inputERef <- getLine
                let tmp  = splitOneOf "," inputERef
                let eRef = (Prelude.map ((Prelude.map read).words) tmp)
                let dnfPhi  = map (map (* (-1))) eRef

        	let answer = pce intLst eRef dnfPhi
        	_  <- print answer
       		print $length answer
