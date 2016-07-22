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
		let n = length intLst
		putStrLn "Enter E_ref (Each clause is space seperated literals) and the clauses are seperated by ','"
                inputERef <- getLine
                let tmp  = splitOneOf "," inputERef
                let eRef = (Prelude.map ((Prelude.map read).words) tmp)
                let phiIni = map f [1..n] 	-- We use the integer value of the variable for xTrue and n+ that for xFalse
			where f l = [-l,-(n+l)]
        	let answer = pce intLst eRef phiIni []
        	_  <- print answer
       		print $length answer
