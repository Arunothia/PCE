------------------------------------------------------------------------------------------------------------
-- Written by M.Arunothia as a part of research internship under Prof.Harald, University of Melbourne.
------------------------------------------------------------------------------------------------------------
module PCE where
-- For SAT Solver
import Picosat
-- For find Function for List manipulation
import Data.List
-- For Printing Debug Statements
import System.IO.Unsafe

------------------------------------------------------------------------------------------------------------
-- HELPER FUNCTIONS

-- fromJust Function

fromJust          :: Maybe a -> a
fromJust Nothing  = error "Error: fromJust detected Nothing"
fromJust (Just x) = x

-- isJust Function

isJust  :: Maybe a -> Bool
isJust Nothing  = False
isJust (Just _) = True

--isSolution Function

isSolution :: Solution -> Bool
isSolution Unsatisfiable = False
isSolution (Solution _)  = True

-- fromSolution Function

fromSolution :: Solution -> [Int]
fromSolution Unsatisfiable = error "Error: fromSolution detected no solution"
fromSolution (Solution x)  = x

-- Print Function for debugging
-- To debug any variable just call $unsafePerformIO $debugPrint <variable>

debugPrint x = do
                _ <- print x
                return x

-- dnfToCNF Function
-- Takes a DNF form and converts it to a CNF form using Tseytin transformation

dnfToCNF :: Int -> [[Int]] -> [[Int]]
dnfToCNF n dnf = cnf
	where cnf = (dnfToCNFHelper n dnf) ++ [[(n+1)..(n+length(dnf))]]

dnfToCNFHelper :: Int -> [[Int]] -> [[Int]]
dnfToCNFHelper _ [] 	= []
dnfToCNFHelper n (x:xs) = ((n+1):map (* (-1)) x):rest
	where 	rest = (foldl' f [] x) ++ (dnfToCNFHelper (n+1) xs)
		f lst l = lst ++ [(-(n+1)):[l]]	

-- applyMuClause Function
-- It applies the partial assignment mu to the clause passed

applyMuClause :: [Int] -> [Int] -> [Int]
applyMuClause m c = foldl' (f m) [] c
f _ [-1,1] _      = [-1,1]
f m tempLst l
	| (isJust $findIndex (==l) m)    = [-1,1]
	| (isJust $findIndex (==(-l)) m) = tempLst
	| otherwise                  = (Data.List.union tempLst [l])

------------------------------------------------------------------------------------------------------------

-- In this code, we are exploring the following algorithm to estimate the Propagation Complete Encodings for SAT Solvers.
{-	phi = negation(eRef)
	while phi is Satisfiable
		Let mu be the total assignment that satisfies phi
		mu' 	= MUS(mu) w.r.t eRef
		E 	= and E negation(mu')
		phi 	= and phi negation(mu')	 
-}
-- The above will be done using the Picosat SAT Solver as the pce function (for the main algorithm)
-- And mus function for finding  Minimal Unsatisfiable Subsets is done by taking reference of the paper -
-- "Enumerating Infeasibility: Finding Multiple MUSes Quickly"
 
------------------------------------------------------------------------------------------------------------

-- Grow Function
-- It takes the original assignment, eRef and the unsatisfying clause set mu as the input.
-- It outputs the grown assignment that acts as the MSS

grow :: [Int] -> [[Int]] -> [Int] -> [Int]
grow seed eRef mu
        | (cond seedPrime) = seed
        | otherwise      = grow seedPrime eRef mu
        where   cond []  = True
                cond var = not $isSolution $unsafePerformIO $Picosat.solve $eRefApplied var
                eRefApplied var = if (check var >0) then [[1],[-1]] else map (applyMuClause var) eRef
                check var       =  length $Prelude.filter (==[]) $map (applyMuClause var) eRef
                seedPrime  = if (seedPrimeList == []) then [] else (head seedPrimeList)
                seedPrimeList       = Prelude.filter (not.cond) $Prelude.filter (/=seed) (map addMore mu)
                addMore l        = Data.List.union seed [l]

-- Shrink Function 
-- It takes the original assignment and Eref as input
-- It outputs the shrunk assignment that acts as the MUS

shrink :: [Int] -> [[Int]] -> [Int]
shrink mu eRef
	| (mu == []) 	 = []
	| (cond muPrime) = mu
	| otherwise 	 = shrink muPrime eRef
	where 	cond []  = True
		cond var = isSolution $unsafePerformIO $Picosat.solve $eRefApplied var
		eRefApplied var = if (check var >0) then [[1],[-1]] else map (applyMuClause var) eRef
		check var	=  length $Prelude.filter (==[]) $map (applyMuClause var) eRef
		muPrime  = if (muPrimeList == []) then [] else (head muPrimeList)
		muPrimeList       = Prelude.filter (not.cond) (map leaveOut mu)
		leaveOut l        = Prelude.filter (/=l) mu

-- marco Function
-- It evaluates the full MUSes set using the MARCO-POLO Algorithm given in the paper.
-- It takes the mapping (empty map initially), mu (the set of unsatisfiable constraints) and eRef as its inputs.
-- It also takes MUSes as its input so that it can be evaluated recursively.
-- It returns the MUSes Set as the output.

marco :: [[Int]] -> [Int] -> [[Int]] -> [[Int]] -> [[Int]]
marco mapping mu eRef muses
	| (not cond) = muses
	| otherwise  = marco mapNew mu eRef newMus
	where	cond 	= isSolution mapSol
		mapSol	= unsafePerformIO $Picosat.solve mapping
		mapNew	= if seedSol then (Data.List.union mapping blockDown) else (Data.List.union mapping blockUp)
		mss	= grow seed eRef mu
		mus	= if seedSol then [] else shrink seed eRef
		newMus  = if (mus == []) then muses else Data.List.union muses [mus]
		seed	= foldl' useMu [] $fromSolution mapSol
		blockDown   = [foldl' g [] mu]
		blockUp	    = [foldl' f [] mu]
		f lst l     = if (isJust $findIndex (==l) mus) then lst ++ [-(fromJust(findIndex (==l) mu))-1] else lst
		g lst l	    = if (not $isJust $findIndex (==l) mss) then lst ++ [(fromJust(findIndex (==l) mu))+1] else lst
		useMu lst l = if (l>0) then (lst ++ [mu!!(l-1)]) else lst
		seedSol 	= isSolution $unsafePerformIO $Picosat.solve $eRefApplied seed
		eRefApplied var = if (check var >0) then [[1],[-1]] else map (applyMuClause var) eRef
                check var       = length $Prelude.filter (==[]) $map (applyMuClause var) eRef

-- mus Function
-- It takes mu (list of integers), eRef (in CNF form) and list of interested variables (1..n) as the input.
-- It gives mus(mu) (list of integers)  as the output.
-- List of Integers representation of mu - 
-- Example - [-1,2,-4] means (1,4) are assigned False and (3) is not assigned or is question and (2) is assigned True.

musfun :: [Int] -> [[Int]] -> Int -> [[Int]]
musfun mu eRef n = marco [] muNew eRef []
	where 	muNew = Prelude.filter (<=n) mu

------------------------------------------------------------------------------------------------------------

-- pce Function
-- It takes List of Interested variables (1..n) by taking the value of n.
-- It takes List of list of integers as input that represent the CNF of eRef
-- It takes List of list of integers as input that represent the CNF of phi (as given in algorithm)
-- It outputs List of list of integers that represent the CNF of the Propagation complete form of eRef

pce :: Int -> [[Int]] -> [[Int]] -> [[Int]]
pce n eRef dnfPhi = Prelude.filter (/=[]) z
	where z = pceHelper n eRef (dnfToCNF n dnfPhi) []

pceHelper :: Int -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]]
pceHelper n eRef phi e
	| (not z)   = e
	| otherwise = pceHelper n eRef phiNew eNew
	where 	phiNew 	   = Data.List.union phi muNeg
		eNew	   = Data.List.union e muPrimeNeg
		z 	   = isSolution muSol
		muSol 	   = unsafePerformIO $Picosat.solve phi 
		muNeg	   = [map (* (-1)) mu]
		muPrime    = musfun mu eRef n
		muPrimeNeg = map (map (* (-1))) muPrime
		mu	   = fromSolution muSol

