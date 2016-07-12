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
dnfToCNF _ [] 	= []
dnfToCNF n (x:xs) = ((n+1):map (* (-1)) x):rest
	where 	rest = (foldl' f [] x) ++ (dnfToCNF (n+1) xs)
		f lst l = lst ++ [(-(n+1)):[l]]	
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
-- And mus function for performing Minimal Unsatisfiable Subsets as given in the paper -
-- "Enumerating Infeasibility: Finding Multiple MUSes Quickly"
 
------------------------------------------------------------------------------------------------------------

-- mus Function
-- It takes mu (list of integers), eRef (in CNF form) and list of interested variables as the input.
-- It gives mus(mu) (list of integers)  as the output.
-- List of Integers representation of mu - 
-- Example - [-1,2,0,-4] means (1,4) are assigned False and (3) is not assigned or is question and (2) is assigned True.

mus :: [Int] -> [[Int]] -> [Int] -> [Int]
mus mu eRef lst = mu -- TO BE COMPLETED


------------------------------------------------------------------------------------------------------------

-- pce Function
-- It takes List of Interested variables (Integer List).
-- It takes List of list of integers as input that represent the CNF of eRef
-- It takes List of list of integers as input that represent the CNF of phi (as given in algorithm)
-- It outputs List of list of integers that represent the CNF of the Propagation complete form of eRef

pce :: [Int] -> [[Int]] -> [[Int]] -> [[Int]]
pce lst eRef dnfPhi = z
	where z = pceHelper lst eRef (dnfToCNF n dnfPhi) []
	      n = maximum (map maximum phiAbs)
	      phiAbs = map (map abs) dnfPhi

pceHelper :: [Int] -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]]
pceHelper lst eRef phi e
	| (not z)   = e
	| otherwise = pceHelper lst eRef (phi++muPrimeNeg) (e++muPrimeNeg)
	where 	z 	   = isSolution mu
		n 	   = maximum (map maximum phi)
		mu 	   = unsafePerformIO $Picosat.solve phi 
		muPrime    = mus (fromSolution mu) eRef lst
		muPrimeNeg = [map (* (-1)) muPrime]
