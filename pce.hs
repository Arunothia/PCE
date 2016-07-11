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

fromSolution :: Solution a -> a
fromSolution Unsatisfiable = error "Error: fromSolution detected no solution"
fromSolution (Solution x)  = x

-- Print Function for debugging
-- To debug any variable just call $unsafePerformIO $debugPrint <variable>

debugPrint x = do
                _ <- print x
                return x

-- negated Function
-- Negates CNF form and gives back the respective DNF form
-- or Negates DNF form and gives back the respective CNF form ( Using De morgan's Law)

negated :: [[Int]] -> [[Int]]
negated phi = map (map (* (-1))) phi

-- toCNF Function
-- Takes a DNF form and converts it to a CNF form.

toCNF :: [[Int]] -> [[Int]]
toCNF phi = phi -- TO BE COMPLETED

 

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
-- It takes mu (in CNF form)  and eRef (in CNF form)  as the input and gives mus(mu) (in CNF form)  as the output.

mus :: [[Int]] -> [[Int]] -> [[Int]]
mus mu eRef = mu -- TO BE COMPLETED


------------------------------------------------------------------------------------------------------------

-- pce Function
-- It takes List of list of integers as input that represent the CNF of eRef
-- It takes List of list of integers as input that represent the DNF of phi (as given in algorithm)
-- It outputs List of list of integers that represent the DNF of the Propagation complete form of eRef

pce :: [[Int]] -> [[Int]] -> [[Int]]
pce eRef phi e
	| (not z)   = e
	| otherwise = pce eRef (phi++muPrimeNeg) (e++muPrimeNeg)
	where 	z 	   = isSolution mu
		mu 	   = unsafePerformIO $ Picosat.solve $ toCNF phi 
		muPrime    = mus (fromSolution mu) eRef
		muPrimeNeg = negated muPrime
