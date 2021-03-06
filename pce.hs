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

--makePA Function

makePA :: [PAValue] -> PA
makePA x = PA $Just x


--isIn Function

isIn :: [Int] -> Int -> Bool
isIn lst l = isJust $findIndex (==l) lst

--isNotIn Function

isNotIn :: [Int] -> Int -> Bool
isNotIn lst l = not $isJust $findIndex (==l) lst 

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

-- Data Type PAValue helps to define our partial assignment. It can be (True|False|Question).

data PAValue = PAFalse | PATrue | PAQuest
                deriving (Eq, Read, Show)

paValue :: PAValue -> Int
paValue PAFalse = -1
paValue PATrue  = 1
paValue PAQuest = 0

-- PA - The Type of Partial Assignment we will be dealing with

newtype PA = PA (Maybe([PAValue]))
                deriving (Eq, Read, Show)

-- To undo PA constructor use unPA Function

unPA :: PA -> Maybe([PAValue])
unPA (PA v) = v

-- lessThanPA Function 

lessThanPA :: PA -> PA -> Bool
lessThanPA (PA Nothing)  _              = False
lessThanPA (PA (Just p)) (PA Nothing)   = True
lessThanPA (PA (Just p)) (PA (Just q))  = ((length $Prelude.filter (==False) $zipWith isLessThan p q) == 0)

-- isLessThan Function

isLessThan :: PAValue -> PAValue -> Bool
isLessThan PAQuest PAQuest = True
isLessThan PAQuest _       = False
isLessThan _ PAQuest       = True
isLessThan p q             = if (p==q) then True else False

-- The following defines an order on Partial Assignments (The more undefined, higher the value)
-- The order should be more defined than just count of PAQuest, refer definition 

instance Ord PA where
  _ `compare` (PA Nothing) = GT
  (PA Nothing) `compare` _ = LT
  PA (Just p) `compare` PA (Just q)
        | (z /= EQ) = z
        | otherwise = zOrd
        where   z = length(Prelude.filter (== PAQuest) p) `compare` length(Prelude.filter (== PAQuest) q)
                px = findIndices (== PATrue) p
                qx = findIndices (== PATrue) q
                py = Data.List.map (+ lth) $findIndices (== PAFalse) p
                qy = Data.List.map (+ lth) $findIndices (== PAFalse) q
                pz = Data.List.union px py
                qz = Data.List.union qx qy
                lth = length p
                zOrd = qz `compare` pz
------------------------------------------------------------------------------------------------------------

-- paTop is the fully unidentified partial assignment.
-- Partial Assignment in this code is defined by the data type Maybe [PAValue]

paTop :: Int -> PA
paTop 0 = error "[paTop:] Empty Vocabulary"
paTop n = PA $Just (replicate (fromIntegral n) PAQuest)

------------------------------------------------------------------------------------------------------------

-- assign Function defines a partial assignment when a literal is specified.
-- Its first integer input stands for 'n' - that denotes the vocabulary.
-- It takes an integer (negative for negated variables) and returns a partial assignment.
-- When the integer passed is zero(which should'nt be the case),the partial assignment returned marks all literals a contradiction.

assign :: Int -> Int -> PA
assign n 0 = error "[assign:] Literal value cannot be '0'"
assign 0 _ = error "[assign:] Empty Vocabulary"
assign n l
        | (abs(l) > n) = error "Invalid Literal"
        | (l > 0) = PA $Just(init(first) ++ [PATrue] ++ second)
        | otherwise = PA $Just(init(first) ++ [PAFalse] ++ second)
        where  (first,second) = Data.List.splitAt (fromIntegral(abs(l))) (fromJust $unPA (paTop n))

------------------------------------------------------------------------------------------------------------

-- paMeet Function defines the 'meet' operator for combining two partial assignments.
-- If the two partial assignments lead to a contradiction, then we return Nothing (denoting contradiction).
-- Otherwise we return the unification of the two partial assignments.


paMeet :: PA -> PA -> PA
paMeet _ (PA Nothing) = PA Nothing
paMeet (PA Nothing) _ = PA Nothing
paMeet (PA (Just p)) (PA (Just q)) = PA $ sequence unifiedPA
        where unifiedPA = zipWith unifyPA p q
              unifyPA a b
                | (a == b) = (Just a)
                | (a == PAQuest) = (Just b)
		| (b == PAQuest) = (Just a)
                | otherwise = Nothing

------------------------------------------------------------------------------------------------------------

-- up(c) Function defines a unit propagation function that takes a partial assignment to another partial assignment.
-- It takes a list of integers (that represents a clause) as input.
-- It takes an integer 'n' - that is the vocabulary value.

up :: Int -> [Int] -> PA -> PA
up _ _ (PA Nothing)  = (PA Nothing)
up n c (PA (Just p))
        | (((length c) -1 ==paFalseCount)&&(paQuestCount ==1))
                                           = paMeet (assign n (c!!(fromJust(findIndex (==PAQuest) paValueListOfC)))) (PA (Just p))
        | otherwise                                            = (PA (Just p))
                where   paQuestCount = length (Prelude.filter (==PAQuest) paValueListOfC)
                        paFalseCount = length (Prelude.filter (==PAFalse) paValueListOfC)
                        paValueListOfC = Data.List.map (assignPAValueToL . fromIntegral) c
                        assignPAValueToL l
                                | (l > n || l < -n) = error "Literal value crossed n"
                                | (l > 0) = p !! (l-1)
                                | (l == 0) = error "[up:] Literal Value cannot be '0'"
                                | otherwise = invertPAValue (p !! (abs(l)-1))
                        invertPAValue v
                                | (v == PAFalse) = PATrue
                                | (v == PATrue) = PAFalse
                                | otherwise = v

------------------------------------------------------------------------------------------------------------

-- gfpUP Function - Greatest Fixed Point of applying up(c) for each clause in the encoding.
-- It takes the vocabulary value 'n', a set of clauses (List of list of integers) and a partial assignment as the input.
-- It outputs the GFP of unit propagation as mentioned in the paper.

gfpUP :: Int -> [[Int]] -> PA -> PA
gfpUP n setC p = greatestFP p
        where greatestFP q
                | (q == (bigPAMeet setC q)) = q
                | otherwise = greatestFP (bigPAMeet setC q)
              bigPAMeet [] q = q
              bigPAMeet (c:rest) q = bigPAMeet rest (up n c q)

------------------------------------------------------------------------------------------------------------
-- Grow Function
-- It takes the original assignment, eRef and the unsatisfying clause set mu as the input.
-- It outputs the grown assignment that acts as the MSS

grow :: [Int] -> [[Int]] -> [Int] -> [Int]
grow seed eRef mu
        | (cond seedPrime) = if (cond seed) then error "Error: How come this seed?" else seed 
        | otherwise      = grow seedPrime eRef mu
        where   cond []  = True
                cond var = not $isSolution $unsafePerformIO $Picosat.solve $eRefApplied var
                eRefApplied var = if (check var >0) then [[1],[-1]] else map (applyMuClause var) eRef
                check var       = length $Prelude.filter (==[]) $map (applyMuClause var) eRef
                seedPrime  	= if (seedPrimeList == []) then [] else (head seedPrimeList)
                seedPrimeList   = Prelude.filter (not.cond) $Prelude.filter (/=seed) (map addMore mu)
                addMore l       = Data.List.union seed [l]

-- Shrink Function 
-- It takes the original assignment and Eref as input
-- It outputs the shrunk assignment that acts as the MUS

shrink :: [Int] -> [[Int]] -> [Int]
shrink mu eRef
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
	| (not cond) = if baseCaseCheck then muses else baseCase
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
		baseCase	= Data.List.union muses [shrink mu eRef]
		baseCaseCheck	= isSolution $unsafePerformIO $Picosat.solve $eRefApplied mu

-- mus Function
-- It takes mu (list of integers), eRef (in CNF form) and list of interested variables (list of integers) as the input.
-- It gives mus(mu) (list of integers)  as the output.
-- List of Integers representation of mu - 
-- Example - [-1,2,-4] means (1,4) are assigned False and (3) is not assigned or is question and (2) is assigned True.

musfun :: [Int] -> [[Int]] -> [Int] -> [[Int]]
musfun mu eRef intLst = marco [] muNew eRef []
	where 	muNew = Prelude.filter (/=0) $map onlyInterested mu
		onlyInterested l
			| (isJust $findIndex (==abs(l)) intLst) = l
			| otherwise				= 0 

------------------------------------------------------------------------------------------------------------

-- In this code, we are exploring the following algorithm to estimate the Propagation Complete Encodings for SAT Solvers.
{-      phi = negation(eRef)
        while phi is Satisfiable
                Let mu be the total assignment that satisfies phi
                mu'     = MUS(mu) w.r.t eRef
                E       = and E negation(mu')
                phi     = and phi negation(mu')
-}
-- The above will be done using the Picosat SAT Solver as the pce function (for the main algorithm)
-- And mus function for finding  Minimal Unsatisfiable Subsets is done by taking reference of the paper -
-- "Enumerating Infeasibility: Finding Multiple MUSes Quickly"

{-This method does NOT work, as by marking off any MUS, we might lose to explore some other MUS that always has this MUS as a part of all its solution. Hence, we try an alternate algorithm as given below.
-}
------------------------------------------------------------------------------------------------------------

-- pceWrong Function (Does NOT compute correct answer)
-- It takes List of Interested variables (list of integers).
-- It takes List of list of integers as input that represent the CNF of eRef
-- It takes List of list of integers as input that represent the CNF of phi (as given in algorithm)
-- It outputs List of list of integers that represent the CNF of the Propagation complete form of eRef

pceWrong :: [Int] -> [[Int]] -> [[Int]] -> [[Int]]
pceWrong intLst eRef dnfPhi = Prelude.filter (/=[]) z
	where 	z      = pceHelper intLst eRef (dnfToCNF n dnfPhi) []
		n      = maximum (map maximum phiAbs)
		phiAbs = map (map abs) dnfPhi 

pceHelper :: [Int] -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]]
pceHelper [] _ _ _  = []
pceHelper _ _ []  _ = []
pceHelper intLst eRef phi e
	| (not z)   = e
	| otherwise = pceHelper intLst eRef phiNew eNew
	where 	phiNew 	   = if muPrimeNeg ==[] then Data.List.union phi muNeg else Data.List.union phi muPrimeNeg
		eNew	   = Data.List.union e muPrimeNeg
		z 	   = isSolution muSol
		muSol 	   = unsafePerformIO $Picosat.solve phi 
		muNeg	   = [map (* (-1)) muIntLst]
		muPrime    = musfun mu eRef intLst
		muPrimeNeg = map (map (* (-1))) muPrime
		mu	   = fromSolution muSol
		muIntLst   = Prelude.filter (/=0) $map onlyInterested mu
                onlyInterested l  
                        | (isJust $findIndex (==abs(l)) intLst) = l
                        | otherwise                             = 0

------------------------------------------------------------------------------------------------------------
{- Here, we try an alternate algorithm whose rough logic is as follows

	phi : unexplored partial assignments 
	phiInitial = ((-xTrue Or -xFalse) | x in interestedVariables) ++ (Or (xTrue Or xFalse) for all x in interestedVariables)
	Find mu - a Partial Assignment satisfying phi
	check whether (Eref and Mu) is SAT
		If UNSAT
			Shrink mu to get MUS
			phiPrime = Phi and (Or (-xV | xV is given in mu))
		Else
			muSAT = solution (Eref and Mu)
			muS   = MSS(muSAT)
			phiPrime = Phi and (MCS)
-}
-- pce Function 
-- It takes List of Interested variables (list of integers).
-- It takes List of list of integers as input that represent the CNF of eRef
-- It takes List of list of integers as input that represent the CNF of phi (as given in algorithm)
-- It outputs List of list of integers that represent the CNF of the Propagation complete form of eRefa

pce :: [Int] -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]]
pce intLst eRef phi e
        | (not cond) 	= e
        | otherwise 	= pce intLst eRef phiNew eNew
	where	phiNew	= if isSAT then (Data.List.union phi blockSAT) else (Data.List.union phi blockUnSAT)
		eNew	= if isSAT then e else (Data.List.union e musNeg)
		musNeg	= [map (* (-1)) musUnSat]
		blockUnSAT   = [map negUnSat musUnSat]
		blockSAT     = [map negSat musSat]
		musUnSat     = shrink muFormatted eRef
		musSat	     = shrink (Prelude.filter ((isIn intLst).abs) muSAT) (dnfToCNF m $map (map (* (-1))) eRef) 
		isSAT 	= isSolution $unsafePerformIO $Picosat.solve (Data.List.union eRef $map (\x->[x]) muFormatted)
		muSAT	= fromSolution $unsafePerformIO $Picosat.solve (Data.List.union eRef $map (\x->[x]) muFormatted)
		muFormatted     = Prelude.filter (/=0) $zipWith assign mu1 mu2
		mu    	= fromSolution muSol
		cond  	= isSolution muSol
		muSol 	= unsafePerformIO $Picosat.solve phi
		(mu1, mu2)	= splitAt ((length mu) `div` 2) mu
		assign	p q
			| (p<0 && q<0)	= 0
			| (p<0)	       	= -(intLst!!(abs(p)-1))
			| (q<0)	      	= (intLst!!(p-1))
			| otherwise   	= error "Error: xTrue and xFalse simultaneously true" 
		negSat l
                        |(l<0)          = (fromJust $findIndex (==abs(l)) intLst) + 1
                        | otherwise     = (n+(fromJust $findIndex (==abs(l)) intLst) + 1)

		negUnSat l
                 	|(l<0)      	= -(n+(fromJust $findIndex (==abs(l)) intLst) + 1)
                 	| otherwise 	= -((fromJust $findIndex (==abs(l)) intLst) + 1)
		n		= length intLst
		m      		= maximum (map maximum eRefAbs)
                eRefAbs 	= map (map abs) eRef


------------------------------------------------------------------------------------------------------------

-- redundancyRemover Function removes the redundant clauses from the given encoding.
-- Note that, this function guarentees only a minimal encoding as the output and not the absolute least (as the outcome will depend on the processing order).
-- It takes an encoding and returns the encoding after removing redundant clauses, if any.

redundancyRemover :: [[Int]] -> [[Int]]
redundancyRemover e = redundancyRemoverHelper 0 (length e) n e
	where	n    = maximum (map maximum eAbs)
                eAbs = map (map abs) e

redundancyRemoverHelper :: Int -> Int -> Int -> [[Int]] -> [[Int]]
redundancyRemoverHelper _ _ _ [] = []		
redundancyRemoverHelper l m n e
	| (l<m)     = if ch then x else y
	| otherwise = e
		where 	ch   = checkClause n rest c
			x    = redundancyRemoverHelper l (m-1) n rest
			y    = redundancyRemoverHelper (l+1) m n e
			c    = e!!l
			rest = Prelude.filter (/= c) e
 
------------------------------------------------------------------------------------------------------------

-- checkClause Function checks whether the given clause is redundant or not w.r.t the rest of the encoding.
-- The function takes the value 'n', the rest of the encoding and the clause as input and outputs True or False as desired.

checkClause :: Int -> [[Int]] -> [Int] -> Bool
checkClause _ e [] = True
checkClause n e c  = length(Prelude.filter (==True) $zipWith (==) oldPAList newPAList) == 0
        where   newPAList = Data.List.map (fromJust . unPA . (gfpUP n e)) (Data.List.map makePA oldPAList)
                oldPAList = Data.List.map markLitQuest c
                markLitQuest x     = Data.List.map (matchLit x) [1..n]
                matchLit x l
                        | (abs(x) == l) = PAQuest
                        | (index l == Nothing)= PAQuest
                        | (c!!(fromJust(index l)) > 0)  = PAFalse
                        | (c!!(fromJust(index l)) < 0)  = PATrue
                        | otherwise = error "Error, 0 appeared"
                index l = getOrIndex (findIndex (==l) c) (findIndex (==(-l)) c)
                getOrIndex Nothing Nothing   = Nothing
                getOrIndex Nothing (Just a)  = Just a
                getOrIndex (Just a) Nothing  = Just a
                getOrIndex (Just a) (Just b) = error "Error,Literal found in both negated and abs form"
------------------------------------------------------------------------------------------------------------

