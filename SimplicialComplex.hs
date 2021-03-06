module SimplicialComplex where

import Control.Monad
import Data.List
import Matrix

type Simplex = [Int]

type SComplex = [Simplex]

compareSimplices :: Simplex -> Simplex -> Ordering
compareSimplices s t | length s == length t = compare s t
                     | otherwise = compare (length s) (length t)

-- Helper function to get the powerset
sublists :: [Int] -> [[Int]]
sublists [] = [[]]
sublists xs = filterM (const [True,False]) xs

-- Arranges a list of vertex indices into a simplex
listToSimplex :: [Int] -> Simplex
listToSimplex = sort . nub 

-- Compute the whole complex from a list of simplices
fromChain :: [Simplex] -> SComplex
fromChain = sortBy compareSimplices . nub . concatMap sublists

-- Build a simplicial complex directly from a list of lists
buildSC :: [[Int]] -> SComplex
buildSC = fromChain . map listToSimplex

-- Get the k-simplices from a simplicial complex
simplices :: Int -> SComplex -> [Simplex]
simplices k = filter ((== k + 1) . length)

-- Get a list of faces of a simplex
faces :: Simplex -> [Simplex]
faces = buildSC . sublists

-- Get the dimension of a chain by looking at the
-- number of vertices in each simplex
dim :: SComplex -> Int
dim = maximum . map ((+(-1)) . length)

-- Compute the kth boundary homomorphism of a simplex
diff :: Simplex -> [(Simplex,Int)]
diff s | length s > 0 = zip [exclude (i+1) s | i <- [0..length s-1]] (cycle [1,-1])
       | otherwise = []
       where exclude j s = take (j-1) s ++ drop j s

-- Compute the kth incidence matrix of the differentials
incidence :: Int -> SComplex -> [[Int]]
incidence k sc = matrix (simplices (k-1) sc) (simplices k sc)

-- Classical algorithm for computing the kth betti number
homology :: Int -> SComplex -> Int
homology k sc = size - cycles - bounds where
    size = length $ simplices k sc
    cycles = rank $ incidence k sc
    bounds = rank $ incidence (k+1) sc
    
-- Using the homology algorithm to create a list of all betti numbers
bettis :: SComplex -> [Int]
bettis sc = [homology k sc | k <- [0..d]]
    where d = dim sc
    
-- We also get the euler characteristic as the alternating sum
euler :: SComplex -> Int
euler sc = sum $ zipWith (*) (cycle [1,-1]) (bettis sc)
    
