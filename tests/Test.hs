module Main (
	n, ls, n',
	m, ms, m',
	main
	) where

import Prelude.Unicode

import Data.Matrix

import Neural

n ∷ Network Double
n = net 1000 2 [
	layer 2 sigmoidNeuron,
	layer 2 sigmoidNeuron,
	layer 1 sigmoidNeuron]

ls ∷ Lessons Double
ls = lessons [
	lesson [0.0, 0.0] [0.0],
	lesson [1.0, 0.0] [1.0],
	lesson [0.0, 1.0] [1.0],
	lesson [1.0, 1.0] [0.0]]

n' ∷ Network Double
n' = train 1.0 n $ study 1e-5 10000 $ course (blocks 4) ls

m ∷ Network Double
m = net 100 12 [
	layer 12 sigmoidNeuron,
	layer 4 sigmoidNeuron]

ms ∷ Lessons Double
ms = lessons [
	lesson' [1, 2, 3] 4,
	lesson' [2, 3, 4] 5,
	lesson' [3, 4, 5] 6,
	lesson' [5, 6, 7] 8,
	lesson' [2, 4, 6] 8,
	lesson' [1, 3, 5] 7,
	lesson' [5, 7, 9] 11,
	lesson' [2, 5, 8] 11,
	lesson' [3, 6, 9] 12]
	where
		lesson' ∷ [Int] → Int → Lesson Double
		lesson' xs x = lesson (concatMap toBin xs) (toBin x)
		toBin ∷ Int → [Double]
		toBin = map fromIntegral ∘ take 4 ∘ map (`mod` 2) ∘ iterate (`div` 2)

m' ∷ Network Double
m' = train 1.0 m $ study 1e-5 10000 $ course (blocks 4) ms

main ∷ IO ()
main = return ()
