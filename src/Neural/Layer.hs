module Neural.Layer (
	module Neural.Types,
	layer, net
	) where

import Control.Monad.State
import Data.Matrix
import System.Random (StdGen, Random, randomR, mkStdGen)

import Neural.Types

data LayerDef a = LayerDef {
	layerSize ∷ Int,
	layerNeuron ∷ Neuron a }

layer ∷ Int → Neuron a → LayerDef a
layer = LayerDef

net ∷ (Random a, Floating a) ⇒ Int → Int → [LayerDef a] → Network a
net gen l ls = Network $ evalState (zipWithM toLayer (l : map layerSize ls) ls) (mkStdGen gen) where
	toLayer ∷ (Random a, Floating a) ⇒ Int → LayerDef a → State StdGen (Layer a)
	toLayer l' layer' = Layer <$>
		randM (layerSize layer') l' <*>
		randM (layerSize layer') 1 <*>
		pure (layerNeuron layer')
	rand ∷ (Random a, Floating a) ⇒ State StdGen a
	rand = state $ randomR (-1.0, 1.0)
	rands ∷ (Random a, Floating a) ⇒ Int → State StdGen [a]
	rands n = replicateM n rand
	randM ∷ (Random a, Floating a) ⇒ Int → Int → State StdGen (Matrix a)
	randM rows cols = fromList rows cols <$> rands (rows * cols)
