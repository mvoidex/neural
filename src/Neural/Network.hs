module Neural.Network (
	module Neural.Types,
	activate_, activate, go
	) where

import Prelude.Unicode

import Control.Lens
import Data.Matrix

import Neural.Types

activate_ ∷ Num a ⇒ Network a → Matrix a → Matrix a
activate_ n xs = activations (activate n xs ^?! values . _last)

activate ∷ Num a ⇒ Network a → Matrix a → Activation a
activate n xs = Activation $ scanl step xs' (n ^. layers) where
	xs' = fmap (\x → (x, x * (1 - x))) xs
	step as l = fmap eval zs where
		zs = (l ^. weights) × fmap fst as + (l ^. biases)
		eval v = ((l ^. neuron . activation) v, (l ^. neuron . activation') v)

go ∷ Num a ⇒ Network a → [a] → [a]
go n = toList ∘ activate_ n ∘ column
