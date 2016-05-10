module Neural.Neuron (
	sigmoidNeuron,
	sigmoid, sigmoid',
	idNeuron,

	module Neural.Types
	) where

import Control.Lens

import Neural.Types

sigmoidNeuron ∷ Floating a ⇒ Neuron a
sigmoidNeuron = Neuron sigmoid sigmoid'

sigmoid ∷ Floating a ⇒ a → a
sigmoid t = 1 / (1 + exp (negate t))

sigmoid' ∷ Floating a ⇒ a → a
sigmoid' t = sigmoid t * (1 - sigmoid t)

idNeuron ∷ Floating a ⇒ Neuron a
idNeuron = Neuron id (const 1)
