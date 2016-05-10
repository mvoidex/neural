{-# LANGUAGE TemplateHaskell #-}

module Neural.Types (
	Neuron(..), activation, activation',
	Layer(..), weights, biases, neuron,
	Network(..), layers,
	Connected(..),
	Activation(..), values, activations, activations',
	listed, row, column,
	Lesson(..), lessonInput, lessonOutput, lesson,
	Lessons(..), lessons,
	(×), (⊙)
	) where

import Prelude.Unicode

import Control.Lens
import Data.Matrix

data Neuron a = Neuron {
	_activation ∷ a → a,
	_activation' ∷ a → a }

makeLenses ''Neuron

data Layer a = Layer {
	_weights ∷ Matrix a,
	_biases ∷ Matrix a,
	_neuron ∷ Neuron a }

makeLenses ''Layer

instance Show a ⇒ Show (Layer a) where
	show (Layer ws bs _) = unlines $ zipWith (++) (lines $ show ws) (lines $ show bs)

data Network a = Network {
	_layers ∷ [Layer a] }

makeLenses ''Network

instance Show a ⇒ Show (Network a) where
	show (Network ls) = concat $ zipWith showLayer ls [0..] where
		showLayer l i = header ++ show l where
			header = "----- " ++ show (i ∷ Integer) ++ " -----\n"

class Connected a where
	inputs ∷ a → Int
	outputs ∷ a → Int

instance Connected (Layer a) where
	inputs = ncols ∘ _weights
	outputs = nrows ∘ _weights

instance Connected (Network a) where
	inputs = inputs ∘ head ∘ _layers
	outputs = outputs ∘ last ∘ _layers

data Activation a = Activation {
	_values ∷ [Matrix (a, a)] }

makeLenses ''Activation

activations ∷ Matrix (a, a) → Matrix a
activations = over traverse fst

activations' ∷ Matrix (a, a) → Matrix a
activations' = over traverse snd

listed ∷ Lens' (Matrix a) [a]
listed = lens to' from' where
	to' = toList
	from' m = fromList (nrows m) (ncols m)

row ∷ [a] → Matrix a
row vs = fromList 1 (length vs) vs

column ∷ [a] → Matrix a
column vs = fromList (length vs) 1 vs

data Lesson a = Lesson {
	_lessonInput ∷ Matrix a,
	_lessonOutput ∷ Matrix a }

makeLenses ''Lesson

instance Show a ⇒ Show (Lesson a) where
	show (Lesson xs ys) = unlines $ zipWith soThat (lines $ show xs) (lines $ show ys) where
		soThat x y = x ++ " => " ++ y

lesson ∷ [a] → [a] → Lesson a
lesson xs ys = Lesson (column xs) (column ys)

newtype Lessons a = Lessons { getLessons ∷ [Lesson a] }

instance Show a ⇒ Show (Lessons a) where
	show = unlines ∘ map show ∘ getLessons 

lessons ∷ [Lesson a] → Lessons a
lessons = Lessons

(×) ∷ Num a ⇒ Matrix a → Matrix a → Matrix a
(×) = multStd

(⊙) ∷ Num a ⇒ Matrix a → Matrix a → Matrix a
(⊙) = elementwise (*)
