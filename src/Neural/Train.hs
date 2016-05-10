{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Neural.Train (
	TrainT(..), Train,
	trainM, train,
	runM, run,
	Learn(..),
	Course(..), course,
	study, blocks
	) where

import Prelude.Unicode
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Data.List (permutations, unfoldr)
import Data.Matrix

import Neural.Network

newtype TrainT a m r = TrainT { runTrainT ∷ ReaderT a (StateT (Network a) m) r }
	deriving (Functor, Applicative, Monad, MonadReader a, MonadState (Network a))

instance MonadTrans (TrainT a) where
	lift = TrainT ∘ lift ∘ lift

type Train a r = TrainT a Identity r

trainM ∷ Monad m ⇒ a → Network a → TrainT a m r → m (Network a)
trainM rate n act = execStateT (runReaderT (runTrainT act) rate) n

train ∷ a → Network a → Train a r → Network a
train rate n = runIdentity ∘ trainM rate n

runM ∷ Monad m ⇒ a → Network a → TrainT a m r → m r
runM rate n act = evalStateT (runReaderT (runTrainT act) rate) n

run ∷ a → Network a → Train a r → r
run rate n = runIdentity ∘ runM rate n

class Learn s where
	learn ∷ Fractional a ⇒ s a → Train a ()
	check ∷ Fractional a ⇒ s a → Train a a

instance Learn Lesson where
	learn (Lesson xs ys) = ask >>= modify ∘ learn' where
		learn' r n = Network layers' where
			layers' = zipWith3 update (tail $ ess ++ [es]) (map activations $ as ^. values) (n ^. layers) where
				update e' a' =
					over biases (subtract $ scaleMatrix r e') ∘
					over weights (subtract (scaleMatrix r $ e' × transpose a'))
			ess = scanr back es $ zip (n ^.. layers . each . weights) (as ^. values) where
				back (ws', as') es' = (transpose ws' × es') ⊙ activations' as'
			es = (activations ys' - ys) ⊙ activations' ys'
			ys' = as ^?! values . _last
			as = activate n xs
	check (Lesson xs ys) = check' <$> get where
		check' n = err where
			err = (transpose es × es) ! (1, 1)
			es = transpose (activations ys' - ys) ⊙ activations ys'
			ys' = as ^?! values . _last
			as = activate n xs

instance Learn Lessons where
	learn = mapM_ learn ∘ getLessons
	check = fmap avg ∘ mapM check ∘ getLessons

data Course a = Course {
	task ∷ a,
	rest ∷ Course a }

instance Functor Course where
	fmap f (Course l ls) = Course (f l) (fmap f ls)

instance Applicative Course where
	pure x = Course x $ pure x
	Course f fs <*> Course x xs = Course (f x) (fs <*> xs)

instance Traversable Course where
	traverse f (Course x xs) = Course <$> f x <*> traverse f xs

instance Foldable Course where
	foldMap f (Course x xs) = f x `mappend` foldMap f xs

course ∷ (s → [a]) → s → Course a
course f = foldr Course (error "empty course") ∘ f

study ∷ (Ord a, Fractional a, Learn s) ⇒ a → Int → Course (s a) → Train a ()
study _ 0 _ = return ()
study eps steps (Course l ls) = do
	err ← check l
	when (err > eps) $ do
		learn l
		study eps (pred steps) ls

blocks ∷ Int → Lessons a → [Lessons a]
blocks n = cycle ∘ map lessons ∘ concatMap split' ∘ permutations ∘ getLessons where
	split' = takeWhile ((≡ n) ∘ length) ∘ unfoldr (Just ∘ splitAt n)

avg ∷ (Fractional a, Foldable t) ⇒ t a → a
avg xs = sum xs / fromIntegral (length xs)
