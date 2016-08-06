-- | Monads from comonads,
-- | based on <https://hackage.haskell.org/package/kan-extensions-5.0.1/docs/Control-Monad-Co.html>.

module Data.Functor.Pairing.Co
  ( Co
  , runCo
  , liftCo
  , lowerCo
  ) where

import Prelude
import Control.Comonad (class Comonad, extract)
import Control.Comonad.Env.Class (class ComonadEnv, ask, local)
import Control.Comonad.Store.Class (class ComonadStore, peek, pos)
import Control.Comonad.Traced.Class (class ComonadTraced, track)
import Control.Extend (class Extend, (=>>))
import Control.Monad.Reader.Class (class MonadReader)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Writer.Class (class MonadWriter)
import Data.Functor.Pairing (type (⋈))
import Data.Identity (runIdentity)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)

data Co w a = Co (forall r. w (a -> r) -> r)

co :: forall w a. (forall r. w (a -> r) -> r) -> Co w a
co = Co

runCo :: forall w a r. Co w a -> w (a -> r) -> r
runCo (Co cow) = cow

-- | `w` pairs with `Co w` whenever `w` is a `Functor`.
pairCo :: forall w. Functor w => w ⋈ Co w
pairCo f w cow = runCo cow (map f w)

liftCo :: forall w s. Comonad w => (forall a. w a -> s) -> Co w s
liftCo f = Co (extract <*> f)

lowerCo :: forall w a s. Functor w => Co w s -> w a -> s
lowerCo m = runIdentity <<< runCo m <<< (pure <$ _)

instance functorCo :: Functor w => Functor (Co w) where
  map f (Co cow) = Co \w -> cow (map (_ <<< f) w)

instance applyCo :: Extend w => Apply (Co w) where
  apply (Co f) (Co a) = Co \w -> f (w =>> \wf g -> a (map (_ <<< g) wf))

instance applicativeCo :: Comonad w => Applicative (Co w) where
  pure a = Co \w -> extract w a

instance bindCo :: Extend w => Bind (Co w) where
  bind (Co k) f = Co \w -> k (w =>> \wa a -> runCo (f a) wa)

instance monadCo :: Comonad w => Monad (Co w)

instance monadReaderCo :: ComonadEnv e w => MonadReader e (Co w) where
  ask = liftCo (ask :: forall a. w a -> e)
  local f (Co x) = Co (x <<< local f)

instance monadStateCo :: ComonadStore s w => MonadState s (Co w) where
  state f = do
    s <- liftCo pos
    case f s of
      Tuple a s1 -> liftCo (peek s1 $> a)

instance monadWriterCo :: ComonadTraced t w => MonadWriter t (Co w) where
  writer (Tuple a t) = liftCo (track t $> a)
  listen _ = unsafeCrashWith "monadWriterCo: listen not implemented"
  pass _ = unsafeCrashWith "monadWriterCo: pass not implemented"
