## Module Data.Functor.Pairing.Co

Monads from comonads,
based on <https://hackage.haskell.org/package/kan-extensions-5.0.1/docs/Control-Monad-Co.html>.

#### `Co`

``` purescript
newtype Co w a
```

##### Instances
``` purescript
(Functor w) => Functor (Co w)
(Extend w) => Apply (Co w)
(Comonad w) => Applicative (Co w)
(Extend w) => Bind (Co w)
(Comonad w) => Monad (Co w)
(ComonadAsk e w) => MonadAsk e (Co w)
(ComonadEnv e w) => MonadReader e (Co w)
(ComonadStore s w) => MonadState s (Co w)
(ComonadTraced t w) => MonadTell t (Co w)
(Functor f, ComonadCofree f w) => MonadFree (Co f) (Co w)
```

#### `co`

``` purescript
co :: forall w a. (forall r. w (a -> r) -> r) -> Co w a
```

#### `runCo`

``` purescript
runCo :: forall w a r. Co w a -> w (a -> r) -> r
```

#### `pairCo`

``` purescript
pairCo :: forall w. Functor w => w ⋈ (Co w)
```

`w` pairs with `Co w` whenever `w` is a `Functor`.

#### `liftCo`

``` purescript
liftCo :: forall w s. Comonad w => (forall a. w a -> s) -> Co w s
```

#### `lowerCo`

``` purescript
lowerCo :: forall w a s. Functor w => Co w s -> w a -> s
```


