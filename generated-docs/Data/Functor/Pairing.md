## Module Data.Functor.Pairing

Pairings between functors.

Based on <http://hackage.haskell.org/package/adjunctions-0.6.0/docs/Data-Functor-Zap.html>.

#### `Pairing`

``` purescript
type Pairing f g = forall a b c. (a -> b -> c) -> f a -> g b -> c
```

A pairing between functors `f` and `g`.

This asserts that any sums in `f` can annihilate any products in `g`, and vice
versa.

This library provides some useful pairings, and ways of lifting pairings over
various constructions on `Functor`s.

#### `type (⋈)`

``` purescript
infix 4 type Pairing as ype (⋈
```

#### `zap`

``` purescript
zap :: forall f g a b. f ⋈ g -> f (a -> b) -> g a -> b
```

#### `sym`

``` purescript
sym :: forall f g. f ⋈ g -> g ⋈ f
```

Pairing is symmetric

#### `identity`

``` purescript
identity :: Identity ⋈ Identity
```

The identity functor pairs with itself

#### `productCoproduct`

``` purescript
productCoproduct :: forall f1 g1 f2 g2. f1 ⋈ g1 -> f2 ⋈ g2 -> (Product f1 f2) ⋈ (Coproduct g1 g2)
```

Functor products pair with functor coproducts

#### `stateStore`

``` purescript
stateStore :: forall f g s. f ⋈ g -> (StateT s f) ⋈ (StoreT s g)
```

`StateT` pairs with `StoreT`.

#### `readerEnv`

``` purescript
readerEnv :: forall f g e. f ⋈ g -> (ReaderT e f) ⋈ (EnvT e g)
```

`ReaderT` pairs with `EnvT`.

#### `writerTraced`

``` purescript
writerTraced :: forall f g w. f ⋈ g -> (WriterT w f) ⋈ (TracedT w g)
```

`WriterT` pairs with `TracedT`.

#### `freeCofree`

``` purescript
freeCofree :: forall f g. Functor f => Functor g => f ⋈ g -> (Free f) ⋈ (Cofree g)
```

`Free` pairs with `Cofree`.


