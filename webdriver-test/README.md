Webdriver-test
==============

Simple test of [webdriver](https://hackage.haskell.org/package/webdriver) on
client-side generated content: [TodoMVC vuejs version](http://todomvc.com/examples/vue).


How to run
----------

Install

* stack
* phantomjs

Then do:

    stack build && stack exec webdriver-test


Explanation
-----------

The example is self contained and launches an instance of phantomjs at the start
which is then killed automatically.

### How the webriver api is constructed

All the page manipulation logic runs into the `WD` monad which is analogous to an `IO`
monad with an embedded state `WDSession`:

```haskell
newtype WD a = WD (StateT WDSession IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadFix)
```

`StateT` is a monad transformer: it takes a monad (here `IO`) and wraps with
some special structure to create another one.

Here, the special structure is a state, so `StateT WDSession IO a` behaves mostly like
`State WDSession` and calling `return`, `>>=` (the monad operations) operates on the
state.

When using a tranformed monad, the inner monad (`IO`) is accessible through the
`lift` operator:

```haskell
lift :: (MonadTrans t, Monad m) => m a -> t m a
```

Here `m` is `IO`, `t` is `StateT WDSession`.

So it is possible to interleave operations on the `WDSession` state with `IO`.
Furthermore, `WD` is an instance of `MonadIO` which is a more generic mechanism
to embed `IO` operation in a monad:

```haskell
liftIO :: MonadIO m => IO a -> m a
```

The signature is similar to `lift` except that there is no mention of monad
transformers.

It's better to use `liftIO` in this case since it does not presume about the
internal structure of `WD`.

At the end, our program should be of type `IO ()`, so we need to interpret all
the `WD` operations and only use its effects (the internal state will be seen as
a black box).

It is done with `runSession` which also needs a configuration:

```haskell
runSession :: WebDriverConfig conf => conf -> WD a -> IO a
```
