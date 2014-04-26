lens-inference
==============

Infer lenses.

With my definition of an Inu:

```hs
newtype Breed = Breed { unBreed :: String }
  deriving Show

data Colour = White | Red | Sesame
  deriving Show

newtype Age = Age { unAge :: Int }
  deriving (Show, Num)

data Inu = Inu { _breed :: Breed, _colour :: Colour, _age :: Age }
  deriving Show
makeInferableLenses ''Inu
```

I use `makeInferableLenses ''Inu` instead of `makeLenses ''Inu` and get
lenses I can do value inference on.

For example, I have a function to increase an `Age` by 1:

```hs
birthday :: Age -> Age
birthday (Age x) = Age (x + 1)
```

I can use lens to write a function that increments a dog's age by one:

```hs
inu_birthday :: Inu -> Inu
inu_birthday = age %~ birthday
```

But since a birthday applies to an `Age`, the only possible lens we can use
is `age`. So instead, we infer it.

```hs
inu_birthday :: Inu -> Inu
inu_birthday = inferLens %~ birthday
```

Or, we can use the special modify operator `%~?`:

```hs
inu_birthday :: Inu -> Inu
inu_birthday = (%~?) birthday
```

Next we can define birds:

```hs
data Inko = Inko { _inkoAge :: Age }
  deriving Show
makeInferableLenses ''Inko
```

And a function that makes anything with an `Age` older:

```hs
older :: IsInferable a Age Identity => a -> a
older x = birthday %~? x
```
