# Validated

A custom type for data validation

```haskell
import Data.Validated

validate :: Int -> Validated String Int
validate n = if n > 18 then valid n else invalid "Must be over 18"
```
