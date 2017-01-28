# Validated

A custom type for data validation

```haskell
import Data.Validated

>> validateN :: Int -> Validated [String] Int
>> validateN n = if n > 10 then Valid n else Invalid [show n ++ " is not greater than 10"]

-- Accumulating failures

>> foldl1 (<>) $ filter isValid $ map validateN [1..20]

Invalid ["1 is not greater than 10","2 is not greater than 10","3 is not greater than 10","4 is not greater than 10","5 is not greater than 10","6 is not greater than 10","7 is not greater than 10","8 is not greater than 10","9 is not greater than 10","10 is not greater than 10"]
```
