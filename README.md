# json-parser

This is a basic JSON parsing library in Haskell. 
I just made it for fun - don't use it for actual development - use [Aeson](https://hackage.haskell.org/package/aeson) instead.

**Note:** Availability of [Haskell Stack](https://docs.haskellstack.org/en/stable/README/) is assumed. 
Please download it first, if you don't have it already.

### Features

```json-parser.hs``` exports a ```Json``` data type for representing JSON values, as well as ```parseJson``` and ```showJson```
functions (the function names are self explanatory!)

```haskell
-- Usage Example

import Data.Either (fromRight)
import Text.RawStrings.QQ (r)

-- import this library
import Data.JSON (parseJson, showJson)

-- test json string (multi-line raw string, using QuasiQuotation)
jsonStr :: String
jsonStr = [r|
{
  "x" : 3.14,
  "y" : [
    2,
    true,
    "Using a Unicode Character: \n\uABCD"
  ],
  "z" : null
}
|]

-- Parse the above JSON string, then display it using showJson
-- Note: showJson doesn't include any whitespace in its output
main :: IO ()
main = putStrLn $ showJson $ fromRight JNull $ parseJson jsonStr
```

A ```typeof``` function is also exported, which is equivalent to the ```typeof``` operator in JavaScript.
It returns the primitive JavaScript data type corresponding to a value of type ```Json```. 

For example:

```haskell
> fromRight (Right r) = r
> typeof $ fromRight $ parseJson "null" 
"null"
> typeof $ fromRight $ parseJson "-3.14"
"number"
> typeof $ fromRight $ parseJson "[ \"hello\", \"world\", true ]"
"array"
> typeof $ fromRight $ parseJson "{\"x\": 2 }"
"object"
```

