#!stack
{- stack runghc
    --package raw-strings-qq
-}

{-# LANGUAGE QuasiQuotes #-}          -- Required for raw strings library
{-# LANGUAGE DeriveDataTypeable #-}   -- Required for implementing typeof

module Data.Json (Json (..), typeof, parseJson, showJson) where

import Control.Applicative ((<|>))
import Control.Monad (replicateM)
import Data.Data (Data, Typeable, toConstr)
import Data.Char (toLower)
import Data.Either (fromRight)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Parsec (ParseError, parse, eof, unexpected,
                    between, many, many1, optionMaybe, sepEndBy)
import Text.Parsec.Char (anyChar, char, digit, hexDigit, noneOf,
                         spaces, string)
import Text.Parsec.String (Parser)
import Text.Printf (printf)
import Text.RawString.QQ

-- |Type to represent JSON values
-- All constructor names are the same as the primitive JavaScript constructors,
-- but with an extra "J" at the start. The only exception to this rule is
-- "JNull" - because null (in JS) doesn't have any constructor function.
data Json = JString  String |
            JNumber  Double |
            JBoolean Bool   |
            JNull           |
            JArray   [Json] |
            JObject  (Map String Json)
     deriving (Eq, Show, Data, Typeable)

-- Equivalent to JavaScript's typeof operator
-- Returns the JavaScript primitive type of the JSON object
typeof :: Json -> String
typeof json = toLower fstChar : rest
  where ('J' : fstChar : rest) = show $ toConstr json

-- TODO: automatic testing
-- TODO: better error messages
-- TODO: dynamic indexing like JavaScript : obj["a"][0] (Is it possible??)

-- a single "token" that may be surrounded by spaces
lexeme :: Parser a -> Parser a
lexeme parser = parser <* spaces

lexChar :: Char -> Parser Char
lexChar = lexeme . char

specialChar :: Parser Char
specialChar = do
  char '\\'
  c <- anyChar
  case c of
    '"'  -> pure '"'
    '\\' -> pure '\\'
    '/'  -> pure '/'
    'b'  -> pure '\b'
    'f'  -> pure '\f'
    'n'  -> pure '\n'
    'r'  -> pure '\r'
    't'  -> pure '\t'
    'u'  -> read . (printf [r|'\x%s'|]) <$> replicateM 4 hexDigit   -- Unicode Character
    _    -> fail $ printf [r|'\%c' is not a valid escape sequence|]

str :: Parser String
str = lexeme $ between quote quote $ many (specialChar <|> noneOf "\"")
  where quote = char '"'

num :: Parser Double
num = lexeme $ (do
        char '-'
        ans <- decimal
        pure (-ans)) <|> decimal

  where nat = many1 digit
        decimal = do
          int <- nat
          frac <- fromMaybe "0" <$> optionMaybe (char '.' *> nat)
          pure $ read $ int ++ "." ++ frac

bool :: Parser Bool
bool = lexeme $
       (pure True <*  string "true") <|>
       (pure False <* string "false")

listOf :: Char -> Char -> Parser a -> Parser [a]
listOf start end parser =
  between (lexChar start) (lexChar end) $
  parser `sepEndBy` (lexChar ',')

array :: Parser [Json]
array = lexeme $ listOf '[' ']' term

object :: Parser (Map String Json)
object = lexeme $ Map.fromList <$> listOf '{' '}' keyValuePair
  where keyValuePair = do
          key <- str
          lexChar ':'
          value <- term
          pure (key, value)

term :: Parser Json
term = (JString  <$> str)   <|>
       (JNumber  <$> num)   <|>
       (JBoolean <$> bool)  <|>
       (pure JNull <* lexeme (string "null")) <|>
       (JArray   <$> array) <|>
       (JObject  <$> object)

json :: Parser Json
json = spaces *> term <* eof

parseJson :: String -> Either ParseError Json
parseJson = parse json ""

-- TODO: display "pretty" JSON (with formatting, indentation)
showJson :: Json -> String
showJson (JString  s)  = show s  -- TODO: Display Unicode Characters properly
showJson (JNumber  n)  = show n
showJson (JBoolean b)  = if b then "true" else "false"
showJson JNull         = "null"
showJson (JArray arr)  = "[" ++ intercalate "," (showJson <$> arr) ++ "]"
showJson (JObject obj) = "{" ++
                         intercalate "," (keyValuePair <$> Map.toList obj) ++
                         "}"
  where keyValuePair (k,v) = show k ++ ":" ++ showJson v
