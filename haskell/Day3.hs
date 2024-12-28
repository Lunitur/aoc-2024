module Day3 where

import Control.Monad (guard, void)
import Data.Bool (bool)
import Data.Function ((&))
import Data.Functor.Identity
import Data.Maybe (catMaybes)
import Text.Parsec

example :: String
example = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

corruption :: Parsec String u ()
corruption = skipMany $ notFollowedBy mul >> anyChar

muls :: Parsec String u [(Int, Int)]
muls = many $ do
  corruption
  m <- mul
  corruption
  return m

mul :: Parsec String a (Int, Int)
mul = try $ do
  string "mul("
  i1 <- many1 digit
  char ','
  i2 <- many1 digit
  char ')'
  guard (length i1 <= 3 && length i2 <= 3)
  return (read i1 :: Int, read i2 :: Int)

part1 :: String -> Int
part1 input = case runParser muls () "" input of
  Right res -> uncurry (*) <$> res & sum
  Left _ -> error "parser error"

doInstruction :: ParsecT String Bool Identity ()
doInstruction = string' "do()" >> putState True

dontInstruction :: ParsecT String Bool Identity ()
dontInstruction = string' "don't()" >> putState False

corruption' :: ParsecT String Bool Identity ()
corruption' = skipMany $ notFollowedBy mul >> (doInstruction <|> dontInstruction <|> void anyChar)

muls' :: Parsec String Bool [Maybe (Int, Int)]
muls' = many $ do
  corruption'
  m <- mul
  shouldParse <- getState
  corruption'
  return $ bool Nothing (Just m) shouldParse

part2 :: String -> Int
part2 input = case runParser muls' True "" input of
  Right res -> uncurry (*) <$> catMaybes res & sum
  Left _ -> error "parser error"
