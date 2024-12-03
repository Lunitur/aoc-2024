import Text.Parsec
import Control.Monad (guard, void)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (catMaybes)
import Data.Bool (bool)

example = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"


corruption = skipMany $ notFollowedBy mul >> anyChar

muls = many $ do
    corruption
    m <- mul
    corruption
    return m

mul :: Parsec String a (Int,Int)
mul = try $ do
  string "mul("
  i1 <- many1 digit
  char ','
  i2 <- many1 digit
  char ')'
  guard (length i1 <= 3 && length i2 <= 3)
  return (read i1 :: Int, read i2 :: Int)

part1 input = case runParser muls () "" input of
    Right res -> uncurry (*) <$> res & sum
    Left _ -> error "parser error"

doInstruction = string' "do()" >> putState True 
dontInstruction = string' "don't()" >> putState False


corruption' = skipMany $ notFollowedBy mul >> (doInstruction <|> dontInstruction <|> void anyChar)
muls' :: Parsec String Bool [Maybe (Int,Int)]
muls' = many $ do
    corruption'
    m <- mul
    shouldParse <- getState
    corruption'
    return $ bool Nothing (Just m) shouldParse

part2 input = case runParser muls' True "" input of
    Right res -> uncurry (*) <$> catMaybes res & sum
    Left _ -> error "parser error"
