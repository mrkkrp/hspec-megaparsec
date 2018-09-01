{-# LANGUAGE CPP #-}

module Main (main) where

import Data.Semigroup ((<>))
import Data.Void
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*))
#endif

type Parser = Parsec Void String

-- | Toy tests, just an example of usage.

main :: IO ()
main = hspec $ do
  describe "shouldParse" $
    it "works" $
      parse (letterChar :: Parser Char) "" "x" `shouldParse` 'x'
  describe "parseSatisfies" $
    it "works" $
      parse (many punctuationChar :: Parser String) "" "?!!"
        `parseSatisfies` ((== 3) . length)
  describe "shouldFailOn" $
    it "works" $
      parse (char 'x' :: Parser Char) "" `shouldFailOn` "a"
  describe "shouldSucceedOn" $
    it "works" $
      parse (char 'x' :: Parser Char) "" `shouldSucceedOn` "x"
  describe "shouldFailWith" $
    it "works" $
      parse (char 'x' :: Parser Char) "" "b" `shouldFailWith`
        err 0 (utok 'b' <> etok 'x')
  describe "failsLeaving" $
    it "works" $
      runParser' (many (char 'x') <* eof :: Parser String) (initialState "xxa")
        `failsLeaving` "a"
  describe "succeedsLeaving" $
    it "works" $
      runParser' (many (char 'x') :: Parser String) (initialState "xxa")
        `succeedsLeaving` "a"
