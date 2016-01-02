{-# LANGUAGE OverloadedStrings #-}
module ParserCombinators (tests) where

import Test.Hspec (Spec, describe, it)
import Test.HUnit (assertBool, assertEqual, Assertion)
import qualified Data.Attoparsec.Text as P
import Data.Text (Text)

tests :: Spec
tests = describe "ParserCombinators" $ do
    testDigitParser
    testDigitsParser
    testSymbolParser
    testAtomParser
    testListParser

failParser :: String -> P.Parser a
failParser parserName =
  fail $ "\n\n\t\x1B[32;1mCheck documentation\x1B[0m of \x1B[33;1m"
         ++ parserName
         ++ "\x1B[0m on:\n\t"
         ++ "http://hackage.haskell.org/packages/archive/attoparsec/latest/doc/html/Data-Attoparsec-Text.html"
         ++ "\n\tand http://hackage.haskell.org/packages/archive/attoparsec/latest/doc/html/Data-Attoparsec-Combinator.html"

assertParse :: (Eq a, Show a) => a -> Either String a -> Assertion
assertParse expected (Right answer) =
  assertEqual "wrong parser" expected answer

testDigitParser :: Spec
testDigitParser = it "digit parser" $ do
    -- Change parser with the correct parser to use
    let parser = P.digit
    let result = P.parseOnly parser "5"
    assertParse '5' result

testDigitsParser :: Spec
testDigitsParser = it "sequence of digits parser" $ do
    -- Change parser with the correct parser to use
    -- let parser = P.takeWhile isDigit where isDigit c = c >= '0' && c <= '9'
    let parser = P.decimal
    let result = P.parseOnly parser "54321"
    assertParse 54321 result

testSymbolParser :: Spec
testSymbolParser = it "symbol parser" $ do
    -- Change parser with the correct parser to use
    --
    -- Here we say symbol is a sequence of characters that doesn't have
    -- parenthes or spaces.
    let parser = P.takeWhile $ P.notInClass "() "
    assertParse "ab" $ P.parseOnly parser "ab"
    assertParse "a/b" $ P.parseOnly parser "a/b"
    assertParse "a/b" $ P.parseOnly parser "a/b c"

data Atom = AInt Int | ASym Text deriving (Eq, Show)

parseAInt :: P.Parser Atom
parseAInt = do
  d <- P.decimal
  return $ AInt d

parseASym :: P.Parser Atom
parseASym = do
  s <- P.takeWhile1 $ P.notInClass "() "
  return $ ASym s 

testAtomParser :: Spec
testAtomParser = it "atom parser" $ do
    -- Change parser with the correct parser to use
    --
    let parser = P.choice [parseAInt, parseASym]
    assertParse (ASym "ab") $ P.parseOnly parser "ab"
    assertParse (ASym "a/b") $ P.parseOnly parser "a/b"
    assertParse (ASym "a/b") $ P.parseOnly parser "a/b c"
    assertParse (AInt 54321) $ P.parseOnly parser "54321"

data List = Nil | LAtom Atom | Cons List List deriving (Eq, Show)

parseList :: P.Parser List
parseList = do
  P.char '('
  retVal <- P.choice [
    Cons <$> token <*> secondToken
    , return Nil
    ]
  P.char ')'
  return retVal
  where
    token = P.choice [parseList, LAtom <$> parseAInt, LAtom <$> parseASym]
    secondToken = P.choice [
        do
          P.char ' '
          token 
      , return Nil
      ]

testListParser :: Spec
testListParser = it "list parser" $ do
    -- Change parser with the correct parser to use
    let parser = parseList :: P.Parser List
    assertParse Nil $ P.parseOnly parser "()"
    assertParse (Cons (LAtom (AInt 12)) Nil) $ P.parseOnly parser "(12)"
    assertParse (Cons (LAtom (ASym "a")) (Cons (LAtom (ASym "b")) Nil)) $ P.parseOnly parser "(a (b))"
