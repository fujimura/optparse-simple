{-# LANGUAGE OverloadedStrings #-}

module Options.Applicative.SimpleSpec (main, spec) where

import Options.Applicative.Simple hiding (action)
import System.Environment (withArgs)

import Helper
import Test.Hspec

simpleProg :: IO ()
simpleProg = do
  ((), ()) <- simpleOptions "version" "header" "desc" (pure ()) empty
  return ()

parserWithSummary :: Parser ()
parserWithSummary = summaryOption <*> pure () where
  summaryOption = infoOption "A program summary"
    $ long "summary"
   <> help "Show program summary"

summaryProg :: IO ()
summaryProg = do
  ((), ()) <- simpleOptions "version" "header" "desc" parserWithSummary empty
  return ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "simpleOptions" $ do
    it "should work well with infoOption" $ do
      (out,_,_) <- withStdIn ""
        $ withArgs ["--version"]
        $ simpleProg
      out `shouldBe` "version\n"

      (out',_,_) <- withStdIn ""
        $ withArgs ["--summary"]
        $ summaryProg
      out' `shouldBe` "A program summary\n"
