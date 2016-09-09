{-# LANGUAGE TemplateHaskell #-}

module TestRels where

import Control.Lens hiding (element)
import Test.HUnit

test1 = TestCase (assertEqual "for (foo 3)," (1,2) (foo 3))

main :: IO ()
main = putStrLn "Test suite not yet implemented"