-- |
-- This module basically just runs the mainnet epoch validation tests from
-- "Test.Bcc.Chain.Block.Validation" but provided a 'ShouldAssertNF' value of
-- 'AssertNF'.
--
-- We've created a separate test executable for this as, in our typical CI jobs,
-- we utilize @hpc@ (i.e. building with @ghc -fhpc@ or @stack --coverage@) for
-- providing code coverage. @hpc@ appears to introduce thunks around our Haskell
-- expressions for its program coverage measurement purposes which prevents us
-- from accurately determining whether a given expression is in normal form. As a
-- result, we have another CI job which will build and run this test executable
-- without @hpc@.
module Main
  ( main,
  )
where

import Bcc.Prelude
import System.IO.Silently (hSilence)
import qualified Test.Bcc.Chain.Block.Validation
import Test.Options (ShouldAssertNF (..), mainWithTestScenario, tsGroupToTree)

main :: IO ()
main =
  hSilence [stderr]
    . mainWithTestScenario
    . tsGroupToTree
    $ Test.Bcc.Chain.Block.Validation.tests AssertNF
