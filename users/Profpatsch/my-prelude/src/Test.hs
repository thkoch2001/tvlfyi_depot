{-# LANGUAGE LambdaCase #-}

{- Generate Test suites.

Restricted version of hspec, introduction: http://hspec.github.io/getting-started.html
-}
module Test
  ( Spec,
    runTest,
    testMain,

    -- * Structure
    describe,
    it,

    -- * Expectations
    Expectation,
    testOk,
    testErr,
    shouldBe,
    shouldNotBe,
    shouldSatisfy,
    shouldNotSatisfy,

    -- * Setup & Teardown (hooks http://hspec.github.io/writing-specs.html#using-hooks)
    before,
    before_,
    beforeWith,
    beforeAll,
    beforeAll_,
    beforeAllWith,
    after,
    after_,
    afterAll,
    afterAll_,
    around,
    around_,
    aroundWith,
    aroundAll,
    aroundAllWith,

    -- * Common helpful predicates (use with 'shouldSatisfy')
    isRight,
    isLeft,

    -- * Pretty printing of errors
    errColored,
    module Pretty,
  )
where

-- export more expectations if needed

import Data.Either
  ( isLeft,
    isRight,
  )
import Pretty
import Test.Hspec
  ( Expectation,
    HasCallStack,
    Spec,
    after,
    afterAll,
    afterAll_,
    after_,
    around,
    aroundAll,
    aroundAllWith,
    aroundWith,
    around_,
    before,
    beforeAll,
    beforeAllWith,
    beforeAll_,
    beforeWith,
    before_,
    describe,
    hspec,
    it,
  )
import Test.Hspec.Expectations.Pretty
  ( expectationFailure,
    shouldBe,
    shouldNotBe,
    shouldNotSatisfy,
    shouldSatisfy,
  )

-- | Run a test directly (e.g. from the repl)
runTest :: Spec -> IO ()
runTest = hspec

-- | Run a testsuite
testMain ::
  -- | Name of the test suite
  String ->
  -- | The tests in this test module
  Spec ->
  IO ()
testMain testSuiteName tests = hspec $ describe testSuiteName tests

-- | test successful
testOk :: Expectation
testOk = pure ()

-- | Abort the test with an error message.
-- If you want to display a Haskell type, use `errColored`.
testErr :: HasCallStack => String -> Expectation
testErr = expectationFailure

-- | Display a list of 'Err's as a colored error message
-- and abort the test.
errColored :: [Pretty.Err] -> Expectation
errColored = testErr . Pretty.prettyErrs
