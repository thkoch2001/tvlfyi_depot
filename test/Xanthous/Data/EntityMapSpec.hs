{-# LANGUAGE ApplicativeDo #-}
--------------------------------------------------------------------------------
module Xanthous.Data.EntityMapSpec where
--------------------------------------------------------------------------------
import           Test.Prelude
--------------------------------------------------------------------------------
import qualified Data.Aeson as JSON
--------------------------------------------------------------------------------
import           Xanthous.Data.EntityMap
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain test

test :: TestTree
test = localOption (QuickCheckTests 20)
  $ testGroup "Xanthous.Data.EntityMap"
  [ testBatch $ monoid @(EntityMap Int) mempty
  , testGroup "Deduplicate"
    [ testGroup "Semigroup laws"
      [ testProperty "associative" $ \(a :: Deduplicate (EntityMap Int)) b c ->
          a <> (b <> c) === (a <> b) <> c
      ]
    ]
  , testGroup "Eq laws"
    [ testProperty "reflexivity" $ \(em :: EntityMap Int) ->
        em == em
    , testProperty "symmetric" $ \(em₁ :: EntityMap Int) em₂ ->
        (em₁ == em₂) == (em₂ == em₁)
    , testProperty "transitive" $ \(em₁ :: EntityMap Int) em₂ em₃ ->
        if (em₁ == em₂ && em₂ == em₃)
        then (em₁ == em₃)
        else True
    ]
  , testGroup "JSON encoding/decoding"
    [ testProperty "Preserves IDs" $ \(em :: EntityMap Int) ->
        let Just em' = JSON.decode $ JSON.encode em
        in toEIDsAndPositioned em' === toEIDsAndPositioned em
    ]
  ]
