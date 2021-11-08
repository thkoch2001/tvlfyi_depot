--------------------------------------------------------------------------------
module Xanthous.Entities.RawTypesSpec (main, test) where
--------------------------------------------------------------------------------
import           Test.Prelude
--------------------------------------------------------------------------------
import           Xanthous.Entities.RawTypes
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous.Entities.RawTypesSpec"
  [ testGroup "CreatureGenerateParams"
    [ testBatch $ monoid @CreatureGenerateParams mempty
    ]
  ]
