--------------------------------------------------------------------------------
module Spec where
--------------------------------------------------------------------------------
import Test.Hspec
import Main hiding (main)
import qualified Data.HashSet as HS
--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  describe "dotted-squares" $ do
    describe "parseInput" $ do
      it "works as expected" $ do
        input <- readFile "input-a.txt"
        parseInput input `shouldBe` Just (Game mempty [ mkLine (Point 0 0) (Point 1 0)
                                                      , mkLine (Point 0 0) (Point 0 1)
                                                      ])

      it "fails when the game has too many user moves" $ do
        input <- readFile "too-many-moves.txt"
        parseInput input `shouldBe` Nothing

      it "fails when the game has too few user moves" $ do
        input <- readFile "too-few-moves.txt"
        parseInput input `shouldBe` Nothing

    describe "shiftLine" $ do
      let horizontal = mkLineDir 1 1 DirRight
          vertical   = mkLineDir 1 1 DirUp
      it "can move a horizontal line up" $
        shiftLine DirUp horizontal `shouldBe` mkLineDir 1 2 DirRight
      it "can move a horizontal line down" $
        shiftLine DirDown horizontal `shouldBe` mkLineDir 1 0 DirRight
      it "can move a horizontal line left" $
        shiftLine DirLeft horizontal `shouldBe` mkLineDir 0 1 DirRight
      it "can move a horizontal line right" $
        shiftLine DirRight horizontal `shouldBe` mkLineDir 2 1 DirRight
      it "can move a vertical line up" $
        shiftLine DirUp vertical `shouldBe` mkLineDir 1 2 DirUp
      it "can move a vertical line down" $
        shiftLine DirDown vertical `shouldBe` mkLineDir 1 0 DirUp
      it "can move a vertical line left" $
        shiftLine DirLeft vertical `shouldBe` mkLineDir 0 1 DirUp
      it "can move a vertical line right" $
        shiftLine DirRight vertical `shouldBe` mkLineDir 2 1 DirUp

    describe "rotateLine" $ do
      let horizontal = mkLineDir 1 1 DirRight -- 1,1;2,1
          vertical   = mkLineDir 1 1 DirUp    -- 1,1;1,2
      it "can rotate a horizontal line CW anchored at its beginning" $
        rotateLine Beg CW horizontal `shouldBe` mkLineDir 1 1 DirDown
      it "can rotate a horizontal line CCW anchored at its beginning" $
        rotateLine Beg CCW horizontal `shouldBe` mkLineDir 1 1 DirUp
      it "can rotate a horizontal line CW anchored at its end" $
        rotateLine End CW horizontal `shouldBe` mkLineDir 2 1 DirUp
      it "can rotate a horizontal line CCW anchored at its end" $
        rotateLine End CCW horizontal `shouldBe` mkLineDir 2 1 DirDown

      it "can rotate a vertical line CW anchored at its beginning" $
        rotateLine Beg CW vertical `shouldBe` mkLineDir 1 1 DirRight
      it "can rotate a vertical line CCW anchored at its beginning" $
        rotateLine Beg CCW vertical `shouldBe` mkLineDir 1 1 DirLeft
      it "can rotate a vertical line CW anchored at its end" $
        rotateLine End CW vertical `shouldBe` mkLineDir 1 2 DirLeft
      it "can rotate a vertical line CCW anchored at its end" $
        rotateLine End CCW vertical `shouldBe` mkLineDir 1 2 DirRight

    describe "closesAnySquare" $ do
      let threeSides = [ (0, 0, DirRight)
                       , (0, 0, DirUp)
                       , (0, 1, DirRight)
                       ]
                       |> fmap (\(x, y, dir) -> mkLineDir x y dir)
                       |> HS.fromList
      it "returns true the line we supply makes a square" $
        closesAnySquare threeSides (mkLineDir 1 1 DirDown) `shouldBe` True
      it "returns false the line we supply doesn't make a square" $
        closesAnySquare threeSides (mkLineDir 1 1 DirUp) `shouldBe` False
      it "returns false when we have no existing lines" $
        closesAnySquare mempty (mkLineDir 1 1 DirUp) `shouldBe` False
