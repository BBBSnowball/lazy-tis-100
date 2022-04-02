{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Tests.ParserSpec (spec) where

import qualified Data.Array as A
import qualified Data.Text as T

import NeatInterpolation (text)
import Test.Hspec

import Lib
import LazyTIS100.Parser

p1 = Puzzle "abc" ["xy z","def\nghi","jkl"]
    [ (StreamInput,"X.0",0,const [])
    , (StreamInput,"X.1",3,const [])
    , (StreamOutput,"Y.0",0,const [])
    , (StreamOutput,"Y.1",3,const [])]
    (A.listArray ((0,0),(2,3))
        [TileCompute,TileDamaged,TileCompute,TileCompute
        ,TileCompute,TileDamaged,TileCompute,TileDamaged
        ,TileCompute,TileDamaged,TileCompute,TileCompute])

p1str = T.unpack [text|
    - abc -
    > xy z
    > def
      ghi
    > jkl
    IN @0: X.0
    IN @3: X.1
    OUT@0: Y.0
    OUT@3: Y.1
    ---
    I..I
    cXcc
    cXcX
    cXcc
    O..O
|]


spec :: Spec
spec = describe "text format of puzzles" $ do
    it "can be converted to text" $ show p1 `shouldBe` p1str <> "\n"
    it "can be parsed" $ read p1str `shouldBe` p1
