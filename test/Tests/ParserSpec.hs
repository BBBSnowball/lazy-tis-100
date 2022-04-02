{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Tests.ParserSpec (spec) where

import qualified Data.Array as A
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Data.Attoparsec.Text (parseOnly)

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


p1program = [text|
        @0
        MOV UP, DOWN

        @1
        MOV RIGHT, DOWN

        @2
        MOV UP, LEFT

        @3
        MOV UP, DOWN

        @4
        MOV UP, DOWN

        @5
        MOV UP, DOWN

        @6
        MOV UP, RIGHT

        @7
        MOV LEFT, DOWN
    |]

parseProgramsToList :: T.Text -> Either String (Map.Map Int [Instruction Int Int])
parseProgramsToList str = Map.map A.elems <$> parseOnly LazyTIS100.Parser.programs str


spec :: Spec
spec = do
    describe "text format of puzzles" $ do
        it "can be converted to text" $ show p1 `shouldBe` p1str <> "\n"
        it "can be parsed" $ read p1str `shouldBe` p1
    describe "text format of programs" $ do
        it "can be parsed" $ parseProgramsToList p1program `shouldBe` Right (Map.fromList [
            (0, [MOV (SPort UP) (TPort DOWN)]), (1, [MOV (SPort RIGHT) (TPort DOWN)]),
            (2, [MOV (SPort UP) (TPort LEFT)]), (3, [MOV (SPort UP) (TPort DOWN)]),
            (4, [MOV (SPort UP) (TPort DOWN)]), (5, [MOV (SPort UP) (TPort DOWN)]),
            (6, [MOV (SPort UP) (TPort RIGHT)]), (7, [MOV (SPort LEFT) (TPort DOWN)]) ])
