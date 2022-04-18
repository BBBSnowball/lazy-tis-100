{-# LANGUAGE QuasiQuotes, OverloadedStrings, LambdaCase, NamedFieldPuns, ScopedTypeVariables #-}
module Tests.ParserSpec (spec) where

import qualified Data.Array as A
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Text.Read

import NeatInterpolation (text)
import Test.Hspec
import Test.QuickCheck

import LazyTIS100.Prelude
import LazyTIS100.Parser
import Lib
import Tests.QuickCheckGenerators

import Debug.Trace

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
p1programPrinted = [text|
        @0
        00: MOV UP DOWN

        @1
        00: MOV RIGHT DOWN

        @2
        00: MOV UP LEFT

        @3
        00: MOV UP DOWN

        @4
        00: MOV UP DOWN

        @5
        00: MOV UP DOWN

        @6
        00: MOV UP RIGHT

        @7
        00: MOV LEFT DOWN
    |]


parseProgramsToList :: T.Text -> Either String (Map.Map Int [Instruction Int Int])
parseProgramsToList str = Map.map A.elems <$> parseOnlyFull LazyTIS100.Parser.programsParser str

listArrayFromZero :: (Integral a, A.Ix a) => [b] -> A.Array a b
listArrayFromZero xs = A.listArray (0, fromInteger $ toInteger $ length xs - 1) xs

spec :: Spec
spec = do
    describe "text format of puzzles" $ do
        it "can be converted to text" $ show p1 `shouldBe` p1str <> "\n"
        it "can be parsed" $ read p1str `shouldBe` p1
    describe "text format of programs" $ do
        let programsAsLists = (Map.fromList [
                (0, [MOV (SPort UP) (TPort DOWN)]), (1, [MOV (SPort RIGHT) (TPort DOWN)]),
                (2, [MOV (SPort UP) (TPort LEFT)]), (3, [MOV (SPort UP) (TPort DOWN)]),
                (4, [MOV (SPort UP) (TPort DOWN)]), (5, [MOV (SPort UP) (TPort DOWN)]),
                (6, [MOV (SPort UP) (TPort RIGHT)]), (7, [MOV (SPort LEFT) (TPort DOWN)]) ])
        let programsAsArrays = Map.map (\xs -> A.listArray (0, length xs - 1) xs) programsAsLists
        it "can be parsed" $ parseProgramsToList p1program `shouldBe` Right programsAsLists
        it "can be converted to text" $ showTISPrograms programsAsArrays `shouldBe` p1programPrinted
        it "instructions can be round-tripped" $ property prop_InstructionRoundtrip

traceIf :: Bool -> String -> b -> b
traceIf cond msg = if cond then trace msg else id

traceShowIf :: Show a => Bool -> a -> b -> b
traceShowIf cond msg = if cond then traceShow msg else id

prop_InstructionRoundtrip :: Instruction Int Int -> Bool
prop_InstructionRoundtrip instr = withTISArbitrary $ let
    instr2 = showTISInstruction instr
    instr3 = parseOnlyFull LazyTIS100.Parser.instructionParser instr2
    instr3' :: Either String (Instruction Int Int)
    instr3' = instr3 >>= bitraverse parseLabel pure
    parseLabel = Text.Read.readEither . T.unpack
    instr4 = showTISInstruction <$> instr3'
    ok1 = Right instr == instr3'
    ok2 = Right instr2 == instr4
    in
        traceIf (not ok1) "ok1: " $ traceShowIf (not ok1) [Right instr, instr3'] $
        traceIf (not ok2) "ok2: " $ traceShowIf (not ok2) [Right instr2, instr4] $
        ok1 && ok2
