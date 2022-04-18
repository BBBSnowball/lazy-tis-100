{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Tests.Puzzle10981SignalAmplifierSpec where

import qualified Data.Array as A
import qualified Data.Text as T

import NeatInterpolation (text)
import Test.HUnit
import Test.Hspec

import LazyTIS100.Prelude
import LazyTIS100.Parser
import Lib

import Tests.Debugger

layout10981str = [text|
    - 10981: SIGNAL AMPLIFIER -
    IN: A
        66, 34, 88, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
        21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39
    OUT: A
    ---
    .I..
    cccX
    cccc
    Xccc
    ..O.
|]
--layout10981 = parseOnlyFull LazyTIS100.Parser.puzzleParser layout10981str

solution10981str = [text|
    @0
    MOV RIGHT, DOWN
    MOV RIGHT, ACC
    ADD ACC
    MOV ACC, DOWN

    @1
    MOV UP, DOWN
    MOV UP, RIGHT
    MOV UP, LEFT
    MOV UP, RIGHT
    MOV UP, LEFT
    MOV UP, DOWN
    MOV UP, RIGHT
    MOV UP, ACC
    ADD ACC
    MOV ACC, RIGHT

    @2
    MOV LEFT, DOWN
    MOV LEFT, ACC
    ADD ACC
    MOV ACC, DOWN
    MOV LEFT, DOWN
    MOV LEFT, DOWN

    @3
    MOV UP, ACC
    ADD ACC
    MOV ACC, RIGHT
    MOV UP, RIGHT

    @4
    MOV UP, DOWN
    MOV LEFT, DOWN
    MOV UP, ACC
    ADD ACC
    MOV LEFT, DOWN
    MOV ACC, DOWN

    @5
    MOV UP, ACC
    ADD ACC
    MOV ACC, DOWN
    MOV UP, DOWN
    MOV UP, DOWN
    MOV UP, DOWN

    @6


    @7
    MOV UP, ACC
    ADD ACC
    MOV ACC, RIGHT
    MOV UP, RIGHT
    MOV UP, RIGHT
    MOV UP, RIGHT

    @8
    MOV LEFT, DOWN
    MOV UP, DOWN
    MOV LEFT, DOWN
    MOV UP, DOWN
    MOV UP, ACC
    MOV LEFT, DOWN
    MOV LEFT, DOWN
    ADD ACC
    MOV ACC, DOWN
    MOV UP, DOWN

    @9
    |]
--solution10981 = parseOnlyFull LazyTIS100.Parser.programsParser solution10981str

startingStateParsed = parsePuzzleWithPrograms' layout10981str (seedForSpecAndTest 10981 1) solution10981str $ \puzzle -> do
    input <- getInputStreamGeneratorByName "A" puzzle
    setOutputStreamExpectedByName "A" (\seed -> map (2*) (input seed)) puzzle

assertInEither check (Left msg) = assertFailure $ "expected Right but got: " <> msg
assertInEither check (Right x) = check x

assertLengthEqual label expected xs = assertEqual (label <> ": " <> show xs) expected (length xs)

spec :: Spec
spec = describe "Puzzle 10981: SIGNAL AMPLIFIER" $ do
    it "can be parsed" $ assertEqual "" (Right ()) (second (const ()) startingStateParsed)
    let Right (puzzle, startingState) = startingStateParsed
    let (unusedSteps, cpustate) = stepN 100 startingState
    let inputA0 = getInputStreamByName "A" puzzle startingState
    let inputA = getInputStreamByName "A" puzzle cpustate
    let outputA = getOutputStreamActualByName "A" puzzle cpustate
    let expectedOutput = map (2*) <$> inputA0
    it "finishes in under 100 steps" $ unusedSteps >= 0
    it "finishes in exactly 96 steps" $ assertEqual "" 96 (100 - unusedSteps)
    it "starts with 39 items in the input stream" $
        assertInEither (assertLengthEqual "A" 39) inputA0
    it "drains the input stream" $
        assertEqual "A" (Right []) inputA
    it "outputs the correct items to OUT.A" $ assertEqual "A" expectedOutput $ fmap toList outputA

debug = debugTIS startingStateParsed
