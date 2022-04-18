{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Tests.Puzzle20176DifferentialConverterSpec where

import qualified Data.Array as A
import qualified Data.Text as T

import NeatInterpolation (text)
import Test.HUnit
import Test.Hspec

import LazyTIS100.Prelude
import LazyTIS100.Parser
import Lib

import Tests.Debugger

layout20176str = [text|
    - 20176: DIFFERENTIAL CONVERTER -
    IN: A
        66, 34, 88, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
        21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39
    IN: B
        51, 62, 16, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
        21, 22, 23, 24, 25, 26, 27, 28, -29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39
    OUT: P
    OUT: N
    ---
    .II.
    cccc
    cccX
    cccc
    .OO.
|]
--layout20176 = parseOnlyFull LazyTIS100.Parser.puzzleParser layout20176str

solution20176str = [text|
    @1
    MOV UP, ACC
    SUB RIGHT
    MOV ACC, DOWN

    @2
    MOV UP, LEFT

    @5
    MOV UP, DOWN

    @8
    MOV UP, ACC
    MOV ACC, RIGHT
    MOV ACC, DOWN

    @9
    MOV LEFT, ACC
    NEG
    MOV ACC, DOWN
    |]
--solution20176 = parseOnlyFull LazyTIS100.Parser.programsParser solution20176str

startingStateParsed = parsePuzzleWithPrograms' layout20176str (seedForSpecAndTest 20176 1) solution20176str $ \puzzle -> do
    inputA <- getInputStreamGeneratorByName "A" puzzle
    inputB <- getInputStreamGeneratorByName "B" puzzle
    puzzle' <- setOutputStreamExpectedByName "P" (\seed -> zipWith (-) (inputA seed) (inputB seed)) puzzle
    setOutputStreamExpectedByName "N" (\seed -> zipWith (-) (inputB seed) (inputA seed)) puzzle'

assertInEither check (Left msg) = assertFailure $ "expected Right but got: " <> msg
assertInEither check (Right x) = check x

assertLengthEqual label expected xs = assertEqual (label <> ": " <> show xs) expected (length xs)

spec :: Spec
spec = describe "Puzzle 20176: DIFFERENTIAL CONVERTER" $ do
    it "can be parsed" $ assertEqual "" (Right ()) (second (const ()) startingStateParsed)
    let Right (puzzle, startingState) = startingStateParsed
    let (unusedSteps, cpustate) = stepN 300 startingState
    it "finishes in under 300 steps" $ unusedSteps >= 0
    it "finishes in exactly 201 steps" $ assertEqual "" 201 (300 - unusedSteps)
    it "starts with 39 items in the input streams" $ do
        assertInEither (assertLengthEqual "A" 39) $ getInputStreamByName "A" puzzle startingState
        assertInEither (assertLengthEqual "B" 39) $ getInputStreamByName "B" puzzle startingState
    it "drains the input streams" $ do
        assertEqual "A" (Right []) $ getInputStreamByName "A" puzzle cpustate
        assertEqual "B" (Right []) $ getInputStreamByName "B" puzzle cpustate
    let outputExpectedP = getOutputStreamExpectedByName "P" puzzle startingState
    let outputExpectedN = getOutputStreamExpectedByName "N" puzzle startingState
    let outputP = getOutputStreamActualByName "P" puzzle cpustate
    let outputN = getOutputStreamActualByName "N" puzzle cpustate
    it "output streams expect exactly 39 items" $ do
        assertInEither (assertLengthEqual "P" 39) outputExpectedP
        assertInEither (assertLengthEqual "N" 39) outputExpectedN
    it "outputs the correct items to OUT.P" $ assertEqual "P" outputExpectedP outputP
    it "outputs the correct items to OUT.N" $ assertEqual "N" outputExpectedN outputN

debug = debugTIS startingStateParsed
