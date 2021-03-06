{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Tests.Puzzle00150SelfTestDiagnosticSpec where

import qualified Data.Array as A
import qualified Data.Text as T

import NeatInterpolation (text)
import Test.HUnit
import Test.Hspec

import LazyTIS100.Prelude
import LazyTIS100.Parser
import Lib

import Tests.Debugger

layout00150str = [text|
    - 00150: SELF-TEST DIAGNOSTIC -
    IN @0: X
        51, 62, 16, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
        21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39
    IN @3: A
        51, 62, 16, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
        21, 22, 23, 24, 25, 26, 27, 28, -29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39
    OUT@0: X
        51, 62, 16, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
        21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39
    OUT@3: A
        51, 62, 16, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
        21, 22, 23, 24, 25, 26, 27, 28, -29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39
    ---
    I..I
    cXcc
    cXcX
    cXcc
    O..O
|]
--layout00150 = parseOnlyFull LazyTIS100.Parser.puzzleParser layout00150str

solution00150str = [text|
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
--solution00150 = parseOnlyFull LazyTIS100.Parser.programsParser solution00150str

startingStateParsed = parsePuzzleWithPrograms layout00150str (seedForSpecAndTest 150 1) solution00150str

assertInEither check (Left msg) = assertFailure $ "expected Right but got: " <> msg
assertInEither check (Right x) = check x

assertLengthEqual label expected xs = assertEqual (label <> ": " <> show xs) expected (length xs)

spec :: Spec
spec = describe "Puzzle 00150: SELF-TEST DIAGNOSTIC" $ do
    it "can be parsed" $ assertEqual "" (Right ()) (second (const ()) startingStateParsed)
    let Right (puzzle, startingState) = startingStateParsed
    let (unusedSteps, cpustate) = stepN 100 startingState
    let inputX0 = getInputStreamByName "X" puzzle startingState
    let inputA0 = getInputStreamByName "A" puzzle startingState
    let inputX = getInputStreamByName "X" puzzle cpustate
    let inputA = getInputStreamByName "A" puzzle cpustate
    let outputX = getOutputStreamActualByName "X" puzzle cpustate
    let outputA = getOutputStreamActualByName "A" puzzle cpustate
    it "finishes in under 100 steps" $ unusedSteps >= 0
    it "finishes in exactly 83 steps" $ assertEqual "" 83 (100 - unusedSteps)
    it "starts with 39 items in both input streams" $ do
        assertInEither (assertLengthEqual "X" 39) inputX0
        assertInEither (assertLengthEqual "A" 39) inputA0
    it "drains both input streams" $ do
        assertEqual "X" (Right []) inputX
        assertEqual "A" (Right []) inputA
    it "outputs the correct items to OUT.X" $ assertEqual "X" inputX0 $ fmap toList outputX
    it "outputs the correct items to OUT.A" $ assertEqual "A" inputA0 $ fmap toList outputA

debug = debugTIS startingStateParsed
