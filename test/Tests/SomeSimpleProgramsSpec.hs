module Tests.SomeSimpleProgramsSpec (spec) where

import qualified Data.Array as A
import qualified Data.Sequence as Seq

import Test.Hspec

import LazyTIS100.Prelude
import Lib

scratch_useg0_layout :: Cpu l Int
scratch_useg0_layout = A.listArray ((0,1),(4,4))
    [ BrokenNode, InputNode [], BrokenNode, BrokenNode
    , emptyComputeNode, emptyComputeNode, emptyComputeNode, emptyComputeNode
    , emptyComputeNode, emptyComputeNode, emptyComputeNode, emptyComputeNode
    , emptyComputeNode, emptyComputeNode, emptyComputeNode, emptyComputeNode
    , BrokenNode, BrokenNode, OutputNode 39 Seq.empty Seq.empty, BrokenNode ]

scratch_useg0_1 :: Cpu l Int
scratch_useg0_1 = either error id $ initWithPrograms initialNodeState scratch_useg0_layout
    [ [], [MOV (SPort ANY) TAcc], [MOV (SImmediate 4) (TPort LEFT)], []
    , [MOV (SPort ANY) TAcc], [MOV (SImmediate 1) (TPort ANY)], [], []
    , [MOV (SImmediate 2) (TPort ANY)], [], [], [] ]

spec :: Spec
spec = describe "read/write priority" $ do
    context "useg0, example 1" $ do
        let (step0 : step1 : step2 : step3 : step4 : _) = steps scratch_useg0_1
        it "step 0" $
            getAccs step0 `shouldBe` [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]]
        it "step 1" $ do
            getModes step1 `shouldBe` [
                [FINISHED, FINISHED, FINISHED, FINISHED],
                [FINISHED, READ ANY, WRITE LEFT 4, FINISHED],
                [READ ANY, WRITE ANY 1, FINISHED, FINISHED],
                [WRITE ANY 2, FINISHED, FINISHED, FINISHED],
                [FINISHED, FINISHED, FINISHED, FINISHED] ]
            getAccs step1 `shouldBe` [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]]
        it "step 2" $ do
            getModes step2 `shouldBe` [
                [FINISHED, FINISHED, FINISHED, FINISHED],
                [FINISHED, RUN, RUN, FINISHED],
                [RUN, RUN, FINISHED, FINISHED],
                [WRITE ANY 2, FINISHED, FINISHED, FINISHED],
                [FINISHED, FINISHED, FINISHED, FINISHED] ]
            getAccs step2 `shouldBe` [[0, 0, 0, 0], [0, 4, 0, 0], [1, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]]
        it "step 3" $ do
            getModes step3 `shouldBe` [
                [FINISHED, FINISHED, FINISHED, FINISHED],
                [FINISHED, READ ANY, WRITE LEFT 4, FINISHED],
                [RUN, WRITE ANY 1, FINISHED, FINISHED],
                [RUN, FINISHED, FINISHED, FINISHED],
                [FINISHED, FINISHED, FINISHED, FINISHED] ]
            getAccs step3 `shouldBe` [[0, 0, 0, 0], [0, 4, 0, 0], [2, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]]
            getLastPorts step3 `shouldBe` [
                [LAST, LAST, LAST, LAST],
                [LAST, RIGHT, LAST, LAST],
                [DOWN, LEFT, LAST, LAST],
                [UP, LAST, LAST, LAST],
                [LAST, LAST, LAST, LAST]]
        it "step 4" $ do
            getModes step4 `shouldBe` [
                [FINISHED, FINISHED, FINISHED, FINISHED],
                [FINISHED, RUN, RUN, FINISHED],
                [RUN, RUN, FINISHED, FINISHED],
                [WRITE ANY 2, FINISHED, FINISHED, FINISHED],
                [FINISHED, FINISHED, FINISHED, FINISHED] ]
            getAccs step4 `shouldBe` [[0, 0, 0, 0], [0, 4, 0, 0], [1, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]]
            getLastPorts step4 `shouldBe` [
                [LAST, LAST, LAST, LAST],
                [LAST, RIGHT, LAST, LAST],
                [RIGHT, LEFT, LAST, LAST],
                [UP, LAST, LAST, LAST],
                [LAST, LAST, LAST, LAST]]
