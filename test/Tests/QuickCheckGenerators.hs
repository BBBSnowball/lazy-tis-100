{-# LANGUAGE OverloadedStrings, LambdaCase, FlexibleInstances, NamedFieldPuns, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Tests.QuickCheckGenerators (
    arbitraryTISValue, arbitraryTISLabel,
    arbitraryInputNode, arbitraryOutputNode, arbitraryComputeNode,
    arbitraryCpu,
    withTISArbitrary
) where

import qualified Data.Array as A
import qualified Data.Bifunctor
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Test.QuickCheck

import LazyTIS100.Types
import LazyTIS100.Parser


withTISArbitrary :: a -> a
withTISArbitrary = id

instance Arbitrary Port where
    arbitrary = arbitraryBoundedEnum
    shrink ANY = [UP, LEFT, RIGHT, DOWN]
    shrink LAST = [UP, LEFT, RIGHT, DOWN]
    shrink _ = []

instance Arbitrary JumpCondition where
    arbitrary = arbitraryBoundedEnum

arbitraryTISValue :: Integral a => Gen a
arbitraryTISValue = fromInteger <$> chooseInteger (-999, 999)

instance (Integral n, Arbitrary n) => Arbitrary (InstructionSource n) where
    arbitrary = oneof
        [ pure SNil
        , pure SAcc
        , SPort <$> arbitrary
        , SImmediate <$> arbitraryTISValue ]
    shrink SNil = []
    shrink SAcc = [SNil]
    shrink x@(SPort _) = [SAcc] <> recursivelyShrink x
    shrink x@(SImmediate _) = [SAcc] <> recursivelyShrink x

instance Arbitrary InstructionTarget where
    arbitrary = oneof
        [ pure TNil
        , pure TAcc
        , TPort <$> arbitrary ]
    shrink TNil = []
    shrink TAcc = [TNil]
    shrink x@(TPort _) = [TAcc] <> recursivelyShrink x

arbitraryTISLabel :: Integral a => Gen a
arbitraryTISLabel = fromInteger <$> chooseInteger (0, 14)

instance (Integral l, Integral n, Arbitrary l, Arbitrary n) => Arbitrary (Instruction l n) where
    --NOTE We are not generating HCF.
    arbitrary = oneof
        [ pure NOP
        , pure SAV
        , pure SWP
        , pure NEG
        , ADD <$> arbitrary
        , SUB <$> arbitrary
        , MOV <$> arbitrary <*> arbitrary
        , JMP <$> arbitraryTISLabel
        , J <$> arbitrary <*> arbitraryTISLabel
        , JRO <$> arbitrary ]
    shrink x = next x <> recursivelyShrink x
        where
            next NOP = []
            next SAV = [NOP]
            next SWP = [NOP]
            next NEG = [NOP]
            next (ADD _) = [NEG]
            next (SUB (SImmediate x)) = [ADD (SImmediate (-x))]
            next (SUB x) = [ADD x]
            next (MOV _ _) = [NOP]
            next (JMP _) = [NOP]
            next (J _ x) = [JMP x]
            next (JRO x) = [MOV x TNil, JMP $ fromInteger 0, JMP $ fromInteger 100]
            next HCF = [JRO SNil, NOP]

instance (Integral l, Integral n, Arbitrary l, Arbitrary n) => Arbitrary (A.Array Int (Instruction l n)) where
    arbitrary = chooseInt (0, 15) >>= \length -> A.listArray (0, length-1) <$> vector length
    shrink = fmap listArrayFromZero . shrinkList shrink . A.elems

listArrayFromZero :: (Integral a, A.Ix a) => [b] -> A.Array a b
listArrayFromZero xs = A.listArray (0, fromInteger $ toInteger $ length xs - 1) xs

instance (Integral n, Arbitrary n) => Arbitrary (NodeMode n) where
    arbitrary = oneof
        [ pure IDLE
        , pure RUN
        , pure FINISHED
        , pure ONFIRE
        , READ <$> arbitrary
        , HasRead <$> arbitrary
        , WRITE <$> arbitrary <*> arbitrary
        , pure HasWritten ]
    shrink IDLE = []
    shrink RUN = []
    shrink FINISHED = []
    shrink _ = [IDLE, RUN]

instance (Integral n, Arbitrary n) => Arbitrary (NodeState n) where
    arbitrary = NodeState <$> arbitraryTISValue <*> arbitraryTISValue <*> arbitrary <*> chooseInt (0, 15) <*> arbitrary

instance (Integral l, Integral n, Arbitrary l, Arbitrary n) => Arbitrary (Node l n) where
    arbitrary = oneof
        [ pure BrokenNode
        , arbitraryInputNode
        , arbitraryOutputNode
        , arbitraryComputeNode ]
    shrink BrokenNode = []
    shrink x = BrokenNode : genericShrink x

arbitraryInputNode, arbitraryOutputNode, arbitraryComputeNode :: (Integral l, Integral n, Arbitrary l, Arbitrary n) => Gen (Node l n)
arbitraryInputNode = InputNode <$> resize 39 (listOf $ arbitraryTISValue)
arbitraryOutputNode = resize 39 (listOf $ arbitraryTISValue) >>= \expectedValues ->
    pure $ OutputNode (length expectedValues) (Seq.fromList expectedValues) Seq.empty
arbitraryComputeNode = fixLabels <$> (ComputeNode <$> arbitrary <*> arbitrary)

fixLabels :: forall l n. Integral l => Node l n -> Node l n
fixLabels (ComputeNode prog state@(NodeState {pc})) = ComputeNode (fmap clampInstr prog) (state {pc = clampInt pc})
    where
        (lminInt, lmaxInt) = A.bounds prog
        lmin = fromInteger $ toInteger lminInt
        lmax = fromInteger $ toInteger lmaxInt

        clamp :: l -> l
        clamp x | lmin >= lmax = lmin
                | x <= lmin = lmin
                | x >= lmax = lmax
                | otherwise = x
        clampInt :: Int -> Int
        clampInt x | lminInt >= lmaxInt = lminInt
                | x <= lminInt = lminInt
                | x >= lmaxInt = lmaxInt
                | otherwise = x

        clampInstr (JMP lbl) = JMP $ clamp lbl
        clampInstr (J cond lbl) = J cond $ clamp lbl
        clampInstr x = x
fixLabels x = x

-- instance for type alias: Cpu l n
instance (Integral l, Integral n, Arbitrary l, Arbitrary n) => Arbitrary (A.Array (Int, Int) (Node l n)) where
    arbitrary = do
        let width = 4
        let height = 3
        let manyBroken other = frequency [(8, pure BrokenNode), (2, other)]
        inputs  <- vectorOf width $ manyBroken arbitraryInputNode
        inner   <- vectorOf (width*height) arbitraryComputeNode
        outputs <- vectorOf width $ manyBroken arbitraryOutputNode
        pure $ A.listArray ((0, 0), (height, width)) $ inputs <> inner <> outputs
    shrink xs = A.listArray (A.bounds xs) <$> recursivelyShrink (A.elems xs)

arbitraryCpu :: (Integral l, Integral n, Arbitrary l, Arbitrary n) => Gen (Cpu l n)
arbitraryCpu = arbitrary
