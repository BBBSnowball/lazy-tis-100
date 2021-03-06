{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase, NamedFieldPuns, ScopedTypeVariables #-}
module LazyTIS100.Parser (
    StreamType (..),
    StreamGenerator,
    TileType (..),
    Puzzle (..),
    puzzleParser,
    portParser, instructionSourceParser, instructionTargetParser, instructionParser, labelParser,
    programParser, programsParser,
    showTISInstruction, showTISProgram, showTISPrograms,
    initPuzzleWithPrograms, seedForSpecAndTest, parsePuzzleWithPrograms, parsePuzzleWithPrograms',
    getInputStreamByPosX, getInputStreamByName, getOutputStreamByPosX, getOutputStreamByName,
    getOutputStreamActualByName, getOutputStreamActualValues,
    getOutputStreamExpectedByPosX, getOutputStreamExpectedByName,
    getInputStreamGeneratorByName, setInputStreamGeneratorByName, setOutputStreamExpectedByName,

    parseOnlyReadS, parseOnlyFull
) where

import Prelude hiding (takeWhile)

import Control.Applicative
import Control.Monad (join)
import qualified Control.Monad.State

import Data.Attoparsec.Text

import qualified Data.Array as A
import qualified Data.Bits
import Data.Char (isSpace)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T

import qualified Debug.Trace as Trace

import LazyTIS100.Prelude
import LazyTIS100.Types hiding (instructionSource, instructionTarget)

data StreamType = StreamInput | StreamOutput | StreamImage
    deriving (Eq, Ord, Enum, Show, Read)
type StreamGenerator = Int -> [Int]  -- argument is the seed
data TileType = TileCompute | TileMemory | TileDamaged
    deriving (Eq, Ord, Enum, Show, Read)

data Puzzle = Puzzle {
    puzzleName :: Text,
    puzzleDescription :: [Text],
    puzzleStreams :: [(StreamType, Text, Int, StreamGenerator)],
    puzzleLayout :: Array (Int, Int) TileType
}

instance Eq Puzzle where
    a == b = puzzleName a == puzzleName b && puzzleDescription a == puzzleDescription b && puzzleLayout a == puzzleLayout b
        && eqStreams (puzzleStreams a) (puzzleStreams b)
        where
            eqStreams [] [] = True
            eqStreams ((x1,x2,x3,x4) : xs) ((y1,y2,y3,y4) : ys) = x1 == y1 && x2 == y2 && x3 == y3 && x4 0 == y4 0
            eqStreams _ _ = False

instance Show Puzzle where
    show Puzzle { .. } = T.unpack $
        "- " <> puzzleName <> " -\n"
        <> T.intercalate "" (map (\desc -> "> " <> T.replace "\n" "\n  " desc <> "\n") puzzleDescription)
        <> T.intercalate "" (map showStream puzzleStreams)
        <> "---\n"
        <> showLayout
        where
            showStream (stype, name, pos, gen) = let elems = Prelude.take 39 $ gen 0 in
                (case stype of { StreamInput -> "IN "; StreamOutput -> "OUT"; StreamImage -> "IMG" })
                <> (if pos >= 0 then "@" <> T.pack (show pos) else "")
                <> (if not (T.null name) then ": " <> name else "")
                <> "\n"
                <> if Prelude.null elems then "" else "  " <> T.intercalate ", " (map (T.pack . show) elems) <> "\n"

            ((y0,x0),(y1,x1)) = A.bounds puzzleLayout

            showLayout = showInout [StreamInput] 'I' <> showTiles <> showInout [StreamOutput, StreamImage] 'O'

            positionSet types = Set.fromList $ catMaybes $ flip map puzzleStreams $
                (\(stype,_,pos,_) -> if stype `elem` types then Just pos else Nothing)

            showInout types c = let positions = positionSet types in
                if Set.null positions || Set.findMin positions < x0 || Set.findMax positions > x1 then ""
                    else T.pack $ map (\p -> if Set.member p positions then c else '.') [x0..x1] ++ "\n"

            showTiles = T.intercalate "" $ flip map [y0..y1] $ \y ->
                (<>"\n") . T.intercalate "" . flip map [x0..x1] $ \x ->
                    case puzzleLayout A.! (y, x) of
                        TileCompute -> "c"
                        TileMemory  -> "M"
                        TileDamaged -> "X"

instance Read Puzzle where
    readsPrec _ = parseOnlyReadS puzzleParser

parseOnlyReadS :: Parser a -> ReadS a
parseOnlyReadS p x = case parseOnly ((,) <$> p <*> takeText) (T.pack x) of
        Left msg -> error msg
        Right (x' ,y) -> [(x', T.unpack y)]

isSpaceInLine c = isSpace c && c /= '\n'
skipSpaceInLine = skipWhile isSpaceInLine

puzzleParser :: Parser Puzzle
puzzleParser = do
    hline <- headerline <?> "name"
    skipSpace
    description <- many descriptionBulletPoint <?> "description"
    skipSpace
    streams <- many (stream <* skipSpace) <?> "streams"
    (inputLine, tileArray, outputLine) <- layout <?> "layout"

    streams'  <- matchStreams "input"  streams [StreamInput] inputLine
    streams'' <- matchStreams "output" streams' [StreamOutput, StreamImage] outputLine
    skipSpace

    pure $ Puzzle hline description streams'' tileArray
    where
        headerline = do
            name <- (string "- " <?> "start") >> takeWhile1 (not . isEndOfLine) <* endOfLine
            let name' = T.dropWhile isSpace name
            when (T.takeEnd 2 name' /= " -") $ fail "expecting \" -\" after name"
            pure $ T.dropEnd 2 name'
        descriptionBulletPoint = do
            void $ string "> "
            firstLine <- takeWhile1 (not . isEndOfLine)
            continued <- many (endOfLine *> string "  " *> takeWhile1 (not . isEndOfLine))
            endOfLine
            pure $ T.intercalate "\n" (firstLine : continued)
        stream = do
            stype <- ((asciiCI "IN" >> pure StreamInput) <|> (asciiCI "OUT" >> pure StreamOutput) <|> (asciiCI "IMG" >> pure StreamImage))
            skipSpaceInLine
            pos <- (string "@" >> skipSpaceInLine >> decimal <* skipSpaceInLine) <|> pure (-1)
            name <- (string ":" *> option "" (string " ") *> takeWhile (not . isEndOfLine)) <|> pure ""
            endOfLine
            elems <- option [] $ (string " " *> skipSpaceInLine *> signed decimal `sepBy` (skipSpace >> string "," >> skipSpace) <* skipSpace <?> "stream items")
            pure (stype, name, pos, const elems)
        layout = do
            skipSpace >> string "---" >> skipSpaceInLine >> endOfLine <?> "layout separator"
            skipSpace
            inputLine <- option [] $ ((((string "." >> pure False) <|> (asciiCI "I" >> pure True)) `sepBy` skipSpaceInLine) <* (endOfLine <|> endOfInput) <?> "input line")
            let tileType =
                    ((asciiCI "X" <|> string "-") >> pure TileDamaged)
                    <|> (asciiCI "C" >> pure TileCompute)
                    <|> (asciiCI "M" >> pure TileMemory)
                    <?> "tile type"
            tileLines <- many1 $ skipSpaceInLine *> ((tileType `sepBy` skipSpaceInLine) <* skipSpaceInLine <* (endOfLine <|> endOfInput) <?> "layout line")
            outputLine <- option [] $ ((((string "." >> pure False) <|> (asciiCI "O" >> pure True)) `sepBy` skipSpaceInLine) <* (endOfLine <|> endOfInput) <?> "output line")
            skipSpace

            let width = Prelude.maximum $ length inputLine : length outputLine : map length tileLines
            let height = length tileLines
            let padded = map (\line -> Prelude.take width (line <> repeat TileDamaged)) tileLines
            pure (inputLine, A.listArray ((0,0), (height-1, width-1)) (concat padded), outputLine)
        matchStreams name streams types lineWithPositions = let
            positions1 = catMaybes $ map (\(i, x) -> if x then Just i else Nothing) $ zip [0..] lineWithPositions
            positions2 = catMaybes $ map (\(stype,_,pos,_) -> if stype `elem` types then Just pos else Nothing) streams
            positions1' = Set.toList $ Set.difference (Set.fromList positions1) (Set.fromList positions2)
            match pos1 (x@(stype,sname,pos,sgen) : xs)
                | not (stype `elem` types) || pos >= 0 = (x :) <$> match pos1 xs
                | otherwise = case pos1 of
                    [] -> fail $ "missing " <> name <> " positions"
                    (pos' : ps) -> ((stype,sname,pos',sgen) :) <$> match ps xs
            match [] [] = pure []
            match (p : ps) [] = ((head types,"",p,const []) :) <$> match ps []
            in match positions1' streams


port, portParser :: Parser Port
portParser = port
port = choice
    [ asciiCI "UP" >> pure UP
    , asciiCI "LEFT" >> pure LEFT
    , asciiCI "RIGHT" >> pure RIGHT
    , asciiCI "DOWN" >> pure DOWN
    , asciiCI "ANY" >> pure ANY
    , asciiCI "LAST" >> pure LAST ]
    <?> "port"

instructionSource, instructionSourceParser :: Integral n => Parser (InstructionSource n)
instructionSourceParser = instructionSource
instructionSource = choice
    [ asciiCI "ACC" >> pure SAcc
    , asciiCI "NIL" >> pure SNil
    , SPort <$> port
    , SImmediate <$> (signed decimal) ]
    <* skipSpaceInLine
    <?> "source"

instructionTarget, instructionTargetParser :: Parser InstructionTarget
instructionTargetParser = instructionTarget
instructionTarget = choice
    [ asciiCI "ACC" >> pure TAcc
    , asciiCI "NIL" >> pure TNil
    , TPort <$> port ]
    <* skipSpaceInLine
    <?> "target"

label, labelParser :: Parser Text
labelParser = label
label = T.pack <$> (many1 (letter <|> digit) <* skipSpaceInLine <?> "label")

instruction, instructionParser :: Integral n => Parser (Instruction Text n)
instructionParser = instruction
instruction = choice
    [ asciiCI "JMP" *> skipSpaceInLine *> (JMP <$> label) <?> "JMP"
    , asciiCI "JEZ" *> skipSpaceInLine *> (J JEZ <$> label) <?> "JEZ"
    , asciiCI "JNZ" *> skipSpaceInLine *> (J JNZ <$> label) <?> "JNZ"
    , asciiCI "JGZ" *> skipSpaceInLine *> (J JGZ <$> label) <?> "JGZ"
    , asciiCI "JLZ" *> skipSpaceInLine *> (J JLZ <$> label) <?> "JLZ"
    , asciiCI "JRO" *> skipSpaceInLine *> (JRO <$> instructionSource) <?> "JRO"
    , asciiCI "MOV" *> skipSpaceInLine *> (MOV <$> instructionSource <* optComma <*> instructionTarget) <?> "MOV"
    , asciiCI "ADD" *> skipSpaceInLine *> (ADD <$> instructionSource) <?> "ADD"
    , asciiCI "SUB" *> skipSpaceInLine *> (SUB <$> instructionSource) <?> "SUB"
    , asciiCI "NEG" *> skipSpaceInLine *> pure NEG <?> "NEG"
    , asciiCI "SWP" *> skipSpaceInLine *> pure SWP <?> "SWP"
    , asciiCI "SAV" *> skipSpaceInLine *> pure SAV <?> "SAV"
    , asciiCI "NOP" *> skipSpaceInLine *> pure NOP <?> "NOP"
    , asciiCI "HCF" *> skipSpaceInLine *> pure HCF <?> "HCF"
    ] <* optComment
    where
        optComma = option "" (string ",") >> skipSpaceInLine

programParser :: Integral n => Parser (NodeProgram n n)
programParser = do
    skipSpaceAndComments
    lines <- programLine `sepBy` (optComment >> endOfLine)
    skipSpaceAndComments
    let labelToAddr = Map.fromList $ do
            (i, (lbls, _)) <- zip [0..] lines
            lbl <- lbls
            pure (lbl, i)
    insts <- forM lines $ \(_, inst) -> case mapLabel inst labelToAddr of
        Left lbl -> fail $ "missing label: " <> T.unpack lbl
        Right inst' -> pure inst'
    pure $ A.listArray (0, length insts - 1) insts
    where
        optBreakpoint = option "" (string "!") >> skipSpaceInLine
        programLine = do
            skipSpaceAndComments 
            labels <- many (try $ optBreakpoint *> label <* ":" <* skipSpaceAndComments)
            instr <- optBreakpoint *> instruction
            pure (labels, instr)
        mapLabel (JMP lbl) m = case Map.lookup lbl m of
            Nothing -> Left lbl
            Just lbl' -> Right $ JMP lbl'
        mapLabel (J cond lbl) m = case Map.lookup lbl m of
            Nothing -> Left lbl
            Just lbl' -> Right $ J cond lbl'
        mapLabel (JRO x) _ = Right $ JRO x
        mapLabel (MOV x y) _ = Right $ MOV x y
        mapLabel (ADD x) _ = Right $ ADD x
        mapLabel (SUB x) _ = Right $ SUB x
        mapLabel NEG _ = Right $ NEG
        mapLabel SWP _ = Right $ SWP
        mapLabel SAV _ = Right $ SAV
        mapLabel NOP _ = Right $ NOP
        mapLabel HCF _ = Right $ HCF

        skipSpaceAndComments = void $ skipSpace `sepBy` comment

comment, optComment :: Parser ()
comment = void $ char '#' >> skipWhile (/='\n') >> (endOfLine <|> endOfInput)
optComment = option () comment

programsParser :: Integral n => Parser (Map.Map n (NodeProgram n n))
programsParser = Map.fromList <$> many programWithNum
    where
        programWithNum = (,) <$> (skipSpace *> string "@" *> decimal <* skipSpace) <*> programParser


showIntegralT :: Integral a => a -> Text
showIntegralT = showT . toInteger

showTISInstruction :: (Show l, Show n) => Instruction l n -> Text
showTISInstruction (J cond lbl) = T.pack $ show cond <> " " <> show lbl
showTISInstruction x = showT x

showTISProgram :: (Show n, Integral n) => NodeProgram n n -> Text
showTISProgram prog = T.intercalate "\n" $ map showInstructionWithLabel $ A.assocs prog
    where showInstructionWithLabel (idx, instr) = (if idx < 10 then "0" else "") <> (showIntegralT idx) <> ": " <> showTISInstruction instr

showTISPrograms :: (Show n, Integral n) => Map.Map n (NodeProgram n n) -> Text
showTISPrograms progs = T.intercalate "\n\n" $ map showProgramWithIndex $ Map.toAscList progs
    where showProgramWithIndex (idx, prog) = "@" <> (showIntegralT idx) <> "\n" <> showTISProgram prog


-- like initWithPrograms but with types that are returned by the parsers
initPuzzleWithPrograms :: forall n. (Integral n, Show n, Eq n) => Puzzle -> Int -> Map.Map n (NodeProgram n n) -> Either String (Cpu n n)
initPuzzleWithPrograms Puzzle {puzzleStreams, puzzleLayout} seed progs = case remainingPrograms of
    _ | any (==TileMemory) (A.elems puzzleLayout) -> Left "memory nodes are not supported, yet"
    _ | any isImageStream puzzleStreams -> Left "image streams are not supported, yet"
    (_:_) -> Left $ "too many programs: " <> show (map fst remainingPrograms)
    [] -> pure $ A.array ((minInnerY-1, minX), (maxInnerY+1, maxX)) $
        A.assocs (initStreamNodes StreamInput (minInnerY-1) (InputNode . genStream))
        <> A.assocs (initStreamNodes StreamOutput (maxInnerY+1) (makeOutputNode . genStream))
        <> A.assocs innerNodes
    where
        (innerNodes, (_, remainingPrograms)) = Control.Monad.State.runState (traverse initInnerNode puzzleLayout) (0, Map.assocs progs)
        initInnerNode :: TileType -> Control.Monad.State.State (n, [(n, NodeProgram n n)]) (Node n n)
        initInnerNode TileDamaged = pure $ BrokenNode
        initInnerNode TileMemory = error "memory nodes are not supported, yet"
        initInnerNode TileCompute = Control.Monad.State.get >>= \case
            (idx, (idx2, prog) : progs') | idx == idx2 -> do
                Control.Monad.State.put (idx+1, progs')
                pure $ ComputeNode prog initialNodeState
            (idx, progs) -> Control.Monad.State.put (idx+1, progs) >> pure emptyComputeNode

        ((minInnerY, minX), (maxInnerY, maxX)) = A.bounds puzzleLayout

        isImageStream (StreamImage, _, _, _) = True
        isImageStream _ = False

        --verboseArray bounds assocs = Trace.traceShow bounds $ Trace.traceShow assocs $ A.array bounds assocs

        initStreamNodes stype idx mkStream = A.array ((idx, minX), (idx, maxX)) (defaultValues <> streamNodes)
            where
                defaultValues = [((idx, i), BrokenNode) | i <- [minX..maxX]]
                streamNodes = catMaybes $ map (makeStreamNode stype) puzzleStreams
                makeStreamNode StreamInput (StreamInput, _, posX, gen) = Just ((idx, posX), mkStream gen)
                makeStreamNode StreamOutput (StreamOutput, _, posX, gen) = Just ((idx, posX), mkStream gen)
                makeStreamNode _ _ = Nothing
        genStream generator = map (fromInteger . toInteger) $ generator seed
        makeOutputNode expected = let expectedAsSeq = Seq.fromList expected
            in OutputNode (Seq.length expectedAsSeq) expectedAsSeq Seq.empty

parseOnlyFull :: Parser a -> T.Text -> Either String a
parseOnlyFull p t = do
    (result, remaining) <- parseOnly ((,) <$> p <*> takeText) t
    if T.null remaining
        then pure result
        else Left $ "remaining input: " <> show remaining

parsePuzzleWithPrograms :: forall n. (Integral n, Show n, Eq n) => T.Text -> Int -> T.Text -> Either String (Puzzle, Cpu n n)
parsePuzzleWithPrograms pzl seed progs = do
    --join $ initPuzzleWithPrograms <$> parseOnlyFull puzzleParser pzl <*> parseOnlyFull programsParser progs
    pzl' <- parseOnlyFull (puzzleParser <?> "puzzle") pzl
    progs' <- parseOnlyFull (programsParser <?> "programs") progs
    initialCpuState <- initPuzzleWithPrograms pzl' seed progs'
    pure (pzl', initialCpuState)

parsePuzzleWithPrograms' :: forall n. (Integral n, Show n, Eq n) => T.Text -> Int -> T.Text -> (Puzzle -> Either String Puzzle) -> Either String (Puzzle, Cpu n n)
parsePuzzleWithPrograms' pzl seed progs f = do
    --join $ initPuzzleWithPrograms <$> parseOnlyFull puzzleParser pzl <*> parseOnlyFull programsParser progs
    pzl' <- parseOnlyFull (puzzleParser <?> "puzzle") pzl
    pzl'' <- f pzl'
    progs' <- parseOnlyFull (programsParser <?> "programs") progs
    initialCpuState <- initPuzzleWithPrograms pzl'' seed progs'
    pure (pzl'', initialCpuState)

getStreamByPosX' :: StreamType -> Int -> Puzzle -> Cpu l n -> Either String (Node l n)
getStreamByPosX' stype posX Puzzle {puzzleLayout} cpustate =
    case (stype, cpustate `at` (posY, posX)) of
        (StreamInput, Just node@(InputNode _)) -> Right node
        (StreamOutput, Just node@(OutputNode _ _ _)) -> Right node
        (_, Nothing) -> Left "no node at that index"
        (StreamImage, _) -> Left "images are not supported, yet"
        _ -> Left "node at that index has an unexpected type"
    where
        posY = case stype of
            StreamInput -> -1
            _ -> let (_, (maxInnerY, _)) = A.bounds puzzleLayout in maxInnerY+1

getStreamByName' :: StreamType -> Text -> Puzzle -> Cpu l n -> Either String (Node l n)
getStreamByName' stype name puzzle cpustate = do
    (_, _, posX, _) <- getPuzzleStreamByName' stype name puzzle
    getStreamByPosX' stype posX puzzle cpustate

getPuzzleStreamByName' :: StreamType -> Text -> Puzzle -> Either String (StreamType, Text, Int, StreamGenerator)
getPuzzleStreamByName' stype name puzzle@Puzzle {puzzleStreams} = findStream puzzleStreams
    where
        findStream [] = Left $ "no such " <> show stype <> " " <> show name <> ", we have " <> show (map (\(x,y,_,_) -> (x,y)) puzzleStreams)
        findStream (s@(stype2, name2, _, _) : xs)
            | stype == stype2 && name == name2 = Right s
            | otherwise = findStream xs

modifyStreamByName' :: StreamType -> Text -> ((StreamType, Text, Int, StreamGenerator) -> Either String (StreamType, Text, Int, StreamGenerator)) -> Puzzle -> Either String Puzzle
modifyStreamByName' stype name action puzzle@Puzzle {puzzleStreams} =
    findStream puzzleStreams >>= \x -> pure puzzle {puzzleStreams=x}
    where
        findStream [] = Left $ "no such " <> show stype <> " " <> show name
        findStream (s@(stype2, name2, _, _) : xs)
            | stype == stype2 && name == name2 = action s >>= \x' -> pure (x' : xs)
            | otherwise = (s:) <$> findStream xs

getInputStreamByPosX :: Int -> Puzzle -> Cpu l n -> Either String [n]
getInputStreamByPosX posX puzzle cpustate = getStreamByPosX' StreamInput posX puzzle cpustate >>= getInputStreamValues

getInputStreamByName :: Text -> Puzzle -> Cpu l n -> Either String [n]
getInputStreamByName name puzzle cpustate = getStreamByName' StreamInput name puzzle cpustate >>= getInputStreamValues

getInputStreamValues (InputNode xs) = Right xs
getInputStreamValues _ = Left "node at that index has an unexpected type"

getOutputStreamByPosX :: Int -> Puzzle -> Cpu l n -> Either String (Node l n)
getOutputStreamByPosX posX puzzle cpustate = getStreamByPosX' StreamOutput posX puzzle cpustate

getOutputStreamByName :: Text -> Puzzle -> Cpu l n -> Either String (Node l n)
getOutputStreamByName name puzzle cpustate = getStreamByName' StreamOutput name puzzle cpustate

getOutputStreamActualByPosX :: Int -> Puzzle -> Cpu l n -> Either String (Seq.Seq n)
getOutputStreamActualByPosX posX puzzle cpustate = getStreamByPosX' StreamOutput posX puzzle cpustate >>= getOutputStreamActualValues

getOutputStreamActualByName :: Text -> Puzzle -> Cpu l n -> Either String (Seq.Seq n)
getOutputStreamActualByName name puzzle cpustate = getStreamByName' StreamOutput name puzzle cpustate >>= getOutputStreamActualValues

getOutputStreamActualValues (OutputNode {outputNodeActual}) = Right outputNodeActual
getOutputStreamActualValues _ = Left "node at that index has an unexpected type"

getOutputStreamExpectedByPosX :: Int -> Puzzle -> Cpu l n -> Either String (Seq.Seq n)
getOutputStreamExpectedByPosX posX puzzle cpustate = getStreamByPosX' StreamOutput posX puzzle cpustate >>= getOutputStreamExpectedValues

getOutputStreamExpectedByName :: Text -> Puzzle -> Cpu l n -> Either String (Seq.Seq n)
getOutputStreamExpectedByName name puzzle cpustate = getStreamByName' StreamOutput name puzzle cpustate >>= getOutputStreamExpectedValues

getOutputStreamExpectedValues (OutputNode {outputNodeExpected}) = Right outputNodeExpected
getOutputStreamExpectedValues _ = Left "node at that index has an unexpected type"

getInputStreamGeneratorByName :: Text -> Puzzle -> Either String StreamGenerator
getInputStreamGeneratorByName name puzzle = getPuzzleStreamByName' StreamInput name puzzle >>= \(_, _, _, gen) -> pure gen

setInputStreamGeneratorByName :: Text -> StreamGenerator -> Puzzle -> Either String Puzzle
setInputStreamGeneratorByName name values = modifyStreamByName' StreamInput name (pure . fmap (const values))

setOutputStreamExpectedByName :: Text -> StreamGenerator -> Puzzle -> Either String Puzzle
setOutputStreamExpectedByName name values = modifyStreamByName' StreamOutput name (pure . fmap (const values))

seedForSpecAndTest :: Int -> Int -> Int
seedForSpecAndTest spec test = (100*spec + test - 1) `mod` (Data.Bits.shift 1 32)
