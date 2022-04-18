{-# LANGUAGE QuasiQuotes, OverloadedStrings, NamedFieldPuns, TupleSections #-}
module Tests.Debugger (debugTIS) where

import qualified Data.Array as A
import qualified Data.Foldable
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

import LazyTIS100.Prelude
import LazyTIS100.Parser
import LazyTIS100.EvalEager
import LazyTIS100.Trace
import Lib

import Graphics.Vty

data DebugState = DebugState { stepnum :: Int, done :: Bool,
    puzzle :: Puzzle, initialStreams :: Map.Map (StreamType, Int) (Seq.Seq Int),
    cpustate :: [Cpu Int Int],
    traceMsgs ::[[T.Text]] }

wrap :: Int -> Attr -> T.Text -> Image
wrap width attr t
    | width <= 0 = text' attr t
    | otherwise = case T.uncons t of
        Nothing -> emptyImage
        Just (x, xs) -> go width (T.pack [x]) xs
    where
        go width2 prefix t2 = case T.uncons t2 of
            Nothing -> text' attr t
            Just (x, xs) -> let width3 = width2 - wcwidth x in
                if width3 >= 0
                    then go width3 (T.snoc prefix x) xs
                    else text' attr prefix <-> wrap width attr t2

renderCpu :: DebugState -> Image
renderCpu DebugState {puzzle, cpustate=[]} = line0 <-> line1
    where
    line0 = text' defAttr $ "TIS-100 simulator: " <> puzzleName puzzle
    line1 = text' (defAttr `withForeColor`  red) $ "invalid state"
renderCpu DebugState {stepnum, done, puzzle, initialStreams, cpustate=cpustate:_, traceMsgs} = lines
    where
    line0 = text' defAttr $ "TIS-100 simulator: " <> puzzleName puzzle
    line1 = text' defAttr "left/right to step, R to restart, q to quit"
    line2 = text' defAttr $ "step " <> showT stepnum <> (if done then " DONE" else "")
    header = line1 <-> line2
    emptyLine = text' defAttr ""
    leftSide = header <-> emptyLine <-> (renderInputStreams <|> renderOutputStreams)
    lines = line0 <-> (leftSide <|> text defAttr "  " <|> renderNodes)

    renderInputStreams = horizCat $ execWriter $ mapM_ renderInputStream (puzzleStreams puzzle)
    renderInputStream :: (StreamType, T.Text, Int, StreamGenerator) -> Writer [Image] ()
    renderInputStream (StreamInput, name, posX, _) = tell [ header <-> nums <-> footer ]
        where
            header = text' defAttr ("IN." <> name <> " ") <-> text defAttr "+----+  "
            footer = text defAttr "+----+"
            initialNums = fromMaybe Seq.empty $ Map.lookup (StreamInput, posX) initialStreams
            nums = case getInputStreamByPosX posX puzzle cpustate of
                Left _ -> text' defAttr "|" <|> text' (defAttr `withForeColor` red) "n/a" <|> text' defAttr "|"
                Right xs -> let
                    numInitial = length initialNums - length xs
                    alreadyUsed = vertCat $ map (formatNumberInInputStream False) (Data.Foldable.toList $ Seq.take numInitial initialNums)
                    notYetUsed = vertCat $ map (uncurry formatNumberInInputStream) $ zip (True : repeat False) xs
                    in alreadyUsed <-> notYetUsed
    renderInputStream _ = pure ()

    formatNumber x
        | 0 <= x && x <= 9 =
            "  " <> showT x <> " "
        | -9 <= x && x <= -1 || 10 <= x && x <= 99 =
            " " <> showT x <> " "
        | -99 <= x && x <= -10 =
            "" <> showT x <> " "
        | 100 <= x && x <= 999 =
            "" <> showT x <> " "
        | otherwise =
            "" <> showT x <> ""
    formatNumberInInputStream current x = fmt $ formatNumber x
        where
            fmt inner = if current
                then text' defAttr ">" <|> text' (defAttr `withStyle` reverseVideo) inner <|> text' defAttr "|"
                else text' defAttr $ "|" <> inner <> "|"

    padZip :: [a] -> [b] -> [(Maybe a, Maybe b)]
    padZip [] [] = []
    padZip xs [] = map ((,Nothing) . Just) xs
    padZip [] ys = map ((Nothing,) . Just) ys
    padZip (x:xs) (y:ys) = (Just x, Just y) : padZip xs ys

    renderOutputStreams = horizCat $ execWriter $ mapM_ renderOutputStream (puzzleStreams puzzle)
    renderOutputStream :: (StreamType, T.Text, Int, StreamGenerator) -> Writer [Image] ()
    renderOutputStream (StreamOutput, name, posX, _) = tell [ header <-> nums <-> footer ]
        where
            header = text' defAttr ("OUT." <> name <> " ") <-> text defAttr "+----+----+  "
            footer = text defAttr "+----+----+"
            expectedNums = fromMaybe Seq.empty $ Map.lookup (StreamOutput, posX) initialStreams
            nums = case getOutputStreamByPosX posX puzzle cpustate of
                Right (OutputNode {outputNodeActual}) ->
                    vertCat $ map formatNumberInOutputStream $ padZip (Data.Foldable.toList expectedNums) (Data.Foldable.toList outputNodeActual)
                _ -> text' defAttr "|" <|> text' (defAttr `withForeColor` red) "   n/a   " <|> text' defAttr "|"
    renderOutputStream _ = pure ()

    formatNumberInOutputStream (Nothing, Nothing) = emptyImage
    formatNumberInOutputStream (xm, ym) = text' defAttr expected <|> actual <|> text' defAttr "|"
        where
            expected = case xm of
                Nothing -> "|    |"
                Just x -> "|" <> formatNumber x <> "|"
            actualText = case ym of
                Nothing -> "    "
                Just y -> formatNumber y
            actual = if xm == ym
                then text' defAttr actualText
                else text' (defAttr `withForeColor` red) actualText

    renderNodes = renderNodes2
        -- for debugging the debugger
        -- <|> wrap 100 defAttr (showT $ getModes cpustate)
        <|> ((vertCat $ map (wrap 100 defAttr . showT) $ getModes cpustate) <-> renderTrace)

    renderTrace = vertCat $ map (text' defAttr) $ concat $ take 1 traceMsgs

    ((minY, minX), (maxY, maxX)) = A.bounds cpustate
    renderNodes2 = vertCat $ flip map [minY..maxY] $ \y -> horizCat $ flip map [minX..maxX] $ \x ->
        let node = cpustate A.! (y, x)
            nodeRight = cpustate `at` (y, x+1)
            nodeDown = cpustate `at` (y+1, x)
        in addInterNodeRight node nodeRight $ addInterNodeDown node nodeDown $ renderNode (y, x) (cpustate A.! (y, x))

    padRight x = pad 0 0 x 0
    _padBottom x = pad 0 0 0 x

    nodeWidth = 26
    nodeHeight = 17

    renderNode (y, x) (InputNode xs) = name <|> arrow <|> value
        where
            findName [] = "IN"
            findName ((StreamInput, name, posX, _) : xs)
                | posX == x = "IN." <> name
                | otherwise = findName xs
            findName (_ : xs) = findName xs
            name = text defAttr "" <-> (resize (nodeWidth-6-2) 1 $ text' defAttr (findName (puzzleStreams puzzle)))

            arrow = padRight 1 $ text' defAttr "||" <-> text' defAttr "\\/"

            value = padRight 1 $ case xs of
                [] -> text' defAttr "    "
                (x : _) -> text' defAttr $ formatNumber x
    renderNode (y, x) (OutputNode {outputNodeActual}) =
        name <|> arrow <|> value
        where
            findName [] = "OUT"
            findName ((StreamOutput, name, posX, _) : xs)
                | posX == x = "OUT." <> name
                | otherwise = findName xs
            findName (_ : xs) = findName xs
            name = resize (nodeWidth-6-2) 2 $ text' defAttr (findName (puzzleStreams puzzle))

            arrow = padRight 1 $ text' defAttr "||" <-> text' defAttr "\\/"

            writtenValue = case cpustate `at` (y-1, x) of
                Just (ComputeNode _ state) -> maybeGetWrittenValue state DOWN
                _ -> Nothing
            value = padRight 2 $ case writtenValue of
                Nothing -> text' defAttr "    "
                Just x -> text' defAttr $ formatNumber x
    renderNode (y, x) BrokenNode
        | y == minY || y == maxY = resize nodeWidth 1 $ emptyImage
        | otherwise = vertCat $ replicate nodeHeight $ string (defAttr `withForeColor` blue) $ replicate nodeWidth 'X'
    renderNode (y, x) (ComputeNode prog state) = borderTB <-> (prog' <|> regs) <-> borderTB
        where
        borderTB = text' defAttr "+-------------------+----+"
        border = text' defAttr "|"
        prog' = resizeWidth (nodeWidth-6) $ (vertCat $ map renderProgLine $ zip activeLine $ A.elems prog) <-> emptyProgLines
        emptyProgLines = vertCat $ replicate (nodeHeight-2-(length $ A.elems prog)) border
        proglines = max (nodeHeight-2) (length $ A.elems prog)
        activeLine = replicate pc False <> [True] <> repeat False
        renderProgLine (False, instr) = text' defAttr $ "|" <> showT instr
        renderProgLine (True, instr) = let str = showT instr in
            text defAttr ">" <|> (text' (defAttr `withStyle` reverseVideo) $ str <> T.replicate (nodeWidth-6-T.length str) " ")
        NodeState {acc,bak,lastPort,pc,mode} = state
        regs = vertCat $
            [ border <|> text' (defAttr `withForeColor` blue) "ACC " <|> border
            , border <|> text' defAttr (formatNumber acc) <|> border
            , text' defAttr "+----+"
            , border <|> text' (defAttr `withForeColor` blue) "BAK " <|> border
            , border <|> text' defAttr (formatNumber bak) <|> border
            , text' defAttr "+----+"
            , border <|> text' (defAttr `withForeColor` blue) "LAST" <|> border
            , border <|> resizeWidth 4 (text' defAttr (showT lastPort)) <|> border
            , text' defAttr "+----+"
            , border <|> text' (defAttr `withForeColor` blue) "MODE" <|> border
            , border <|> text' defAttr (showMode mode) <|> border
            , text' defAttr "+----+" ]
            <> replicate (proglines-12) (text' defAttr "+    +")
        showMode IDLE = "IDLE"
        showMode RUN = "RUN "
        showMode FINISHED = "FIN "
        showMode ONFIRE = "FIRE"
        showMode (READ _) = "READ"
        showMode (HasRead _) = "READ"
        showMode (WRITE _ _) = "WRTE"
        showMode HasWritten = "WRTE"

    maybeGetWrittenValue NodeState {mode=WRITE dir1 x} dir2 | dir1 == dir2 = Just x
    maybeGetWrittenValue NodeState {mode=WRITE ANY x} _ = Just x
    maybeGetWrittenValue NodeState {mode=WRITE LAST x, lastPort=dir1} dir2 | dir1 == dir2 = Just x
    maybeGetWrittenValue _ _ = Nothing

    isReading NodeState {mode=READ dir1} dir2 | dir1 == dir2 = True
    isReading NodeState {mode=READ ANY} _ = True
    isReading NodeState {mode=READ LAST, lastPort=dir1} dir2 | dir1 == dir2 = True
    isReading _ _ = False

    addInterNodeRight node Nothing renderedNode = renderedNode
    addInterNodeRight (ComputeNode _ state1) (Just (ComputeNode _ state2)) renderedNode =
        renderedNode <|> comm
        where
            comm = emptyLines 5
                <-> text defAttr " -> " <-> formatComm LEFT RIGHT state1 state2 <-> emptyLines 3
                <-> text defAttr " <- " <-> formatComm RIGHT LEFT state2 state1
            emptyLines n = resize 1 n emptyImage
    addInterNodeRight node nodeRight renderedNode = padRight 4 renderedNode

    formatComm dir1 dir2 state1 state2 = case maybeGetWrittenValue state1 dir2 of
        Just x -> text' defAttr (formatNumber x)
        Nothing -> if isReading state2 dir1
            then text' defAttr " ?? "
            else text' defAttr "    "

    addInterNodeDown node Nothing renderedNode = renderedNode
    addInterNodeDown (ComputeNode _ state1) (Just (ComputeNode _ state2)) renderedNode =
        renderedNode <-> comm
        where
            comm = text defAttr "     /\\         ||" <|> formatComm UP DOWN state1 state2
                <-> text defAttr "     || " <|> formatComm DOWN UP state2 state1 <|> text defAttr "    \\/"
    addInterNodeDown node nodeDown renderedNode = renderedNode

stepBack :: DebugState -> DebugState
stepBack st@DebugState {stepnum, cpustate=(_ : prev@(_:_)), traceMsgs=(_ : prevtrace@(_:_))} = st { stepnum = stepnum-1, done = False, cpustate = prev, traceMsgs = prevtrace }
stepBack st = st

stepForward :: DebugState -> IO DebugState
stepForward st@DebugState {stepnum, done=False, cpustate=prevStates@(cpustate : _), traceMsgs} = do
    startTrace
    let (done, cpustate') = step cpustate
    newTraceMsgs <- done `seq` cpustate' `seq` restartTrace
    pure $ st { stepnum = stepnum+1, done, cpustate = cpustate' : prevStates, traceMsgs = newTraceMsgs : traceMsgs }
stepForward st = pure st

debugTIS :: Either String (Puzzle, Cpu Int Int) -> IO ()
debugTIS (Left msg) = putStrLn $ "Error: " <> msg
debugTIS (Right (puzzle, initialState)) = do
    cfg <- standardIOConfig
    vty <- mkVty cfg
    let initialStreams = execWriter $ mapM_ genInitialStream (puzzleStreams puzzle)
        genInitialStream :: (StreamType, T.Text, Int, StreamGenerator) -> Writer (Map.Map (StreamType, Int) (Seq.Seq Int)) ()
        genInitialStream (stype@StreamInput, _, posX, _) = case getInputStreamByPosX posX puzzle initialState of
            Left _ -> pure ()
            Right xs -> tell $ Map.singleton (stype, posX) (Seq.fromList xs)
        genInitialStream (stype@StreamOutput, _, posX, _) = case getOutputStreamByPosX posX puzzle initialState of
            Right (OutputNode {outputNodeExpected}) -> tell $ Map.singleton (stype, posX) outputNodeExpected
            _ -> pure ()
        genInitialStream (stype@StreamImage, _, posX, gen) = pure ()

        state0 = DebugState { stepnum = 0, done = False, puzzle, cpustate = [initialState], initialStreams, traceMsgs=[[]]}
        go lastEvent state = do
            let img = renderCpu state <-> string defAttr (show lastEvent)
                pic = picForImage img
            update vty pic
            ev <- nextEvent vty
            case ev of
                EvKey (KChar 'R') [] -> go (Just ev) state0
                EvKey (KChar 'q') _ -> pure ()
                EvKey (KChar 'Q') _ -> pure ()
                EvKey KLeft [] -> go (Just ev) (stepBack state)
                EvKey KRight [] -> go (Just ev) =<< stepForward state
                _ -> go (Just ev) state
    go Nothing state0 `finally` shutdown vty
