{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module LazyTIS100.Parser where

import Prelude hiding (takeWhile)

import Control.Applicative
import Control.Monad (when)

import Data.Attoparsec.Text

import qualified Data.Array as A
import Data.Array (Array, (//))
import Data.Char (isSpace)
import qualified Data.Set as Set
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T

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
    readsPrec _ x = case parseOnly puzzleParser (T.pack x) of
        Left msg -> error msg
        Right x' -> [(x',"")]

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
    streams'' <- matchStreams "output" streams [StreamOutput, StreamImage] outputLine

    pure $ Puzzle hline description streams tileArray
    where
        headerline = do
            name <- (string "- " <?> "start") >> takeWhile1 (not . isEndOfLine) <* endOfLine
            let name' = T.dropWhile isSpace name
            when (T.takeEnd 2 name' /= " -") $ fail "expecting \" -\" after name"
            pure $ T.dropEnd 2 name'
        descriptionBulletPoint = do
            string "> "
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
            inputLine <- option [] $ ((((string "." >> pure False) <|> (asciiCI "I" >> pure True)) `sepBy` skipSpaceInLine) <* endOfLine <?> "input line")
            let tileType =
                    ((asciiCI "X" <|> string "-") >> pure TileDamaged)
                    <|> (asciiCI "C" >> pure TileCompute)
                    <|> (asciiCI "M" >> pure TileMemory)
                    <?> "tile type"
            tileLines <- many1 $ skipSpaceInLine *> ((tileType `sepBy` skipSpaceInLine) <* skipSpaceInLine <* endOfLine <?> "layout line")
            outputLine <- option [] $ ((((string "." >> pure False) <|> (asciiCI "O" >> pure True)) `sepBy` skipSpaceInLine) <* endOfLine <?> "output line")

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
