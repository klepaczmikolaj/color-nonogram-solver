{-# LANGUAGE DeriveDataTypeable #-}
module Main where
    import Data.List
    import System.Console.ANSI
    import Text.JSON.Generic
    import System.Environment

    -- Colors and display
    palette :: [Color]
    palette = [Black, Red, Green, Yellow, Blue, Magenta, Cyan, White] 

    putCharWithColor :: Char -> Color -> IO ()
    putCharWithColor x c = do
        setSGR [SetColor Background Vivid White]
        setSGR [SetColor Foreground Vivid c]
        putChar x
        setSGR [Reset]

    putIntWithColor :: Int -> Color -> IO ()
    putIntWithColor x c 
        | x > 0 && c /= Yellow && c /= Green = do
            setSGR [SetColor Background Vivid c]
            setSGR [SetColor Foreground Vivid White]
            putStr (show x)
            setSGR [Reset]
        | x > 0 && (c == Yellow || c == Green) = do
            setSGR [SetColor Background Vivid c]
            setSGR [SetColor Foreground Vivid Black]
            putStr (show x)
            setSGR [Reset]
        | otherwise = putStr " "

    putColorCell :: Color -> IO ()
    putColorCell c = do
        setSGR [SetColor Background Vivid c]
        setSGR [SetColor Foreground Vivid c]
        putChar 'x'
        setSGR [Reset]

    getCellColor :: Cell -> Color
    getCellColor (Cell _ color)     | color == "Black"      = Black
                                    | color == "Red"        = Red
                                    | color == "Green"      = Green
                                    | color == "Yellow"     = Yellow
                                    | color == "Blue"       = Blue
                                    | color == "Magenta"    = Magenta
                                    | color == "Cyan"       = Cyan
                                    | color == "White"      = White
                                    | otherwise = error "Wrong color"

    displayResult :: Grid -> Puzzle -> IO ()
    displayResult [] puzzle = do
        putStr "Inconsistent puzzle\n"
        displayResultLines init puzzle
        displayVertLines (getVerticalValue puzzle)
            where
                init = replicate sizeHoriz (replicate sizeVert White)
                sizeHoriz = getPuzzleSizeHoriz puzzle
                sizeVert = getPuzzleSizeVert puzzle
    displayResult res puzzle = do
        putStr "\n"
        displayResultLines res puzzle
        displayVertLines (getVerticalValue puzzle)

    displayResultLines :: Grid -> Puzzle -> IO ()
    displayResultLines [] puzzle = putChar '\n'
    displayResultLines (x:xs) (Puzzle (hor:hTail) vert) = do
        putSingleLine x
        displayLine hor
        displayResultLines xs (Puzzle hTail vert)
            where
                putSingleLine [] = putChar ' '
                putSingleLine (y:ys) = do
                    if y == Cyan 
                        then do putCharWithColor '\\' Cyan
                                putCharWithColor '/' Cyan
                        else do putColorCell y
                                putColorCell y
                    putSingleLine ys

    displayLine :: [Cell] -> IO ()
    displayLine [] = putChar '\n'
    displayLine (x:xs) = do
        putIntWithColor (getCellValue x) (getCellColor x)
        putChar ' '
        displayLine xs
    
    displayVertLines :: [[Cell]] -> IO ()
    displayVertLines lines = displayVertLinesPrep linesPrepared
        where
            maxLen = maximum (map length lines)
            linesPrepared = transpose [x ++ (replicate (maxLen - (length x)) (Cell {v=0, c="Black"})) | x <- lines]
            displayVertLinesPrep [] = putChar '\n'
            displayVertLinesPrep (x:xs) = do
                displayVertLine x
                displayVertLinesPrep xs

    displayVertLine :: [Cell] -> IO ()
    displayVertLine [] = putChar '\n'
    displayVertLine (x:xs) = do
        putIntWithColor (getCellValue x) (getCellColor x)
        if (getCellValue x) >= 10 then
            displayVertLine xs
            else do
                putChar ' '
                displayVertLine xs

    -- Data structures and types
    type Row = [Color]
    type Grid = [Row]
    
    -- deriving data, typeable for decodeJson
    data Cell = Cell
        { v     :: Int
        , c     :: String
        } deriving (Data, Typeable)
    
    data Puzzle = Puzzle
        { horizontal    :: [[Cell]]
        , vertical      :: [[Cell]]
        } deriving (Data, Typeable)
    
    --utils
    getPuzzleSizeHoriz :: Puzzle -> Int
    getPuzzleSizeHoriz (Puzzle hor _) = length hor

    getPuzzleSizeVert :: Puzzle -> Int
    getPuzzleSizeVert (Puzzle _ ver) = length ver

    getCellValue :: Cell -> Int
    getCellValue (Cell value _) = value
    
    getVerticalValue :: Puzzle -> [[Cell]]
    getVerticalValue (Puzzle _ ver) = ver

    getHorisontalValue :: Puzzle -> [[Cell]]
    getHorisontalValue (Puzzle hor _) = hor

    --nub - distinct
    getRowColorList :: [Cell] -> [Color]
    getRowColorList list = nub (getCellListColors list)
        where
            getCellListColors [] = []
            getCellListColors (x:xs) = (getCellColor x) : getCellListColors xs

    -- puzzle solution logic
    -- guessing cells that are not deduced n should start from 0
    solvePuzzleGuess :: Grid -> Puzzle -> Int -> Grid
    solvePuzzleGuess inputResult (Puzzle puzHoriz puzVert) n
        | length inputResult == n   = inputResult
        | fittingResults == []      = error "pusta lista mozliwych rozwiązan"
        | length fittingResults > 1 = solvePuzzleGuess inputResult (Puzzle puzHoriz puzVert) (n + 1)
        | otherwise                 = solvePuzzleGuess (head fittingResults) (Puzzle puzHoriz puzVert) (n + 1)
        where
            (front, resRow : back) = splitAt n inputResult
            (frontPuz, puzRow : backPuz) = splitAt n puzHoriz
            rowMatches = getListOfRowMatches puzRow resRow
            resList = [front ++ (match : back) | match <- rowMatches]
            fittingResults = checkResultsVert resList puzVert

    checkResultsVert :: [Grid] -> [[Cell]] -> [Grid]
    checkResultsVert [] _ = []
    checkResultsVert (res:resTail) puzVert
        | checkRes (transpose res) puzVert    = res : (checkResultsVert resTail puzVert)
        | otherwise = (checkResultsVert resTail puzVert)
        where
            checkRes [] [] = True
            checkRes (row:rowTail) (puz:puzTail) 
                | (getListOfRowMatches puz row) == [] = False
                | otherwise = checkRes rowTail puzTail

    -- deduction logic
    -- Cyan is considered as 'X' character, White is a cell that is not yet filled
    solvePuzzleDeduction :: Puzzle -> Grid
    solvePuzzleDeduction puzzle = converge makeDeduction init puzzle
        where
            init = replicate sizeHoriz (replicate sizeVert White)
            sizeHoriz = getPuzzleSizeHoriz puzzle
            sizeVert = getPuzzleSizeVert puzzle

    converge :: (Grid -> Puzzle -> Grid) -> Grid -> Puzzle -> Grid
    converge f arg arg2
        | (f arg arg2) == arg = arg
        | otherwise = converge f (f arg arg2) arg2

    makeDeduction :: Grid -> Puzzle -> Grid
    makeDeduction inputResult (Puzzle puzHoriz puzVert) = transpose (zipWith (\res puz -> deduceRow res puz) (transpose horizResult) puzVert)
        where
            horizResult = zipWith (\res puz -> deduceRow res puz) inputResult puzHoriz

    deduceRow :: Row -> [Cell] -> Row
    deduceRow inputGridRow puzzleRow = getRowListCommon (getListOfRowMatches puzzleRow inputGridRow)

    -- generate all possibilities of color placement in a result row based on
    -- current filled row cells and puzzle conditions for that row
    -- Cyan is an X cell, White is a not filled cell
    getListOfRowMatches :: [Cell] -> Row -> [Row]
    getListOfRowMatches [] [] = [[]]
    getListOfRowMatches puzzleRow [] = []
    getListOfRowMatches puzzleRow (White:inputRowTail) = getListOfMatchesColor puzzleRow Cyan inputRowTail ++ 
        concat [getListOfMatchesColor puzzleRow color inputRowTail |
                color <- getRowColorList puzzleRow] 
    getListOfRowMatches puzzleRow (color:inputRowTail) = getListOfMatchesColor puzzleRow color inputRowTail

    getListOfMatchesColor :: [Cell] -> Color -> Row -> [Row]
    getListOfMatchesColor conds Cyan inputRowTail = [Cyan : row | row <- getListOfRowMatches conds inputRowTail]

    --only one condition for row
    getListOfMatchesColor [cond] color inputRowTail
            | condColor == color    = [replicate condVal color ++ replicate (tailLen + 1 - condVal) Cyan |
                (tailLen + 1) >= condVal && all (\x -> (x == White || x == color)) front && all (/=color) back]
            | otherwise     = []
        where
            (front, back) = splitAt (condVal-1) inputRowTail
            condVal = getCellValue cond
            condColor = getCellColor cond
            tailLen = length inputRowTail

    --more than one condition for row
    getListOfMatchesColor (cond:cond2:condTail) color inputRowTail
            | condColor == color && condColor == cond2Color     = [replicate condVal color ++ Cyan : row |
                tailLen > condVal && all (\x -> (x == White || x == color)) front && (space == White || space == Cyan),
                row <- getListOfRowMatches (cond2:condTail) back]
            | condColor == color && condColor /= cond2Color     = [replicate condVal color ++ row |
                (tailLen + 1) > condVal && all (\x -> (x == White || x == color)) front && space /= color,
                row <- getListOfRowMatches (cond2:condTail) (space:back)]
            | otherwise = []
        where
            (front, space : back) = splitAt (condVal-1) inputRowTail
            condVal = getCellValue cond
            condColor = getCellColor cond
            cond2Val = getCellValue cond2
            cond2Color = getCellColor cond2
            tailLen = length inputRowTail

    getRowListCommon :: [Row] -> Row
    getRowListCommon [] = []
    getRowListCommon [x] = x
    getRowListCommon [x, y] = getRowPairCommon x y
    getRowListCommon (x:y:ys) = getRowListCommon ((getRowPairCommon x y) : ys)

    getRowPairCommon :: Row -> Row -> Row
    getRowPairCommon [] [] = []
    getRowPairCommon (x:xtail) (y:ytail)
        | (length xtail) /= (length ytail) = error "rows lengths not equal"
        | x == y    = x : (getRowPairCommon xtail ytail)
        | otherwise = White : (getRowPairCommon xtail ytail)
    
    -- Main 
    main = do
        args <- getArgs
        inputPuzzle <- readFile (head args)
        let puzzle = (decodeJSON inputPuzzle :: Puzzle)
        let solDeduction = solvePuzzleDeduction puzzle
        let solution = solvePuzzleGuess solDeduction puzzle 0
        displayResult solution puzzle
