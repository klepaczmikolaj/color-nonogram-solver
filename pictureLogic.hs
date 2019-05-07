{-# LANGUAGE DeriveDataTypeable #-}
module Main where
    import Data.List
    import System.Console.ANSI
    import Text.JSON.Generic

    -- Colors and display
    palette :: Row
    palette = [Black, Red, Green, Yellow, Blue, Magenta, Cyan, White] 

    putCharWithColor :: Char -> Color -> IO ()
    putCharWithColor x c = do
        setSGR [SetColor Background Vivid White]
        setSGR [SetColor Foreground Vivid c]
        putChar x
        setSGR [Reset]

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

    displayResult :: Grid -> IO ()
    displayResult [] = putChar '\n'
    displayResult (x:xs) = do
        putSingleLine x
        displayResult xs
            where
                putSingleLine [] = putChar '\n'
                putSingleLine (y:ys) = do
                    if y == Cyan 
                        then do putCharWithColor 'X' Cyan
                                --putCharWithColor 'X' Cyan
                        else do putColorCell y
                               --putColorCell y
                    
                    putSingleLine ys

    -- Data structures
    type Row = [Color]
    type Grid = [Row]

    data Cell = Cell
        { v     :: Int
        , c     :: String
        } deriving (Show, Data, Typeable)
    
    data Puzzle = Puzzle
        { horizontal    :: [[Cell]]
        , vertical      :: [[Cell]]
        } deriving (Show, Data, Typeable)

    --data ResultCell = Empty | Color deriving (Show, Data, Typeable)
    
    --utils
    
    getPuzzleSizeHoriz :: Puzzle -> Int
    getPuzzleSizeHoriz (Puzzle hor _) = length hor

    getPuzzleSizeVert :: Puzzle -> Int
    getPuzzleSizeVert (Puzzle _ ver) = length ver

    getCellValue :: Cell -> Int
    getCellValue (Cell value _) = value
    
    getHorisontalValue :: Puzzle -> [[Cell]]
    getHorisontalValue (Puzzle hor _) = hor
    
    getVerticalValue :: Puzzle -> [[Cell]]
    getVerticalValue (Puzzle _ ver) = ver

    --nub - distinct
    getRowColorList :: [Cell] -> Row
    getRowColorList list = nub (getCellListColors list)
        where
            getCellListColors [] = []
            getCellListColors (x:xs) = (getCellColor x) : getCellListColors xs

    getColorList :: Puzzle -> Row
    getColorList (Puzzle hor vert) = nub (getRowColorList cells)
        where
            cells = (concat hor) ++ (concat vert)

    accessHorizontalRow :: Puzzle -> Int -> [Cell]
    accessHorizontalRow (Puzzle hor _) id = hor !! id

    accessVerticalRow :: Puzzle -> Int -> [Cell]
    accessVerticalRow (Puzzle _ vert) id = vert !! id

    modifyResultCell :: Int -> Int -> Color -> Grid ->  Grid
    modifyResultCell row column value result = replaceN row (replaceN column value (result !! row) ) result
    replaceN _ _ [] = []
    replaceN n value (x:xs)
      | n == 0 = value:xs
      | otherwise = x:replaceN (n-1) value xs

    modifyResultRange :: Int -> Int -> Int -> Color -> Grid ->  Grid
    modifyResultRange row columnStart count value result
        |   count == 1 = modifyResultCell row columnStart value result
        |   count > 1  = modifyResultCell row columnStart value (modifyResultRange row (columnStart + 1) (count - 1)value result)
        |   otherwise  = error "Wrong count value"

    -- algorithm logic
    -- Cyan is considered as 'X' character, White is a cell that is not yet filled
    solvePuzzle :: Puzzle -> Grid
    solvePuzzle puzzle = converge makeDeduction init puzzle
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
                                                            getListOfMatchesColor puzzleRow Red inputRowTail
    getListOfRowMatches puzzleRow (color:inputRowTail) = getListOfMatchesColor puzzleRow color inputRowTail

    getListOfMatchesColor :: [Cell] -> Color -> Row -> [Row]
    getListOfMatchesColor conds Cyan inputRowTail = [Cyan : row | row <- getListOfRowMatches conds inputRowTail]

    --only one condition for row
    getListOfMatchesColor [cond] Red inputRowTail = [replicate condVal Red ++ replicate (tailLen + 1 - condVal) Cyan |
            (tailLen + 1) >= condVal && all (/=Cyan) front && all (/=Red) back]
        where
            (front, back) = splitAt (condVal-1) inputRowTail
            condVal = getCellValue cond
            tailLen = length inputRowTail

    --more than one condition for row
    getListOfMatchesColor (cond:condTail) Red inputRowTail = [replicate condVal Red ++ Cyan : row |
            tailLen > condVal && all (/=Cyan) front && space /= Red,
            row <- getListOfRowMatches condTail back]
        where
            (front, space : back) = splitAt (condVal-1) inputRowTail
            condVal = getCellValue cond
            tailLen = length inputRowTail

    getRowListCommon :: [Row] -> Row
    getRowListCommon [] = error "common row list empty"
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
        inputPuzzle <- readFile "puzzleSingleColor.json"
        let puzzle = (decodeJSON inputPuzzle :: Puzzle)
        let sol = solvePuzzle puzzle
        print sol
        displayResult sol
        print puzzle

        -- testing
        --let row1 = [White, Red, Red, White, Cyan, Cyan]
        --let row2 = [White, White, Red, White, Cyan, White]
        --let row3 = [White, Red, Red, White, Red, White]
        --print (getRowPairCommon row1 row2)
        --print (getRowListCommon [row1, row2, row3])

        