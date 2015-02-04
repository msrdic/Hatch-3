module Board ( Board
             , mkBoard
             , step, mark, fill) where

import Data.List

data Cell = Cell { value :: Int
                 , cleared :: Bool
                 }

data Board = Board { cells :: [[Cell]] }

mkBoard :: [[Int]] -> Board
mkBoard cs = Board { cells = makeCells cs }
  where makeCells cs = map (map mkCell) cs
        mkCell c = Cell { value = c, cleared = False }

instance Show Cell where
  show cell = if cleared cell then "(" ++ (show $ value cell) ++ ")" else " " ++ (show $ value cell) ++ " "

instance Eq Cell where
  c1 == c2 = value c1 == value c2

instance Show Board where
  show b = concat $ intersperse "\n" $ (map show (cells b))

-- aliases for better documentation
type Height = Int
type Width = Int

-- | Do a single step through board.
step :: Board -> Board
step board = step' minChainLen board

minChainLen = 3

step' :: Int -> Board -> Board
step' len = fill . mark len

mark :: Int -> Board -> Board
mark len b = Board { cells = transpose . markChains len . transpose . markChains len $ cells b }

fill :: Board -> Board
fill b = Board { cells = transpose . map fillRow . transpose $ cells b }

fillRow :: [Cell] -> [Cell]
fillRow cells = replicate taken Cell { value = 1, cleared = False } ++ notClearedCells
  where taken = length $ filter cleared cells
        notClearedCells = filter (not . cleared) cells

markChains :: Int -> [[Cell]] -> [[Cell]]
markChains len rows = map (markRowChains len) rows

markRowChains :: Int -> [Cell] -> [Cell]
markRowChains len cells = concat $ markChains' len (group cells)

markChains' :: Int -> [[Cell]] -> [[Cell]]
markChains' len [] = []
markChains' len (c:cs)
  | length c < len = c : markChains' len cs
  | otherwise      = map clearCell c : markChains' len cs
  where clearCell c = c { cleared = True }