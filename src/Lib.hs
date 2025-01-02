module Lib where
import qualified Data.Map.Strict as Map
import Control.Monad.Trans.State.Strict
import System.Random

data Status = Empty |
              Marked|
              Clear deriving Show

data GameStatus = Won |
                  Failed |
                  Playing deriving Show

type Grid = Map.Map (Int,Int) Cell
data Cell = Cell {status::Status,mines::Int} deriving Show
type GameInfo = (Int,GameStatus)
type GameState = State GameInfo Grid 

toggleMarkDisplay :: Status -> Status
toggleMarkDisplay Marked = Clear
toggleMarkDisplay _ = Marked

initGameState :: RandomGen g => Grid -> Int -> g -> GameState
initGameState grid size gen = 
  do
    let (newGen,mineGrid) = Map.mapAccum addMine gen grid
    let final = setMineProximities mineGrid (0,0) size
    let numMines = foldr (\a b -> if (mines a == (-1)) then b+1 else b) 0 mineGrid
    --setStdGen $ fst mineGrid
    put (numMines,Playing)
    return (final)

calcDrawPos :: (Int,Int) -> Double -> (Double,Double)
calcDrawPos (x,y) step = (dx,dy) where dx = (fromIntegral x) * step
                                       dy = (fromIntegral y) * step

findCellPos :: (Double,Double) -> Double -> (Int,Int)
findCellPos (x,y) size = (floor $ 20 * (x / size) ,floor $ 20 * (y / size))

positions :: Int -> Int -> [((Int,Int),Cell)] 
positions _ 0 = []
positions size n = (rowPositions size (size-n)) `mappend` (positions size (n-1))

rowPositions :: Int -> Int -> [((Int,Int),Cell)]
rowPositions n x = take n $ iterate (\((x,y),c) -> ((x,y+1),c)) ((x,0),cell Empty 0)

grid :: Int -> Grid
grid size = Map.fromList (positions size size)

cell :: Status -> Int -> Cell
cell s m = Cell {status=s,mines=m}

addMine :: RandomGen g => g -> Cell -> (g,Cell)
addMine gen c = if (val <= 0.9) then (newGen,cell Empty 0) else (newGen,cell Empty (-1)) where (val,newGen) = uniformR (0::Float,1::Float) gen

setMineProximities :: Grid -> (Int,Int) -> Int -> Grid
setMineProximities grid (x,y) size 
  | (x == size) = setMineProximities grid (0,y+1) size
  | (y == size) = grid
  | otherwise = setMineProximities (setMineProximity grid (x,y)) (x+1,y) size

setMineProximity grid (x,y) = 
  case (Map.lookup (x,y) grid) of
    Just Cell{mines = -1} -> grid
    Just _ -> let numMines = foldr getMines 0 (getNeighbouringCells grid (x,y))
                  in Map.adjust (\c -> c{mines=numMines}) (x,y) grid
    Nothing -> grid

getMines :: Maybe Cell -> Int -> Int
getMines (Just c) a = if (mines c == (-1)) then a+1 else a
getMines Nothing a = a

getNeighbouringCells :: Grid -> (Int,Int) -> [Maybe Cell]
getNeighbouringCells grid (x,y) =   
  let c1 = Map.lookup (x+1,y) grid
      c2 = Map.lookup (x-1,y) grid
      c3 = Map.lookup (x,y+1) grid
      c4 = Map.lookup (x,y-1) grid
      c5 = Map.lookup (x+1,y+1) grid
      c6 = Map.lookup (x+1,y-1) grid
      c7 = Map.lookup (x-1,y+1) grid
      c8 = Map.lookup (x-1,y-1) grid
      in [c1,c2,c3,c4,c5,c6,c7,c8] --[(x+1,y),(x-1,y),(x,y+1),(x,y-1),(x+1,y+1),(x+1,y-1),(x-1,y+1),(x-1,y-1)]

updateGameState :: Grid -> (Int,Int) -> Status -> GameState
updateGameState gr pos s = evalMove pos s (Map.lookup pos gr) gr

evalMove :: (Int,Int) -> Status -> Maybe Cell -> Grid -> GameState
evalMove pos Marked (Just c) g = 
  do
    gm <- toggleMark pos Marked c g
    (m,_) <- get
    if m == 0 then 
      do 
        put (0,Won) 
        return gm 
    else return gm

evalMove pos Clear (Just c) g = 
  do
    case (c) of
      Cell{status=Marked} -> return g
      Cell{mines=(-1)} ->
        do
          put (0,Failed)
          return g
      Cell{status=Empty} -> clearFromPos pos g (Just c)
      _ -> return g
        
evalMove _ _ (Nothing) g = return g
    
clearFromPos :: (Int,Int) -> Grid -> Maybe Cell -> GameState
clearFromPos _ g Nothing = return g
clearFromPos pos g (Just Cell{status=Clear}) = return g
clearFromPos (x,y) g (Just c) = 
  do
    if ((mines c) == 0) then
      do
        a <- clearCell (x,y) g
        b <- clearFromPos (x+1,y) a (Map.lookup (x+1,y) a)
        cg <- clearFromPos (x,y+1) b (Map.lookup (x,y+1) b)
        d <- clearFromPos (x-1,y) cg (Map.lookup (x-1,y) cg)
        res <- clearFromPos (x-1,y) d (Map.lookup (x,y-1) d)
        return res
    else return g
    

clearCell :: (Int,Int) -> Grid -> GameState
clearCell pos g = 
  do
    let gn = Map.adjust (\c -> c{status=Clear}) pos g
    return gn


toggleMark :: (Int,Int) -> Status -> Cell -> Grid -> GameState
toggleMark pos s c g = 
  do
    (ms,st) <- get
    case (c) of
      Cell{status=Clear} -> return g
      Cell{status=Marked} ->
        do
          if (mines c == (-1)) then 
            do
              put (ms+1,st)
              let gn = Map.adjust (\c -> c{status=Empty}) pos g
              return gn
          else
            do
              let gn = Map.adjust (\c -> c{status=Empty}) pos g
              return gn
      Cell{status=Empty} ->
        do
          if (mines c == (-1)) then 
            do
              put (ms-1,st)
              let gn = Map.adjust (\c -> c{status=Marked}) pos g
              return gn
          else
            do
              let gn = Map.adjust (\c -> c{status=Marked}) pos g
              return gn
                              