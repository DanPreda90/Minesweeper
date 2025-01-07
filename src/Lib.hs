module Lib where
import qualified Data.Map.Strict as Map
import Control.Monad.Trans.State.Lazy
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
    let numUncleared = foldr accumUnclearedCells 0 mineGrid
    put (numUncleared,Playing)
    return final

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

getAdjacentCells :: Grid -> (Int,Int) -> [(Maybe Cell,(Int,Int))]
getAdjacentCells grid (x,y) =   
  let c1 = Map.lookup (x+1,y) grid
      c2 = Map.lookup (x-1,y) grid
      c3 = Map.lookup (x,y+1) grid
      c4 = Map.lookup (x,y-1) grid
      in [(c1,(x+1,y)),(c2,(x-1,y)),(c3,(x,y+1)),(c4,(x,y-1))]

updateGameState :: Grid -> (Int,Int) -> Status -> GameState
updateGameState gr pos s = evalMove pos s (Map.lookup pos gr) gr

evalMove :: (Int,Int) -> Status -> Maybe Cell -> Grid -> GameState
evalMove pos Marked (Just c) g = toggleMark pos Marked c g
evalMove pos Clear (Just c) g = 
  do
    case (c) of
      Cell{status=Marked} -> return g
      Cell{mines=(-1)} -> do
        put (0,Failed)
        return g
      Cell{status=Empty,mines=0} -> 
        do
          gn <- clearFromPos pos g (Just c)
          (cl,s) <- get
          if cl == 0 then
            do
              put (cl,Won)
              return gn
          else return gn
      Cell{status=Empty} ->
        do  
          gn <- clearCell pos g
          (cl,s) <- get
          if cl == 0 then
            do
              put (cl,Won)
              return gn
          else return gn
      _ -> return g
        
evalMove _ _ _ g = return g

accumUnclearedCells :: Cell -> Int -> Int
accumUnclearedCells Cell{status=Empty,mines=m} b = if (m /= (-1)) then b+1 else b
accumUnclearedCells _ b = b 
    
clearFromPos :: (Int,Int) -> Grid -> Maybe Cell -> GameState
clearFromPos _ g Nothing = return g
clearFromPos pos g (Just Cell{status=Clear}) = return g
clearFromPos (x,y) g (Just c) = 
  do
    if ((mines c) == 0) then
      do
        gc <- clearCell (x,y) g
        let mp = Map.singleton (x,y) True
        res <- bfs gc (filt (getAdjacentCells gc (x,y))) mp
        return res
    else return g

bfs :: Grid -> [(Cell,(Int,Int))] -> Map.Map (Int,Int) Bool -> GameState
bfs g [] _ = return g  
bfs g ((Cell{status=Empty,mines=m},pos):cs) mp =
  do
    let lkup = mp Map.!? pos
    if (m > (-1) && lkup == Nothing) then
      do
        let nmp = Map.insert pos True mp
        gn <- clearCell pos g
        if m == 0 then
          do
            let frontier = cs ++ filt (getAdjacentCells gn pos)
            bfs gn frontier nmp
        else bfs gn cs nmp
    else bfs g cs mp
bfs g (_:cs) dp = bfs g cs dp

filt :: [(Maybe Cell,(Int,Int))] -> [(Cell,(Int,Int))]
filt [] = []
filt ((Nothing,_):cs) = filt cs 
filt (((Just c),p):cs) = [(c,p)] ++ filt cs 

clearCell :: (Int,Int) -> Grid -> GameState
clearCell pos g = 
  do
    (cl,s) <- get
    let gn = Map.adjust (\c -> c{status=Clear}) pos g
    put (cl-1,s)
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
                              


                              