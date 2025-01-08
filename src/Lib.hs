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
initGameState g size gen = 
  do
    let (_,mineGrid) = Map.mapAccum addMine gen g
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
rowPositions n xp = take n $ iterate (\((x,y),c) -> ((x,y+1),c)) ((xp,0),cell Empty 0)

grid :: Int -> Grid
grid size = Map.fromList (positions size size)

cell :: Status -> Int -> Cell
cell s m = Cell {status=s,mines=m}

addMine :: RandomGen g => g -> Cell -> (g,Cell)
addMine gen _ = if (val <= 0.9) then (newGen,cell Empty 0) else (newGen,cell Empty (-1)) where (val,newGen) = uniformR (0::Float,1::Float) gen

setMineProximities :: Grid -> (Int,Int) -> Int -> Grid
setMineProximities g (x,y) size 
  | (x == size) = setMineProximities g (0,y+1) size
  | (y == size) = g
  | otherwise = setMineProximities (setMineProximity g (x,y)) (x+1,y) size

setMineProximity :: Grid -> (Int, Int) -> Grid
setMineProximity g (x,y) = 
  case (Map.lookup (x,y) g) of
    Just Cell{mines = -1} -> g
    Just _ -> let numMines = foldr getMines 0 (getNeighbouringCells g (x,y))
                  in Map.adjust (\c -> c{mines=numMines}) (x,y) g
    Nothing -> g

getMines :: Maybe Cell -> Int -> Int
getMines (Just c) a = if (mines c == (-1)) then a+1 else a
getMines Nothing a = a

getNeighbouringCells :: Grid -> (Int,Int) -> [Maybe Cell]
getNeighbouringCells g (x,y) =   
  let c1 = Map.lookup (x+1,y) g
      c2 = Map.lookup (x-1,y) g
      c3 = Map.lookup (x,y+1) g
      c4 = Map.lookup (x,y-1) g
      c5 = Map.lookup (x+1,y+1) g
      c6 = Map.lookup (x+1,y-1) g
      c7 = Map.lookup (x-1,y+1) g
      c8 = Map.lookup (x-1,y-1) g
      in [c1,c2,c3,c4,c5,c6,c7,c8] --[(x+1,y),(x-1,y),(x,y+1),(x,y-1),(x+1,y+1),(x+1,y-1),(x-1,y+1),(x-1,y-1)]

getAdjacentCells :: Grid -> (Int,Int) -> [(Maybe Cell,(Int,Int))]
getAdjacentCells g (x,y) =   
  let c1 = Map.lookup (x+1,y) g
      c2 = Map.lookup (x-1,y) g
      c3 = Map.lookup (x,y+1) g
      c4 = Map.lookup (x,y-1) g
      in [(c1,(x+1,y)),(c2,(x-1,y)),(c3,(x,y+1)),(c4,(x,y-1))]

updateGameState :: Grid -> (Int,Int) -> Status -> GameState
updateGameState gr _ Empty = return gr
updateGameState gr pos s = evalMove pos s (Map.lookup pos gr) gr

evalMove :: (Int,Int) -> Status -> Maybe Cell -> Grid -> GameState
evalMove pos Marked (Just c) g = toggleMark pos c g
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
          checkWinCon gn
          
      Cell{status=Empty} ->
        do  
          gn <- clearCell pos g
          checkWinCon gn

      _ -> return g
        
evalMove _ _ _ g = return g

checkWinCon :: Grid -> GameState
checkWinCon g = 
  do
    (cl,_) <- get
    if cl == 0 then
      do
        put (cl,Won)
        return g
    else return g

accumUnclearedCells :: Cell -> Int -> Int
accumUnclearedCells Cell{status=Empty,mines=m} b = if (m /= (-1)) then b+1 else b
accumUnclearedCells _ b = b 
    
clearFromPos :: (Int,Int) -> Grid -> Maybe Cell -> GameState
clearFromPos _ g Nothing = return g
clearFromPos _ g (Just Cell{status=Clear}) = return g
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

toggleMark :: (Int,Int)  -> Cell -> Grid -> GameState
toggleMark pos ce g = 
  do
    (ms,st) <- get
    case (ce) of
      Cell{status=Clear} -> return g
      Cell{status=Marked} ->
        do
          if (mines ce == (-1)) then 
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
          if (mines ce == (-1)) then 
            do
              put (ms-1,st)
              let gn = Map.adjust (\c -> c{status=Marked}) pos g
              return gn
          else
            do
              let gn = Map.adjust (\c -> c{status=Marked}) pos g
              return gn
                              

type Strategy = Grid -> (Int,Int) -> Maybe [Solution]
type Solution = (Int,Int)
strategies :: [Strategy]
strategies = [adjacencyStrat]

findNextSafeMoves :: Grid -> Maybe [Solution]
findNextSafeMoves g = 
  let (ks,_) = unzip $ Map.toList g in map (\k -> applyStrategies g k strategies) ks

applyStrategies :: Grid -> (Int,Int) -> [Strategy] -> Maybe Solution
applyStrategies _ _ [] = Nothing
applyStrategies g (0,0) (st:sts) = 
  case (st g (0,0)) of
    Just s -> Just s
    Nothing -> applyStrategies g (0,0) sts 
applyStrategies _ _ _ = Nothing

testStrat :: Strategy
testStrat _ _ = let (i,gn) = uniformR (0,19) (mkStdGen 201223131)
                    (i',_) = uniformR (0,19) gn in Just (i,i')
      