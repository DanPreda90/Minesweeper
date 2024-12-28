module Main (main) where

import Lib

import Data.IORef
import qualified Data.Map.Strict as Map
import Control.Monad.IO.Class
import qualified Graphics.UI.Threepenny as UI

data Status = Empty |
              Mine  |
              Clear deriving Show

data Cell = Cell {status::Status,marked::Bool} deriving Show

type Grid = Map.Map (Int,Int) Cell

positions :: Int -> Int -> [((Int,Int),Cell)]
positions _ 0 = []
positions size n = (rowPositions size (size-n)) `mappend` (positions size (n-1))

rowPositions :: Int -> Int -> [((Int,Int),Cell)]
rowPositions n x = take n $ iterate (\((x,y),c) -> ((x,y+1),c)) ((x,0),cell Empty)

grid :: Int -> Grid
grid size = Map.fromList (positions size size)

cell :: Status -> Cell
cell s = Cell {status=s,marked=False}

main :: IO ()
main = do
  UI.startGUI UI.defaultConfig setup

setup window = do
  let size = 20
  let cellSize = 20
  let step = 20
  canv <- UI.canvas
    UI.# UI.set UI.height 700
    UI.# UI.set UI.width 700
    UI.# UI.set UI.style [("border","solid black 1px"),("background","#eee")]

  g <- liftIO $ newIORef (grid size)
  currentPosition <- liftIO $ newIORef (0,0)

  UI.on UI.mousemove canv $ \xy ->
    do liftIO $ writeIORef currentPosition xy

  UI.on UI.click canv $ \_ ->
    do pos <- liftIO $ readIORef currentPosition
       gr <- liftIO $ readIORef g
       let (x,y) = findCellPos pos (size*cellSize)
       let grNew = updateGrid gr (x,y)
       liftIO $ writeIORef g grNew
       drawGrid canv step (Map.assocs grNew)
       return ()

  do
    gr <- liftIO $ readIORef g
    drawGrid canv step (Map.assocs gr)
    return ()

  UI.getBody window UI.#+ [UI.element canv]
  return ()

drawGrid :: UI.Element -> Double -> [((Int,Int),Cell)] -> UI.UI ()
drawGrid _ _ [] = return ()
drawGrid e step (((x,y),c):cs) = do
                                   drawCell e (calcDrawPos (x,y) step) 20.0 c
                                   drawGrid e step cs

drawCell :: UI.Element -> (Double,Double) -> Double -> Cell -> UI.UI ()
drawCell e (x,y) size c = do
                            drawCellStatus e (status c)
                            e UI.# UI.fillRect (x,y) size size

drawCellStatus e Empty = do e UI.# UI.set' UI.fillStyle (UI.htmlColor "#c9c9c9")
drawCellStatus e Mine = do e UI.# UI.set' UI.fillStyle (UI.htmlColor "red")
drawCellStatus e Clear = do e UI.# UI.set' UI.fillStyle (UI.htmlColor "black")

updateGrid :: Grid -> (Int,Int) -> Grid
updateGrid grid (x,y) = Map.adjustWithKey (\_ c -> c{status=Clear}) (x,y) grid

calcDrawPos :: (Int,Int) -> Double -> (Double,Double)
calcDrawPos (x,y) step = (dx,dy) where dx = (fromIntegral x) * step
                                       dy = (fromIntegral y) * step

findCellPos :: (Double,Double) -> Double -> (Int,Int)
findCellPos (x,y) size = (floor $ 20 * (x / size) ,floor $ 20 * (y / size))

