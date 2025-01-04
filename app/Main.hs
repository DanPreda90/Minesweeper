module Main (main) where

import Lib

import Data.IORef
import qualified Data.Map.Strict as Map
import System.Random
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy

import qualified Graphics.UI.Threepenny as UI

main :: IO ()
main = do
  UI.startGUI UI.defaultConfig setup

setup window = do
  let size = 20
  let cellSize = 20
  let step = 20
  generator <- initStdGen

  let t = grid size
  let (g,(m,st)) = runState (initGameState t size generator) (0,Playing)
  gamestate <- liftIO $ newIORef (m,st)
  gridstate <- liftIO $ newIORef g
  
  currentPosition <- liftIO $ newIORef (0,0)
  markToggle <- liftIO $ newIORef Clear

  clearBtn <- UI.button UI.#+ [UI.string "Reset "]
  markCheck <- UI.input UI.# UI.set UI.type_ "checkbox"

  canv <- UI.canvas
    UI.# UI.set UI.height 700
    UI.# UI.set UI.width 700
    UI.# UI.set UI.style [("border","solid black 1px"),("background","#eee")]

  UI.on UI.mousemove canv $ \xy ->
    do liftIO $ writeIORef currentPosition xy

  UI.on UI.click clearBtn  $ \_ ->
    do
      gen <- newStdGen 
      let cleared = grid size
      let (g,(m,st)) = runState (initGameState cleared size gen) (0,Playing)
      liftIO $ writeIORef gamestate (m,st) 
      liftIO $ writeIORef gridstate g
      drawGrid canv step (Map.assocs g)

  UI.on UI.click canv $ \_ ->
    do 
      gs <- liftIO $ readIORef gamestate
      case (snd gs) of
        Playing ->
          do
            mark <- liftIO $ readIORef markToggle
            liftIO $ print mark
            pos <- liftIO $ readIORef currentPosition
            gr <- liftIO $ readIORef gridstate
            
            let (x,y) = findCellPos pos (fromIntegral $ size*cellSize)
            let (grNew,gsNew) = runState (updateGameState gr (x,y) mark) gs
            liftIO $ print gsNew
            liftIO $ print (x,y)
            liftIO $ writeIORef gamestate gsNew
            liftIO $ writeIORef gridstate grNew 
            drawGrid canv step (Map.assocs grNew)
            return ()
        _ -> return ()

  UI.on UI.click markCheck $ \_ ->
    do
      liftIO $ modifyIORef' markToggle toggleMarkDisplay
      
  do
    gr <- liftIO $ readIORef gridstate
    drawGrid canv step (Map.assocs gr)
    return ()

  UI.getBody window UI.#+ [UI.element canv,UI.element clearBtn] UI.#+ [UI.element markCheck, UI.string "Toggle to Mark Tiles"]
  return ()

drawGrid :: UI.Element -> Double -> [((Int,Int),Cell)] -> UI.UI ()
drawGrid _ _ [] = return ()
drawGrid e step (((x,y),c):cs) = do
                                   drawCell e (calcDrawPos (x,y) step) 20.0 c
                                   drawGrid e step cs

drawCell :: UI.Element -> (Double,Double) -> Double -> Cell -> UI.UI ()

{-drawCell e (x,y) size Cell{mines=(-1)} = do 
                                      e UI.# UI.set' UI.fillStyle (UI.htmlColor "red")
                                      e UI.# UI.fillRect (x,y) size size-}
drawCell e (x,y) size c = do
                            drawCellStatus e (status c)
                            e UI.# UI.fillRect (x,y) size size

drawCellStatus e Empty = do e UI.# UI.set' UI.fillStyle (UI.htmlColor "#c9c9c9")
drawCellStatus e Marked = do e UI.# UI.set' UI.fillStyle (UI.htmlColor "green")
drawCellStatus e Clear = do e UI.# UI.set' UI.fillStyle (UI.htmlColor "black")
