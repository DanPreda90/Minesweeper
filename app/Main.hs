module Main (main) where

import Lib
import Data.IORef
import qualified Data.Map.Strict as Map
import System.Random
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict

import qualified Graphics.UI.Threepenny as UI

main :: IO ()
main = do
  UI.startGUI UI.defaultConfig setup

setup :: UI.Window -> UI.UI ()
setup window = do
  let size = 20
  let cellSize = 20
  let step = 20
  generator <- initStdGen

  let t = grid size
  let (g,(m,st)) = runState (initGameState t size generator) (0,Playing)
  liftIO $ print m
  gamestate <- liftIO $ newIORef (m,st)
  gridstate <- liftIO $ newIORef g
  safeMoves <- liftIO $ newIORef []
  
  currentPosition <- liftIO $ newIORef (0,0)
  markToggle <- liftIO $ newIORef Clear

  clearBtn <- UI.button UI.#+ [UI.string "Reset "]
  markCheck <- UI.input UI.# UI.set UI.type_ "checkbox"
  solveBtn <- UI.button UI.#+ [UI.string "Press to find next safe move"]

  canv <- UI.canvas
    UI.# UI.set UI.height 700
    UI.# UI.set UI.width 700
    UI.# UI.set UI.style [("border","solid black 1px"),("background","#eee")]

  UI.on UI.mousemove canv $ \xy ->
    do liftIO $ writeIORef currentPosition xy

  UI.on UI.click solveBtn $ \_ ->
    do
      gr <- liftIO $ readIORef gridstate
      gs <- liftIO $ readIORef gamestate
      safemvs <- liftIO $ readIORef safeMoves
      liftIO $ print safemvs
      case (safemvs) of
        [] -> 
          do
            case (findNextSafeMoves gr) of
              [] -> return ()
              (m:ms) ->
                do
                  let (grNew,gsNew) = runState (updateGameState gr m Clear) gs
                  liftIO $ writeIORef gamestate gsNew
                  liftIO $ writeIORef gridstate grNew 
                  liftIO $ writeIORef safeMoves ms  
                  drawGrid canv step (Map.assocs grNew) 
                  return ()

        (m:ms) ->
          do
            let (grNew,gsNew) = runState (updateGameState gr m Clear) gs
            liftIO $ writeIORef gamestate gsNew
            liftIO $ writeIORef gridstate grNew 
            liftIO $ writeIORef safeMoves ms  
            drawGrid canv step (Map.assocs grNew) 
            return ()


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

  _ <- UI.getBody window UI.#+ [UI.element canv,UI.element clearBtn,UI.element solveBtn] UI.#+ [UI.element markCheck, UI.string "Toggle to Mark Tiles"]
  return ()

drawGrid :: UI.Element -> Double -> [((Int,Int),Cell)] -> UI.UI ()
drawGrid _ _ [] = return ()
drawGrid e step (((x,y),c):cs) = do
                                   drawCell e (calcDrawPos (x,y) step) 20.0 c
                                   drawGrid e step cs

drawCell :: UI.Element -> (Double,Double) -> Double -> Cell -> UI.UI ()
drawCell e (x,y) size c = 
  if (mines c > 0 && status c == Clear) then
    do
      e UI.# UI.set' UI.fillStyle (UI.htmlColor "black")
      e UI.# UI.fillRect (x,y) size size
      e UI.# UI.set' UI.fillStyle (getCellColor c)
      e UI.# UI.set' UI.textFont "10px sans-serif"
      e UI.# UI.fillText (show (mines c)) (x+5,y+10) 
      
  else
    do
      e UI.# UI.set' UI.fillStyle (getCellColor c)
      e UI.# UI.fillRect (x,y) size size

drawCell e (x,y) size c = do
                            e UI.# UI.set' UI.fillStyle (getCellColor c)
                            e UI.# UI.fillRect (x,y) size size

getCellColor :: Cell -> UI.FillStyle
getCellColor Cell{status=Clear,mines=m} =  
  case (m) of
    0 -> UI.htmlColor "black"
    1 -> UI.htmlColor "#efed5b"
    2 -> UI.htmlColor "#efcb5f"
    3 -> UI.htmlColor "#efbc23"
    4 -> UI.htmlColor "#f96d02"
    5 -> UI.htmlColor "red"
    6 -> UI.htmlColor "#b20000"
    7 -> UI.htmlColor "#a50052"
    8 -> UI.htmlColor "#700000"
    _ -> UI.htmlColor "black"
getCellColor Cell{mines=(-1)} = UI.htmlColor "red"
getCellColor Cell{status=Empty} = UI.htmlColor "#c9c9c9"
getCellColor Cell{status=Marked} = UI.htmlColor "green"

