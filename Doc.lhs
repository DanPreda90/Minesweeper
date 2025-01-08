To create an instance of the minesweeper game, use initGameState

> main = 
> do
>   gen <- initStdGen
>   let gs = initGameState (grid 20) 20 gen
>   let (grid,gs) = runState gs (0,Playing)

To update the state of the game, we can use updateGameState

> do
>   gen <- initStdGen
>   let (g_final,(cleared,state)) = runState $ (do
>       g <- initGameState (grid 20) 20 gen
>       g_updated <- updateGameState g (5,5) Clear
>       return g_updated) (0,Playing)
>   case (state) of
>     Playing -> do print g_final
>     Won -> do print "Won!"
>     _ -> do print "Game over"

> updateGameState :: Grid -> (Int,Int) -> Status -> initGameState
1st arg: the input grid
2nd arg: the position of the move
3rd arg: the status you want to set the cell to (Clear/Marked), if status = Empty, returns the original Grid
