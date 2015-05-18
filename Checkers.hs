-- Required stuff (monads, set functiosn, list functions etc).
import System.IO
import Data.Set as Set
import Data.Monoid
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import Data.List (find)

-- StateT monad used for keeping track of the game and minimax tree states.
type STM a = StateT (Sum Int) (ReaderT  (Bool, (Int -> Board -> BoardReport -> Bool), Board -> Int) (ErrorT String IO)) a 

-- As specified by the official rules - once a game has run for 50 moves, it is over.
maxmoves :: Int
maxmoves = 50 

-- Upper bound (max search). Intentionally unrealistically high.
ubound :: Int
ubound = 100000

-- Lower bound; same deal.
lbound :: Int
lbound = -ubound

-- Data types to represents squares on the board, captures, and moves.
type Square = (Int, Int)
type Capture = (Square, Square)
type Move = [Square]


data BoardReport = BRWinner Color | Ongoing [Board] deriving (Show)
data GameState = GameState {board :: Board}
data GameUpdate = Winner Color | Move Board 

-- Show instance for pretty printing the board after each move decision.
instance Show GameUpdate where
  show (Move brd) = show brd
  show (Winner c) = show c ++ " player wins!\n"

-- Color data structure.
-- Requires Ord instance for Data.Set stuff like filter, sort, etc..
data Color = Black | Red deriving (Show, Eq, Ord)

-- Piece data structure
-- Reg/King = is a normal piece or a king piece; pos = square; clr = color
data Piece = Reg {pos :: Square, clr :: Color}
  	     | King {pos :: Square, clr :: Color}
	deriving (Show, Eq)

-- Compares two squares (necessary to derive Ord on Piece)
compSquare :: (Int, Int) -> (Int, Int) -> Ordering
compSquare (a, b) (c, d)
  | a == c	= compare b d
  | otherwise	= compare a c

-- Provides a ordering system for pieces based on location.
instance Ord Piece where
  compare x y 	= compSquare (pos x) (pos y)

-- Board data structure.  
-- Contains: 
--  red piece set
--  black piece set
--  player whose move it initially is
--  number of elapsed moves so far
data Board = Board {
    redpcs :: Set.Set Piece, blkpcs :: Set.Set Piece, cmpplyr :: Color, crntrnd :: Int
} 

-- Simple board; with this one black is guaranteed to lose in 1 turn
board1 :: Board
board1 = Board (Set.fromList [(King (3, 8) Red)] ) (Set.fromList [(Reg (1, 8) Black)]) Black 1

-- Regular checker game board.
board2 :: Board
board2 = Board (Set.fromList ([(Reg (2, 1) Red)]++[(Reg (4, 1) Red)]++[(Reg (6, 1) Red)]++[(Reg (8, 1) Red)]++[(Reg (1, 2) Red)]++[(Reg (3, 2) Red)]++[(Reg (5, 2) Red)]++[(Reg (7, 2) Red)]++[(Reg (2, 3) Red)]++[(Reg (4, 3) Red)]++[(Reg (6, 3) Red)]++[(Reg (8, 3) Red)])) (Set.fromList ([(Reg (1, 6) Black)]++[(Reg (3, 6) Black)]++[(Reg (5, 6) Black)]++[(Reg (7, 6) Black)]++[(Reg (2, 7) Black)]++[(Reg (4, 7) Black)]++[(Reg (6, 7) Black)]++[(Reg (8, 7) Black)]++[(Reg (1, 8) Black)]++[(Reg (3, 8) Black)]++[(Reg (5, 8) Black)]++[(Reg (7, 8) Black)])) Red 1

-- Returns true if the specified piece is positioned at the specified square; false otehrwise
pieceAt :: Piece -> Square -> Bool
pieceAt pc (x, y) = pos pc == (x, y)

-- Removed; not used in final project.
-- bloatString :: Char -> String -> String
-- bloatString _ "" = ""
-- bloatString c (ch:st) = ch:c:bloatString c st


printPiece :: Piece -> Char
printPiece (Reg _ Red) = 'a'
printPiece (Reg _ Black) = 'b'
printPiece (King _ Black) = 'B'
printPiece (King _ Red) = 'A'


-- Shows the board in a pretty printed format
-- First, prints out the column names (A-H).
-- Then, prints out the row numbers (1-8), followed by the actual content of the squares in each number ('.' for no piece; 'a', 'A', 'b', 'B' for pieces)
-- Had a lot of help on this one from Stack Overflow.
instance Show Board where
  show b = let letters = " ABCDEFGH"
             in unlines (letters:Prelude.map printRowHead [1..8])
     where
      pcl = Set.toList (redpcs b) ++ Set.toList (blkpcs b)
      printRowHead n = show n ++ Prelude.map (printRowSquares n) [1..8]
      printRowSquares y x 
        	= case find (\pc -> pos pc == (x, y)) pcl of
                         Just k -> printPiece k
                         Nothing -> '.'


-- Initializes the state monad (necessary for continuously updating the state of the games)
runSTM :: Num a1 => rdr -> StateT (Sum a1) (ReaderT rdr (ErrorT er mn)) a -> mn (Either er (a, Sum a1))
runSTM config x =  runErrorT $ runReaderT (runStateT x (Sum 0)) config

-- Function that runs the alpha/beta pruning in the context of the StateT monad.
runAlphaBetaPrune :: Board ->  (Bool, (Int -> Board -> BoardReport -> Bool), Board -> Int) -> IO (Either String (GameUpdate, Sum Int))
runAlphaBetaPrune brd cnt = runSTM cnt (alphaBetaPrune brd)

-- Actual alpha-beta pruning algorithm; primary function.
-- Given the board; call minimax searching where the initial parameters are the bounds we specified. 
-- Return the winning board
alphaBetaPrune :: Board -> STM GameUpdate
alphaBetaPrune brd = case genReport brd of
                  BRWinner x -> return (Winner x)
                  _ -> do Move pc <- fmap (Move . fst) (if cmpplyr brd == Red 
                                                       then maxSearch 0 lbound ubound brd
                                                       else minSearch 0 lbound ubound brd)
                          return $ Move pc

-- Given an integer parameter, a board, and the progress of the game,
--  return the cut-off threshold in the stateT monad
-- Base cases are if someone's going to win the game in one step; we don't need to bother with determining the optimal pruning value.
-- Just return a ridiculously extreme number so that nothing gets pruned
coThreshold :: Int -> Board -> BoardReport -> STM Int
coThreshold _ _ (BRWinner Black) =  return (negate 1000)
coThreshold _ _ (BRWinner Red) =  return 1000
coThreshold i b r = do
  cf <- asks (\(_,f,_) -> f)
  if even i && cf i b r then (asks (\(_,_,e) -> e)) >>= \f -> return (f b)
                        else mzero

-- Step one branch further down the tree
-- I had extensive help with this concept and the min/max searching functions below on Stack Overflow.
stepInTree :: (MonadState w m, Monoid w) =>  w -> m ()
stepInTree mndst = do
  wrd <- get
  put $! wrd `mappend` mndst

-- Primary min searching function
-- Takes the recursion level (so far), alpha bound, beta bound, board to search, and returns a tuple with the board and the minimum score.
-- Generates a list of child boards from the given board and recurses on them with the helper method.
-- I had extensive help with this concept on Stack Overflow.
minSearch :: Int -> Int -> Int -> Board -> STM (Board, Int)
minSearch recur alpha beta brd = do
  stepInTree (Sum 1) 
  let brdreport = genReport brd
      list = getChildBoards brdreport
  mplus (coThreshold recur brd brdreport >>= (\s -> return (brd,s)))  (mnsHelper recur alpha beta list brd)
   
-- Helper method for minimum searching.
-- Given the same parameters as minSearch and also a list of boards, pick the best possible board as far as min score goes.
-- Base case: the list is empty
-- Recursive case: try a max search with the bounds we currently have.
-- 		   if the max value is smaller than our current beta, use the board we currently have
--		   otherwise, recurse down the list using the max value as the new beta.
-- I had extensive help with this concept on Stack Overflow.
mnsHelper :: Int -> Int -> Int -> [Board] -> Board -> STM (Board, Int)
mnsHelper _ _ beta [] brd = return (brd,beta)
mnsHelper recur alpha beta (x:xs) brd = do
  max <- maxSearchFromMin (recur + 1) alpha beta x 
  let brd2 = if max < beta then x else brd
      temp = min beta max
  if temp <= alpha then return (brd2, temp)
            else mnsHelper recur alpha temp xs brd2

-- Primary max searching function
-- Same concept as minSearch but working in the opposite direction.
-- Generates a list of child boards and recurses through them with the helper method.
-- I had extensive help with this concept on Stack Overflow.
maxSearch :: Int -> Int -> Int -> Board -> STM (Board, Int)
maxSearch recur alpha beta brd = do
  stepInTree (Sum 1) 
  let report = genReport brd
      list = getChildBoards report
  (coThreshold recur brd report >>= (\s -> return (brd,s)))  `mplus` mxsHelper recur alpha beta list brd

-- Helper method for maxSearch
-- Contains the actual recursive method call
-- Algorithm:
--  try a min search with the bounds we currently have
--  if the min search is larger than our alpha value, use the board we currently have.
--  otherwise keep searching with the min value as the new alpha value.
-- I had extensive help with this concept on Stack Overflow.
mxsHelper :: Int -> Int -> Int -> [Board] -> Board -> STM (Board, Int)
mxsHelper _ alpha _ [] brd = return (brd,alpha)
mxsHelper recur alpha beta (x:xs) brd = do
  min <- minSearchFromMax (recur + 1) alpha beta x
  let newb = if min > alpha then x else brd
      temp = max alpha min
  if beta <= temp then return (newb, temp)
            else mxsHelper recur temp beta xs newb

-- Given a BoardReport return *all* child boards in the boardlist.
getChildBoards :: BoardReport -> [Board]
getChildBoards (Ongoing l) = l
getChildBoards _ = []

-- Custom version of minimum search, designed specifically to be called from the max search and only return a single integer (within the stateT monad)
minSearchFromMax :: Int -> Int -> Int -> Board -> STM Int
minSearchFromMax recur alpha beta brd = do
  stepInTree (Sum 1)
  let report = genReport brd
      list = getChildBoards report
  mplus (coThreshold recur brd report) (mnsfmxHelper recur alpha beta list)

-- Helper method for the above method; contains the actual recursive calls.
mnsfmxHelper :: Int -> Int -> Int -> [Board] -> STM Int
mnsfmxHelper _ _ beta [] = return beta
mnsfmxHelper recur alpha beta (x:xs) = do
  max <- maxSearchFromMin (recur + 1) alpha beta x 
  let temp = min beta max
  if temp <= alpha then return temp
            else mnsfmxHelper recur alpha temp xs

-- Custom version of maximum search; designed specificially to be called from the min search algorithm for internal use.
-- Analogous to the minsearchfrommax functions above.
maxSearchFromMin :: Int -> Int -> Int -> Board -> STM Int
maxSearchFromMin recur alpha beta brd = do
  stepInTree (Sum 1)
  let report = genReport brd
      list = getChildBoards report
  mplus (coThreshold recur brd report) (mxsfmnHelper recur alpha beta list)

-- Helper method for the above function; same deal as mxfmnHelper
mxsfmnHelper :: Int -> Int -> Int -> [Board] -> STM Int
mxsfmnHelper _ alpha _ [] = return alpha
mxsfmnHelper recur alpha beta (x:xs) = do
  min <- minSearchFromMax (recur + 1) alpha beta x
  let temp = max alpha min
  if beta <= temp then return temp 
            else mxsfmnHelper recur temp beta xs 

-- Determines whether or not to perform alpha beta pruning based on the board state, a multiplication modifier, and an opper bound.
-- Thought about adding randomness, but didnt' think the FSM mechanics should have randomness - only the game state *initializers*
pruneRules :: Int -> Int -> Board -> BoardReport -> Bool
pruneRules _ _ _ (BRWinner _) = True
pruneRules mod bnd brd (Ongoing (x:_)) 
  | bnd <=  (11 * mod `div` (Set.size (friendlyPcs brd)+7)) = False
  | Set.size (enemyPcs brd) > Set.size (friendlyPcs x) = False
  | otherwise = True

-- HElper method; returns true if the piece is a king; false otherwise.
isKing :: Piece -> Bool
isKing (King _ _) = True
isKing _	  = False

-- HElper method; returns true if the piece is not a king; false otherwise.
isReg :: Piece -> Bool
isReg (Reg _ _) = True
isReg _ 	= False

-- Given a board state, calculate the score of the board (as discussed in assignment webpage on robins site - weight the pieces of the two sides comparatively; but weight kings more)
evalEquation :: Board -> Int
evalEquation brd = (Set.size r + (Set.size rk)*2) - (Set.size bl + (Set.size bk)*2)
  where r =   Set.filter (isReg) (redpcs brd)
        bl =  Set.filter (isReg) (blkpcs brd)
	rk =  Set.filter (isKing) (redpcs brd)
	bk =  Set.filter (isKing) (blkpcs brd)
	

-- Main game function.
-- Called recursively throughout the computer's decisions.
-- Given a board and two state monads attempt to make a move after alpha-beta pruning.  If it doesn't work, try the next 
gameProc :: Board -> ((Bool, (Int -> Board -> BoardReport -> Bool), Board -> Int)) -> IO ()
gameProc brd (state1) 
 | crntrnd brd >= maxmoves = putStrLn $ show maxmoves ++ " moves have passed; game is a draw.\n"
 | otherwise = do
   Right (m, Sum n) <- runAlphaBetaPrune brd state1
   case m of 
        Winner _ -> print m
        Move b' -> do
            print b'
            gameProc b' (state1)

-- Given a board and an update descriptor, write the new board using the IO monad.
boardStateWriter :: Board -> (Board -> GameUpdate, Board -> GameUpdate) -> IO ()
boardStateWriter brd (upd1,upd2)
 | crntrnd brd == maxmoves = putStrLn $ show maxmoves ++ " moves have passed; game is a draw.\n"
 | otherwise = do
    let b' = upd1 brd
    case b' of
      Winner _ -> print b'
      Move x -> putStrLn (show (cmpplyr brd) ++ " plays:" ) >>
                     print x >> boardStateWriter x (upd1,upd2)


-- Inverts the board's current player in memory.
-- Necessary for determining opponent side moves
flipBoard :: Board -> Board
flipBoard (Board rp bp c r) = Board rp bp (oppclr c) (r+1)

-- Simple method; given a color return the other color
oppclr :: Color -> Color
oppclr Red = Black
oppclr Black = Red

-- Given a board return the set of the pieces on the same team the computer is currently playing as.
friendlyPcs :: Board -> Set.Set Piece
friendlyPcs b
  | cmpplyr b == Red 	= redpcs b
  | otherwise 		= blkpcs b

-- Given a board return the set of the pieces on the opposing team the computer is currently playing as.
enemyPcs :: Board -> Set.Set Piece
enemyPcs b
  | cmpplyr b == Black 	= redpcs b
  | otherwise 		= blkpcs b


-- Helper method; returns true if a number is between arguments 2 and 3; false otherwise.
between :: Int -> Int -> Int -> Bool
between a x y 	= (a<=y)&&(a>=x)

-- validates that the specified square exists on a checkers board ie is (1,1)-(8,8)
validateSquare :: Square -> Bool
validateSquare (x, y) = (between x 1 8) && (between y 1 8)

-- validates a jump move (validate both source and destination targets).
validateJump :: Capture -> Bool
validateJump (src, dst) = validateSquare src && validateSquare dst

-- Given a square return all potential moves from a piece on that square (True = look up; False = look down)
possibleMoves :: Square -> Bool -> [Square]
possibleMoves (x, y) True	= Prelude.filter validateSquare [(x+1, y+1), (x-1, y+1)]
possibleMoves (x, y) False	= Prelude.filter validateSquare [(x+1, y-1), (x-1, y-1)]

-- Given a piece, return the valid moves that piece can make (reg can move only up OR down (not both); king can move either way).
possibleSteps :: Piece -> [Square]
possibleSteps (Reg sq Red) 	= (possibleMoves sq True)
possibleSteps (Reg sq Black) 	= (possibleMoves sq False)
possibleSteps (King sq _) 	= (possibleMoves sq True)++(possibleMoves sq False)

-- Given a piece, return the valid captures it can make (if there are target pieces nearby).
possibleCaptures :: Piece -> [Capture]
possibleCaptures (Reg (x,y) Black) = Prelude.filter validateJump [((x+1,y-1), (x+2,y-2)), ((x-1,y-1),(x-2,y-2))]
possibleCaptures (Reg (x,y) Red) = Prelude.filter validateJump [((x+1,y+1),(x+2,y+2)),((x-1,y+1),(x-2,y+2))]
possibleCaptures (King (x,y) _) = Prelude.filter validateJump [((x+1,y-1), (x+2,y-2)), ((x-1,y-1),(x-2,y-2)), ((x+1,y+1),(x+2,y+2)),((x-1,y+1),(x-2,y+2))]

-- Remove a piece from the targeted position.
-- Necessary after a capture or kinging etc.
deletePiece :: Piece -> Board -> Board
deletePiece p (Board rp bp c r) 
  | clr p == Red 	= Board (Set.delete p rp) bp c r
  | otherwise 		= Board rp (Set.delete p bp) c r

-- Returns true if there is no piece at the target square; false otherwise.
noPieceAt :: Board -> Piece -> Bool
noPieceAt b p
  | clr p == Red 	= Set.notMember p (redpcs b)
  | otherwise 		= Set.notMember p (blkpcs b)

-- Returns true if there is neither a red nor black piece on th specified square.
sqrIsEmpty :: Board -> Square -> Bool
sqrIsEmpty brd sq = noPieceAt brd (Reg sq Red) && noPieceAt brd (Reg sq Black)

-- Given a piece return all boards that could result in that piece moving in any legal direction.
childBoardsFromPiece :: Piece -> Board -> [Board]
childBoardsFromPiece pc brd = do
  sq <- Prelude.filter (sqrIsEmpty brd) $ possibleSteps pc
  return $ commitPace pc sq brd

commitPace :: Piece -> Square -> Board -> Board
commitPace pc sq = addPiece $ movePiece pc sq


-- Move a piece; promoting it to  king if necessary
movePiece :: Piece -> Square -> Piece
movePiece (Reg _ Black) (x, 1)  = King (x, 1) Black
movePiece (Reg _ Red) (x, 8)  = King (x, 8) Red
movePiece (Reg _ c) sq = Reg sq c
movePiece (King _ c) sq = King sq c

-- Decide whether to king a piece.
decideKing :: Piece -> Square -> Bool
decideKing (Reg _ Black) (_, 1) = True
decideKing (Reg _ Red) (_, 8) = True
decideKing _ _ = False

-- Given a piece return all possible boards resulting if it makes a capture/jump move.
captures :: Piece -> Board -> [Board]
captures pc brd = do
  (src, dst) <- Prelude.filter (canCapture brd) (possibleCaptures pc)
  let newbrd = deletePiece (Reg src (oppclr $ clr pc)) brd 
      newpc = movePiece pc dst
  if decideKing pc dst then return (addPiece newpc newbrd)
                 else capturesHelper newpc newbrd

-- Helper method for captures - 
capturesHelper :: Piece -> Board -> [Board]
capturesHelper pc brd = let pcapture = Prelude.filter (canCapture brd) (possibleCaptures pc)
                             in if Prelude.null pcapture then return (addPiece pc brd)
                                           else do
                                             (src,dst) <- pcapture;
                                             let newbrd = deletePiece (Reg src (oppclr $ clr pc)) brd
                                                 newpc = movePiece pc dst
                                             if decideKing pc dst then return (addPiece newpc newbrd)
                                                            else capturesHelper newpc newbrd


-- Inserts a piece into the board.
addPiece :: Piece -> Board -> Board
addPiece p (Board rp bp c r)
  | clr p == Red 	= Board (Set.insert p rp) bp c r
  | otherwise 		= Board rp (Set.insert p bp) c r


-- Given a board return true if it is possible for the current side's turn to make the specified capture.
canCapture :: Board -> Capture -> Bool
canCapture b (src, dst)
  | cmpplyr b == Red 	= (noPieceAt b (Reg src Black)) == False && sqrIsEmpty b dst
  | otherwise 		= (noPieceAt  b (Reg src Red)) == False && sqrIsEmpty b dst

-- Determine the next game state
-- If the current side cannot make any moves or captures then the other side wins
-- Otherwise, process the possible moves and/or jumps
nextGameState :: Reader GameState BoardReport
nextGameState = do
  pcs <- asks (friendlyPcs . board)
  b <- asks board
  let deletedPair = Prelude.map (\p -> (p, deletePiece p b)) (Set.toList pcs)
      jmplist = concatMap (uncurry captures) deletedPair
      movlist = concatMap (uncurry childBoardsFromPiece) deletedPair
  merge jmplist movlist
  where
    liftAndSort = return . Ongoing . Prelude.map flipBoard
    merge jmplist movlist
      | (Prelude.null jmplist)==False  = liftAndSort jmplist
      | (Prelude.null movlist)==False  = liftAndSort movlist
      | otherwise = liftM (BRWinner . oppclr . cmpplyr. board) ask


-- Generates a BoardReport given the specified board (useful in the AI functions).
genReport :: Board -> BoardReport
genReport brd =  runReader nextGameState (GameState brd)


-- Main method: prints out the two boards in their initial state and runs them with evaluation equations.
main ::  IO ()
main = do
  putStrLn "Sample board #1 (quick winning demonstration): "
  let eqn = (True, pruneRules 5, evalEquation)
  gameProc board1 (eqn)	
  putStrLn "Press a key to see next board (real checkers game): "
  getChar 
  putStrLn "Sample board #2: "
  print board2
  let eqn2 = (True, pruneRules 3, evalEquation)
  gameProc board2 (eqn2)
  return ()
