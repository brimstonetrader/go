{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTSyntax #-}

-- cd OneDrive\Documents\csci365-game\game
-- cabal run


module Game where

import Data.Char
import Data.Maybe
import Text.Read
import qualified Data.Map.Strict as M

(%), (//) :: Int -> Int -> Int
(%)  = mod
(//) = div

type Liberties = [Int]
type Group     = [Int]
type Status    = [(Group, Liberties)]
type Board     = [Player]

data Move where 
  Play :: Player -> Int -> Move
  Pass :: Move
  deriving Eq

data Player = Black | White | Empty 
  deriving (Show, Eq)

data GameState where
  Go    :: Player ->  
           Int ->              --board size
           Board ->            --all played stones
           [Int] ->            --rng
           (Status, Status) -> --all current groups and liberties
           (Bool, Bool) ->     --whether the past two turns have been passes
           GameState
  Won   :: Player ->           --winner
           (Double, Double) -> --margin of victory
           GameState
  
initGameState :: [Int] -> String -> GameState
initGameState rs s = 
  let sz = fromMaybe 7 (readMaybe s) 
  in Go Black sz (replicate (sz*sz) Empty) rs ([], []) (False, False)

--Prints board to screen every so often
gamePrompt :: GameState -> String
gamePrompt (Go Black s b _ _ _) = '\n':printBoard (-1) s b++("\nInput coordinate: ")
gamePrompt (Go White s b _ _ _) = '\n':printBoard (-1) s b++("\nInput coordinate, or press Enter for AI play: ")  
gamePrompt (Won White _)        = "White won! Play again?"
gamePrompt (Won Black _)        = "Black won! Play again?"
gamePrompt _                    = "How did that happen? Try again"  

--gamestep manager, where parsing and updating is called.
gameStep :: String -> GameState -> (String, Maybe GameState)
gameStep _ (Won Black _)  = ("You are a winner", Nothing)
gameStep _ (Won White _) = ("Better luck next time", Nothing)
gameStep _ (Won Empty _) = ("None of the above", Nothing)
gameStep _ (Go _ sz b _ _ (True, True)) = 
  let s@(bv, wv) = score b sz in 
  ("", Just (Won (if bv>=wv then Black else White) s))
gameStep m g@(Go White sz b rnds s (_, b1)) =
  let move = fromMaybe (selectMove g) (parseMove White sz m) in
  let (board, status) = update b sz s move in
  ("", Just (Go Black sz board rnds status (b1, move == Pass)))
gameStep m g@(Go p sz b rnds s (_, b1)) =
  case parseMove p sz m of
    Nothing   -> ("Try again.", Just g)
    Just move@(Play _ i) -> 
      if b !! i /= Empty 
        then ("Already occupied, try again", Just g) 
        else if filter (==Empty) (map (\n -> b!!n) (findNeighbors sz i)) == [] then ("No liberties there, try again.", Just g) 
        else 
          let (board, status) = update b sz s move 
          in ("", Just (Go White sz board rnds status (b1, False)))
    Just Pass -> ("", Just (Go p sz b rnds s (b1, True)))


-- 1. Merge Active Player's Groups
-- 2. Remove Move from other Player's Liberties.
-- 3. Remove Groups with no remaining liberties.

update :: Board -> Int -> (Status, Status) -> Move -> (Board, (Status, Status))
update b _ ss Pass = (b, ss) 
update b sz (bs, ws) (Play p i) = 
  let (opss, caps) = updateOpStatus (if p==Black then ws else bs) i in
  let plss = updateMyStatus sz (if p==Black then bs else ws) i ([i], [n | n <- findNeighbors sz i , n < length b, b !! n == Empty]) caps in
  (updateBoard b i 0 p caps, (if p==Black then (plss, opss) else (opss, plss)))

--Adds new stone, removes captures
updateBoard :: Board -> Int -> Int -> Player -> [Int] -> Board 
updateBoard [] _ _ _ _ = []
updateBoard (b:bs) pos idx plr caps = 
  (if elem idx caps then Empty 
    else (if pos==idx then plr else b)):updateBoard bs pos (idx+1) plr caps

alphabet :: String
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

--prints board, with coordinates on top and left.
printBoard :: Int -> Int -> Board -> String
printBoard _ _ []         = ""
printBoard (-1) n ps      = ' ':take n alphabet ++ printBoard 0 n ps
printBoard i n (p:ps) = 
  if i%n==0 
    then '\n':(alphabet !! (i//n)):(playerToChar p):printBoard (i+1) n ps 
    else (playerToChar p):printBoard (i+1) n ps
    
--returns coordinate based on first two chars of input, or pass.
parseMove :: Player -> Int -> String -> Maybe Move
parseMove _ _ ('P':'A':'S':'S':_) = Just Pass
parseMove player size (x:y:_) = do 
  let (vx, vy) = (ord x, ord y)
  if vx < 65+size && vy < 65+size && 64 < vx && 64 < vy
    then Just (Play player (size * (vy - 65) + (vx - 65)))
    else Nothing
parseMove _ _ _ = Nothing 

--Removes placed stone from liberties, and libertyless groups.
updateOpStatus :: Status -> Int -> (Status, [Int]) 
updateOpStatus [] _ = ([], [])
updateOpStatus ((g,l):gls) i
  | l == [i]  = let (s, gg) = updateOpStatus gls i in (s, g++gg)
  | elem i l  = let (s, gg) = updateOpStatus gls i in ((g, filter (/=i) l):s, gg)
  | otherwise = let (s, gg) = updateOpStatus gls i in ((g,l):s, gg)

--Merges groups, and adds new liberties in from the captures
updateMyStatus :: Int -> Status -> Int -> ([Int], [Int]) -> [Int] -> Status 
updateMyStatus _ [] _ t _ = [t]
updateMyStatus s ((g,l):gls) i (grp, lib) caps = 
  if elem i l 
    then updateMyStatus s gls i (dropDups $ grp++g, dropDups $ lib++(filter (/=i) l)++(filter (\x -> elem x caps) (groupNeighbors s (i:g)))) caps 
    else (g, l++filter (\x -> elem x caps) (groupNeighbors s g)):updateMyStatus s gls i (grp,lib) caps

--Shell method for MCTS
selectMove :: GameState -> Move 
selectMove g@(Go p _ b _ _ _) = do 
  let legalMoves = empties 0 b
  let scores = M.fromList [(x,0.0) | x <- legalMoves]
  let visits = M.fromList [(x,1) | x <- legalMoves]
  Play p (mcts g legalMoves scores visits 0 (15000//length legalMoves))
selectMove _ = Pass
  
--Finds best move via repeated iteration.
mcts :: GameState -> [Int] -> M.Map Int Double -> M.Map Int Int -> Int -> Int -> Int
mcts g@(Go p sz b rnds s (_, b1)) legalMoves scores visits itr cap = 
  let m = uct legalMoves scores visits itr (-1, -1) in
  if itr>cap 
    then m
    else 
      let (board, status) = update b sz s (Play p m) in
      let (bv, wv) = randomPlayout (Go (nextPlayer p) sz board rnds status (b1, False)) (length (filter (==Empty) board)) in
      let v = if p == Black then bv else wv in
      mcts g legalMoves (M.adjust (+v) m scores) (M.adjust (+1) m visits) (itr+1) cap
mcts _ _ _ _ _ _ = -1


randomPlayout :: GameState -> Int -> (Double, Double)
randomPlayout (Go _ sz b _ _ _) 0 = score b sz
randomPlayout (Go p sz b (r:rnds) s (b2, b1)) i =
  if b2&&b1 then score b sz else 
  let move = randomMove r p b in
  let (board, status) = update b sz s move in
  randomPlayout (Go (nextPlayer p) sz board rnds status (b1, False)) (i-1)
randomPlayout (Won _ (bv, wv)) _ = (bv, wv)
randomPlayout _ _ = (0,0)

--Heuristic for best move to look at
uct :: [Int] -> M.Map Int Double -> M.Map Int Int -> Int -> (Int, Double) -> Int
uct [] _ _ _ (maxm, _) = maxm
uct (m:legalMoves) scores visits itr (maxm, maxv) = 
  let (w, t) = (scores M.! m, fromIntegral $ visits M.! m) in
  let v = (w / t) + ((2 / t * log (fromIntegral itr)) ** 0.5) in
  if v > maxv
     then uct legalMoves scores visits itr (m, v)
     else uct legalMoves scores visits itr (maxm, maxv)

--Easy functions:

score :: Board -> Int -> (Double, Double)
score b sz = 
  ((fromIntegral $ length $ filter (==Black) b) / (fromIntegral $ sz*sz), 
  ( fromIntegral $ length $ filter (==White) b) / (fromIntegral $ sz*sz))

moveToInt :: Move -> Int
moveToInt (Play _ i) = i
moveToInt _ = -1     
                          
randomMove :: Int -> Player -> Board -> Move
randomMove rnd p b = let v = empties 0 b in 
  if v == [] then Pass else Play p (v !! (rnd % (length v)))

empties :: Int -> Board -> [Int]
empties _ []        = []
empties i (Empty:b) = i:(empties (i+1) b)
empties i (_:b)     =    empties (i+1) b

playerToChar :: Player -> Char
playerToChar Black = '@'
playerToChar White = '#'
playerToChar Empty = '·'

nextPlayer :: Player -> Player
nextPlayer Black = White
nextPlayer White = Black
nextPlayer Empty = Empty

groupNeighbors :: Int -> [Int] -> [Int]
groupNeighbors s = dropDups . concat . map (findNeighbors s)

intersection :: Eq a => [a] -> [a] -> [a]
intersection xs ys = filter (\x -> elem x ys) xs

dropDups :: Eq a => [a] -> [a]
dropDups []     = []
dropDups (x:xs) = if elem x xs then dropDups xs else x:dropDups xs

findNeighbors :: Int -> Int -> [Int]
findNeighbors s n = 
  (if s>n        then [] else [n-s])++
  (if s*s-s<n    then [] else [n+s])++
  (if 0==n%s     then [] else [n-1])++
  (if 0==(n+1)%s then [] else [n+1])




-- Shreds:

-- isEmpty :: [Player] -> Int -> Bool
-- isEmpty b pos = b !! pos == Empty

-- mergeGroups :: [[Int]] -> [Int] -> Int -> Int -> [Int] -> ([Int], [Int])
-- mergeGroups (g:gs) ns pos sz ngrp = if filter (`elem` g) ns /= [] then mergeGroups gs ns pos else ([],[])

-- updateBoard :: GameState -> Maybe Move -> (String, Maybe GameState)
-- updateBoard (Go Black _ _ _ _ _) Nothing = ("Something went wrong, try again.", Nothing)
-- updateBoard (Go Black size b rnds (bs, ws) (p2, p1)) (Just (Play (player, pos))) = do
  -- let (ls, f:rs) = splitAt pos b
  -- let ns = findNeighbors size pos
  -- let (bgs, bls) = unzip bs
  -- let (wgs, wls) = unzip ws
  -- if f /= Empty then ("Someone is already in that location.", Nothing) else
    -- (printBoard 1 size b, Just (Go White size 
                            -- (ls++Black:rs) rnds 
                            -- ((mergeGroups bgs ns pos size [], shredLibs bls pos size),                            
                            -- (wgs,                            shredLibs wls pos size))                            
                            -- (p1, False)))                          
                            
-- 
-- updateGroups :: Bool -> (Int, [Int]) -> [([Int], [Int])] -> [([Int], [Int])]
-- updateGroups _ _ [] = []
-- updateGroups True  (pos, nbs) ((ts, ls):rest) = (if any (const True) [elem n ts | n<-nbs] then pos:ts else ts, 
--                                                  if any (const True) [elem n ls | n<-nbs] then pos:ls else ls):updateGroups False (pos, nbs) rest
-- updateGroups False (pos, nbs) ((ts, ls):rest) = (ts, filter (elem pos) ls):updateGroups False (pos, nbs) rest
-- 
-- 
-- 
-- 
-- 
-- gameStep :: String -> GameState -> (String, Maybe GameState)
-- gameStep move@(x:y:_) g@(Go Black size b rnds (bgs, wgs) (p2, p1)) = 
--   ("Move at "++(x:',':' ':y), snd parsePos Black size move)
-- 
-- 
-- 
-- isAdjacent :: Int -> [Int] -> [Int] -> Bool
-- isAdjacent _ [] _ = False
-- isAdjacent sz (g:g1) g2 = if [elem a g2 | a <- [g, g-sz, g+sz, g+1, g-1]] /= [] then True else isAdjacent sz g1 g2

-- shredLibs :: Int -> [[Int]] -> [[Int]]
-- shredLibs p ((l:[]):lss) = undefined

-- removeLiberty :: Int -> [Int] -> [Int]
-- removeLiberty n = filter (/=n)

-- addLiberty :: Int -> [Int] -> [Int]
-- addLiberty n ls = if elem n ls then ls else n:ls


-- def mcts(int time_allowed, GameState root, int player):
  -- #initialize data structures
  -- Move[]                legalMoves = root.getAllPossibleMoves()
  -- dict(Move, double) scoresPerMove = new dict(Move, double)
  -- dict(Move, int)    visitsPerMove = new dict(Move, 1) 
  -- int                numIterations = 0

  
  -- while (time_elapsed < time_allowed):
  
    -- #select which node we explore this time around.
    -- GameState leaf = uct(root, legalMoves, scoresPerMove, visitsPerMove, numIterations)
  
    -- #random playout until we reach end-state.
    -- GameState result = new GameState(leaf)
    -- while result is not terminal:
      -- result = result.apply(result.getRandomMove)

    -- #update data structures
    -- double myScore  = getScores(result)[player]
    -- scoresPerMove[leaf] += myScore
    -- visitsPerMove[leaf]++
    -- numIterations++

  -- #output most explored move.  
  -- return visitsPerMove.keyWithMaxValue()

-- def uct(GameState root, 
--         Move[] legalMoves, 
--         dict(Move, double) scoresPerMove, 
--         dict(Move, int) visitsPerMove, 
--         int numIterations):
  -- Move maxMove = null
  -- double maxValue = -infinity
  -- for (Move move : legalMoves):
    -- w_i = scoresPerMove[move]
    -- t_i = min(visitsPerMove[move], 1)
    -- s_i = (w_i / t_i) + (2 * ln(numIterations) / t_i) ^ 0.5    
    -- if maxValue < s_i:
      -- maxMove = move
      -- maxValue = s_i
  -- return root.apply(maxMove)
  
-- sq :: Int -> Int 
-- sq = sq2 1

-- sq2 :: Int -> Int -> Int 
-- sq2 n nn = 
  -- if n*n <= nn && nn <= n*n-n-n+1
    -- then n
    -- else sq2 (nn//n//2) nn
