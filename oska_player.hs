{- OSKA PLAYER CPSC 312
 - Name:             
 - Ke Er Xiong        (primarily responsible for move generator)
 - Taranbir Bhullar   (primarily responsible for static evaluator)
 -                    (jointly responsible for minimax algorithm)
 -}

-- TOP LEVEL FUNCTION ------------------------------------------------------------------------------
-- given a board a side and the depth of search, produce the best move possible.
-- Depth cannot be 0

oska_y5q8 :: [String] -> Char -> Int -> [String]
oska_y5q8 board side depth = 
    select_y5q8 (zip (map (minimax_y5q8 side depth True) moves) moves)
    where moves = gen_y5q8 board side


-- MAIN HELPERS -----------------------------------------------------------------------------------

-- select best value in the end

select_y5q8 :: [(Int,[String])] -> [String]
select_y5q8 valboards = 
    findState valboards
    where
        bestValue = maximum (fst (unzip valboards))
        findState ((val,board):vbs)
            | val == bestValue = board
            | otherwise        = findState vbs

-- evaluate a board by looking ahead given the depth of search
-- Note that in a zero sum game, minimizing the maximum losses is equivalent to 
-- maximizing the minimum gain.
-- evaluate all boards for our side and produce the board with the max evaluation 
-- select best (minimax)


minimax_y5q8 :: Char -> Int -> Bool-> [String] -> Int

minimax_y5q8 side 1 toggle board = eval_y5q8 side board
minimax_y5q8 side n toggle board 
    | toggle     = minimum (map (minimax_y5q8 side (n-1) (not toggle)) 
                                (gen_y5q8 board (switch_y5q8 side)))
    | otherwise  = maximum (map (minimax_y5q8 side (n-1) (not toggle)) 
                                (gen_y5q8 board side))

                                 
-- static evaluator function
-- The length currState is just a precuation so that if the board is to ever get 
-- a large number of pieces it should be able to still produce a winnning state value
-- given a board and a side, evaluate the board for the side
-- evaluator for white
-- WIN == 5857 * length currState
-- LOSS == -5857 * length currState
-- Since the evaluator cannot look ahead no good way to impliment draw where no one can move 
eval_y5q8 :: Char -> [String] -> Int
eval_y5q8 'w' board 
  | winBoard_y5q8 board 'w' || numOfB == 0            = (5857 * length board)
  | winBoard_y5q8 (reverse board) 'b' || numOfW == 0  = (-5857 * length board)
  | otherwise                                         = advantageW - advantageB
  where
    numOfW = boardReader_y5q8 board 'w' 0
    numOfB = boardReader_y5q8 board 'b' 0
    advantageW = advantage_y5q8 (reverse board) 'w' 0
    advantageB = advantage_y5q8 (reverse board) 'b' 0
-- evaluator for black  
eval_y5q8 'b' board = eval_y5q8 'w' (changeP_y5q8 board) 


-- move generator
-- generate all possible move for side given the current board
gen_y5q8 :: [String] -> Char -> [[String]]
gen_y5q8 board 'w' 
    | null moves = [board] 
    | otherwise  = moves
    where 
        moves = (gen_c_y5q8 board) ++ (gen_f_y5q8 board) 
gen_y5q8 board 'b' = map changeP_y5q8 (gen_y5q8 (changeP_y5q8 board) 'w')



-- OTHER HELPERS -----------------------------------------------------------------------------------

-- HELPERS FOR EVALUATING BOARD -------------------------------------------------------------

--reads everypieve on a board
boardReader_y5q8:: [String] -> Char -> Int -> Int
boardReader_y5q8 (headCS:tailCS) player numOfPieces
  | null tailCS = rowReader_y5q8 headCS player  
  | otherwise = numOfPieces + (rowReader_y5q8 headCS player)
                            + boardReader_y5q8 tailCS player numOfPieces


-- Reads number of pieces of w or b 
rowReader_y5q8 :: String -> Char -> Int
rowReader_y5q8 [] x  = 0
rowReader_y5q8 (y:xs) x
  | y == x = 1 + rowReader_y5q8 xs x
  | otherwise = rowReader_y5q8 xs x 


winBoard_y5q8 :: [String] -> Char -> Bool
winBoard_y5q8 (x:xs) player
  | (elem player x) && (null xs) = True
  | elem player x = False
  | otherwise = winBoard_y5q8 xs player
winBoard_y5q8 [] player = False


advantage_y5q8 :: [String] -> Char -> Int -> Int
advantage_y5q8 (x:xs) player tavantage
  | null xs       = tavantage + (rowReader_y5q8 x player)
  | otherwise     = advantage_y5q8 xs player (tavantage + advantagePerLine)
  where
    advantagePerLine = ((rowReader_y5q8 x player)* 2^(length xs))


-- HELPERS FOR GENERATING MOVES -------------------------------------------------------------


-- generate all forward moves
gen_f_y5q8 :: [String] -> [[String]]
gen_f_y5q8 board =
    (f_y5q8 board) ++ (map (flip_y5q8) (f_y5q8 (flip_y5q8 board)))


-- move forward in one direction
f_y5q8 :: [String] -> [[String]]
f_y5q8 board = 
    filter (\x -> not (null x)) (map (\(_,r,n) -> forward r n) needCheck)
    where 
        forward = forward_y5q8 board
        needCheck = findW_y5q8 board


-- move forward
-- move a white piece at (r,n) forward on a board
forward_y5q8 :: [String] -> Int -> Int -> [String]
forward_y5q8 board r n 
    | f_able_y5q8 board r n = setP_y5q8 (setP_y5q8 board '-' r n) 'w' (r+1) n
    | otherwise             = []


-- determine if the piece at (r,n) on a board can be moved forward on white's turn
f_able_y5q8 :: [String] -> Int -> Int -> Bool
f_able_y5q8 board r n = (valid_y5q8 board (r+1) n) && ((getP_y5q8 board (r+1) n) == '-')


-- generate all capture moves
gen_c_y5q8 :: [String] -> [[String]]
gen_c_y5q8 board = 
    (c_y5q8 board) ++ (map (flip_y5q8) (c_y5q8 (flip_y5q8 board)))


-- capture in one direction
c_y5q8 :: [String] -> [[String]]
c_y5q8 board =
    filter (\x -> not (null x)) (map (\(_,r,n) -> capture r n) needCheck)
    where 
        capture = capture_y5q8 board (special_y5q8 board) 
        needCheck = findW_y5q8 board


-- determine the special row for capture on a board
special_y5q8 :: [String] -> Int
special_y5q8 board = ((length board) `div` 2) - 1 


-- capture if possible
capture_y5q8 :: [String] -> Int -> Int -> Int -> [String]
capture_y5q8 board s r n 
    | (s /= r) && (c_able_r_y5q8 board r n)  = setR r n  
    | (s == r) && (c_able_s_y5q8 board r n)  = setS r n 
    | otherwise                              = []
    where 
        setR r n = setP_y5q8 (setP_y5q8 (setP_y5q8 board '-' r n) '-' (r+1) n) 'w' (r+2) n
        setS r n = setP_y5q8 (setP_y5q8 (setP_y5q8 board '-' r n) '-' (r+1) n) 'w' (r+2) (n+1)


-- determine if the piece at (r,n) on a board can be captured for regular rows
c_able_r_y5q8 :: [String] -> Int -> Int -> Bool
c_able_r_y5q8 board r n = 
    and [(valid_y5q8 board (r+1) n),
         (valid_y5q8 board (r+2) n),
         ((getP_y5q8 board (r+1) n) == 'b'),
         ((getP_y5q8 board (r+2) n) == '-')] 


-- determine if the piece at (r,n) on a board can be captured for regular rows
c_able_s_y5q8 :: [String] -> Int -> Int -> Bool
c_able_s_y5q8 board r n =
    and [(valid_y5q8 board (r+1) n),
         (valid_y5q8 board (r+2) (n+1)),
         ((getP_y5q8 board (r+1) n) == 'b'),
         ((getP_y5q8 board (r+2) (n+1)) == '-')] 


-- GENERAL HELPERS --------------------------------------------------------------------------


-- find all white pieces
findW_y5q8 :: [String] -> [(Char, Int, Int)]
findW_y5q8 board = 
    filter (\(p,_,_) -> p == 'w') elements
    where 
        elements = concatMap (\(row,r) -> zip3 row (repeat r) [0..]) (zip board [0..])


-- switch the side
switch_y5q8 :: Char -> Char
switch_y5q8 'b' = 'w'
switch_y5q8 'w' = 'b'
switch_y5q8  x  =  x


-- determine if (r,n) is a valid position on the board
-- index starts with 0
valid_y5q8 :: [String] -> Int -> Int -> Bool
valid_y5q8 board r n 
  | r >= length board        = False
  | n >= length (board !! r) = False
  | otherwise                = True


-- get the piece at (r,n) 
-- Precondition: (r,n) is a valid position on the board
-- index starts with 0
getP_y5q8 :: [String] -> Int -> Int -> Char
getP_y5q8 board r n = (board !! r) !! n


-- set (r,n) to piece on a board
-- Precondition: (r,n) is a valid position on the board
setP_y5q8 :: [String] -> Char -> Int -> Int -> [String]
setP_y5q8 board piece r n = 
    subIn (subIn piece n (board !! r)) r board
    where 
        subIn sub 0 (c:cs) = sub:cs
        subIn sub n (c:cs) = c:(subIn sub (n - 1) cs)


-- change the perspective of the board
-- this function make a lot of functions easier because we only need to write 
-- functions for white
changeP_y5q8 :: [String] -> [String]
changeP_y5q8 board = 
  reverse (map (map switch_y5q8) board)


-- flip the board
-- this function make a lot of functions easier because we only need to write
-- functions to generate moves in one way
flip_y5q8 :: [String] -> [String]
flip_y5q8 board =
    map reverse board







------------------------------------------------------------------------------------------------------
{-

-- for testing purposes -- TO BE DELETED
sample1 = ["wwww","---","--","---","bbbb"]
sample2 = ["www-","--w","--","-b-","b-bb"]
sample3 = ["-ww-","w-w","--","-b-","b-bb"]
sample4 = ["-ww-","--w","w-","-b-","b-bb"]
sample5 = ["-www-","w---","b-w","bb","---","b---","----b"]
sample6 = ["----","--b","--","b-w","--ww"] 

-}