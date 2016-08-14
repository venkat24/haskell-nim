--NIM AI in Haskell
import Data.List
import Data.Bits
import Control.Monad

--Perform the move
changeBoard :: [Int] -> Int -> Int -> [Int]
changeBoard [] _ _= []
changeBoard (x:xs) n coins
    | n == 0    = [x-coins] ++ changeBoard xs (n-1) coins
    | otherwise = [x] ++ changeBoard xs (n-1) coins

--Make Computer Move
computerMove board = do
    putStrLn "I play now : "

    let currNimSum = nimSum board
    let movePos = getMovePos board 0
                      where getMovePos [] _ = (-1)
                            getMovePos (x:xs) n
                                | currNimSum `xor` x < x = n
                                | otherwise = getMovePos xs (n+1)

    let badMovePos = getInt $ elemIndex (maximum board) board
                      where getInt Nothing  = 0
                            getInt (Just x) = x
    if (movePos == (-1)) 
        then do         
        let newBoard = changeBoard board badMovePos 1
        print newBoard
        userMove newBoard
        return ()
    else do
        let prevValue = board !! movePos
        let newBoard = changeBoard board movePos (prevValue - moveAmount)
                          where moveAmount = prevValue `xor` currNimSum
        print newBoard
        if checkWinState newBoard then
            putStrLn "COMPUTER WINS!"
        else
            userMove newBoard
        return ()

--Make User Move
userMove board = do
    putStrLn "Make your move!"
    putStrLn "Select the heap number : "
    heapInput <- getLine
    let heap = read heapInput :: Int
    putStrLn "Enter number of coins to take : "
    coinsInput <- getLine
    let coins = read coinsInput :: Int
    let newBoard = changeBoard board (heap-1) coins
    print newBoard
    if checkWinState newBoard then
        putStrLn "YOU WIN!"
    else
        computerMove newBoard
    return ()

--Check whether the game is over
checkWinState :: [Int] -> Bool
checkWinState board
    | sum board == 0 = True
    | otherwise      = False

--Calculate XOR reduction of board
nimSum :: [Int] -> Int
nimSum board = foldl (\acc x -> acc `xor` x) 0 board

--Input the Board
inputBoard :: Int -> [IO Int]
inputBoard 0 = []
inputBoard n = [a | a <- [inputNumber] ] ++ inputBoard (n-1)

--Input any one number
inputNumber::IO Int
inputNumber = do
    n <- getLine
    return (read n)

--Main function
main = do
    putStrLn "Welcome to NIM!"
    putStrLn "Enter the number of heaps : "
    heaps <- inputNumber
    putStrLn "Enter number of coins in each heap (On newlines) : "
    board <- sequence (inputBoard heaps)
    print board
    userMove board
    putStrLn "Game Over!"