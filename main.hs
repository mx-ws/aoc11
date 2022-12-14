import Data.Map as Map
import Control.Monad.State.Lazy
import Data.Maybe
import Data.List
import Data.Char (digitToInt)

data Monkey = Monkey {
    test :: Int,
    op :: Int -> Int,
    items :: [Int],
    throw :: Bool -> Int,
    inspections :: Int
}

type Worry = Map Int Monkey

data MonS = MonS {
    worries :: Int -> Worry,
    monkeys :: Map Int Monkey,
    turn :: Int
}

monRound :: State MonS [()]
monRound = do
    s <- get
    sequence
        $ fmap (thisMonkeyTurn)
        $ (items
            $ fromJust
            $ Map.lookup (turn s)
            $ monkeys s)

-- TODO
thisMonkeyTurn :: Int -> State MonS ()
thisMonkeyTurn item = do
    s <- get
    let w = (const Map.empty )
        m = Map.empty
        t = (turn s + 1 `mod` ((size $ monkeys s) - 1))
    put s
    put $ MonS w m t

parseMonkeys :: State String MonS
parseMonkeys = undefined
-- parseMonkeys = do
--     n <- monkeyNumber
--     items <- startingItems
--     test 

monkeyNumber :: State String Int
monkeyNumber = do
    s <- get
    let (l, _:ls) = break (== '\n') s
        n = digitToInt $ l !! 7
    put ls
    return n

startingItems :: State String [Int]
startingItems = do
    s <- get
    let (l, _:ls) = break (== '\n') s
    put ls
    return $ read $ "[" ++ Data.List.drop 16 l ++ "]"

opP :: State String (Int -> Int)
opP = do
    s <- get
    let (l, _:ls) = break (== '\n') s
    put ls
    let opString = Data.List.drop 19 l
        (first, _:r) = break (==' ') opString
        (operation, _:second) = break (== ' ') r
    return (\x ->
        let a = if first == "old" then x else read first
            b = if second == "old" then x else read second
            op = if operation == "*" then (*) else (+)
        in
            op a b)


testW :: State String Int
testW = do
    s <- get
    let (l, _:ls) = break (== '\n') s
    put ls
    return $ read $ Data.List.drop 19 l

throwTo :: State String Int
throwTo = do
    s <- get
    let (l, _:ls) = break (== '\n') s 
    put ls
    return $ digitToInt $ last l

monkeyBusiness:: Map Int Monkey -> Int
monkeyBusiness mM = (\(a:b:bs) -> a*b) $ sort $ elems $ fmap (inspections) mM

main = do
    input <- readFile("in")
    let s = runState parseMonkeys input
    --     monS = execState (forM_ [1..20] $ const monRound) s
    -- putStrLn $ show $ monkeyBusiness $ monkeys $ monS
    putStrLn $ show $ 5