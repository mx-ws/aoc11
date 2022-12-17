import Data.Map as Map
import Control.Monad.State.Lazy
import Data.Maybe
import Data.List
import Data.Char (digitToInt)
import Data.Euclidean

data Monkey = Monkey {
    op :: Int -> Int,
    test :: Int,
    throw :: Bool -> Int,
    inspections :: Int
}

instance Show Monkey where
    show = show.test

data IntMonkey = IntMonkey {
    iItems :: [Int],
    iMonkey :: Monkey
} deriving Show

data ModMonkey = ModMonkey {
    items :: [Worry],
    monkey :: Monkey
}

instance Show ModMonkey where
    show m = show $ fmap show $ items m

type Worry = Map Int Int

data MonS = MonS {
    monkeys :: Map Int ModMonkey,
    turn :: Int
} deriving Show

-- instance Show MonS where
    -- show m = show $ fmap (\n -> show $ fromJust $ Map.lookup n $ monkeys m) [0..7]

data IntMonS = IntMonS {
    iMonkeys :: Map Int IntMonkey,
    iTurn :: Int
}

monRound :: State MonS [()]
monRound = undefined
-- monRound = do
--     s <- get
--     sequence
--         $ fmap (thisMonkeyTurn)
--         $ (items
--             $ fromJust
--             $ Map.lookup (turn s)
--             $ monkeys s)

-- TODO
thisMonkeyTurn :: Int -> State MonS ()
thisMonkeyTurn item = do
    s <- get
    let m = Map.empty
        t = (turn s + 1 `mod` ((size $ monkeys s) - 1))
    put s
    put $ MonS m t


-- Restklassen


ggT :: Int -> Int -> (Int, Int, Int)
ggT a b = undefined

-- parsing

-- main parsing function
parseMonS :: State String MonS
parseMonS = do
    ms <- parseMonkeys Map.empty
    let tests = Map.elems $ fmap (test.iMonkey) ms
        modMonkeys = fmap (intToModMonkey tests) ms
    return $ MonS modMonkeys 0


intToModItem :: [Int] -> Int -> Worry
intToModItem [] _ = Map.empty
intToModItem (x:xs) n = let w = intToModItem xs n in
    Map.insert x (n `mod` x) w

intToModMonkey :: [Int] -> IntMonkey -> ModMonkey
intToModMonkey l m = ModMonkey (fmap (intToModItem l) $ iItems m) $ iMonkey m

parseMonkeys :: (Map Int IntMonkey) -> State String (Map Int IntMonkey)
parseMonkeys iM = do
    s <- get
    case s of   []  -> return iM
                _   -> do
                        ignoreEmptyLine
                        iM' <- parseMonkey iM
                        parseMonkeys iM'


ignoreEmptyLine :: State String ()
ignoreEmptyLine = do
    s <- get
    let (l, _:ls) = break (== '\n') s
    put $ if l == "" then ls else s


parseMonkey :: (Map Int IntMonkey) -> State String (Map Int IntMonkey)
parseMonkey iM = do
    n <- monkeyNumber
    items <- startingItems
    op <- opP
    test <- testW
    throwT <- throwTo
    throwF <- throwTo
    let throw True = throwT
        throw False = throwF

    return $ Map.insert n (IntMonkey items $ Monkey op test throw 0) iM


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
    return $ read $ "[" ++ Data.List.drop 18 l ++ "]"

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
    return $ read $ Data.List.drop 21 l

throwTo :: State String Int
throwTo = do
    s <- get
    let (l, _:ls) = break (== '\n') s 
    put ls
    return $ digitToInt $ last l

-- parsing end


monkeyBusiness:: Map Int Monkey -> Int
monkeyBusiness mM = (\(a:b:bs) -> a*b) $ sort $ elems $ fmap (inspections) mM

main = do
    input <- readFile("in")
    let s = evalState parseMonS input
    -- let s = evalState (parseMonkey Map.empty) input
    --     monS = execState (forM_ [1..20] $ const monRound) s
    -- putStrLn $ show $ monkeyBusiness $ monkeys $ monS
    putStrLn $ show $ s
    -- putStrLn "end"
    return s