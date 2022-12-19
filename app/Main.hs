import Data.Map as Map
import Control.Monad.State.Lazy
import Data.Maybe
import Data.List
import Data.Char (digitToInt)
import Data.Euclidean

data Monkey = Monkey {
    op :: Op,
    test :: Int,
    throw :: Throw,
    inspections :: Int
} deriving (Eq)

instance Show Monkey where
    show = show.test

data IntMonkey = IntMonkey {
    iItems :: [Int],
    iMonkey :: Monkey
} deriving Show

newtype Op = Op (Int -> Int)
newtype Throw = Throw (Bool -> Int)

unOp (Op x) = x
unThrow (Throw x) = x

instance Eq Op where
    f == g = and $ fmap (\x -> unOp f x == unOp g x) [1..10]

instance Eq Throw where
    f == g = and [unThrow f True == unThrow g True, unThrow f False == unThrow g False]

data ModMonkey = ModMonkey {
    items :: [Worry],
    monkey :: Monkey
} deriving (Eq)

instance Show ModMonkey where
    show m = show $ fmap show $ items m

type Worry = Map Int Int

data MonS = MonS {
    monkeys :: Map Int ModMonkey,
    turn :: Int,
    monRound :: Int
    -- sizeM :: Int
} deriving (Show, Eq)

-- instance Show MonS where
    -- show m = show $ fmap (\n -> show $ fromJust $ Map.lookup n $ monkeys m) [0..7]

data IntMonS = IntMonS {
    iMonkeys :: Map Int IntMonkey,
    iTurn :: Int,
    iMonRound :: Int
}

takeItem :: State MonS (Worry, Monkey)
takeItem = do
    s <- get
    let t = turn s
        r = monRound s
        ms = monkeys s
        m = fromJust $ Map.lookup t $ ms
        (i:is) = items m
    put $ MonS (Map.insert t (ModMonkey is $ ((monkey m) { inspections = (inspections $ monkey m) + 1})) ms) t r
    return (i, monkey m)

nextTurn :: Int -> Int -> Int -> (Int, Int)
nextTurn turn size r = let turnMod = turn `mod` size in
    if turnMod == turn then (turn, r) else (turnMod, r+1)

throwItem :: State MonS ()
-- throwItem = undefined
throwItem = do
    (i, m) <- takeItem
    let t = test m
        f = unOp $ op m
    -- i' = op auf i anwenden
        i' = mapWithKey (\k x -> (f x) `mod` k) i
    -- i testen und throw auf Ergebnis anwenden.
        nextM = case fromJust $ Map.lookup t i' of
            0   -> unThrow (throw m) True
            _   -> unThrow (throw m) False
    s <- get
    let oldMonkey = fromJust $ Map.lookup nextM $ monkeys s
        oldMonkeyItems = items oldMonkey
        newMonkeys = Map.insert nextM (ModMonkey (oldMonkeyItems ++ [i']) (monkey oldMonkey)) $ monkeys s
    put $ MonS (newMonkeys) (turn s) $ monRound s

    -- entsprechendem affen i'' ans ende der liste packen
    return ()

untilRound :: Int -> State MonS ()
untilRound maxRound = do
    tryItem
    s <- get    
    if monRound s <= maxRound then untilRound maxRound else return ()

tryItem = do
    s' <- get
    let t = turn s'
        r = monRound s'
        ms = monkeys s'
        m = fromJust $ Map.lookup t $ ms
    if items m == [] then do
        let (u, r') = ((t + 1) `nextTurn` (size ms)) r
        put $ s' { turn = u, monRound = r'}
    else do
        throwItem


-- Int version
takeItemInt :: State IntMonS (Int, Monkey)
takeItemInt = do
    s <- get
    let t = iTurn s
        r = iMonRound s
        ms = iMonkeys s
        m = fromJust $ Map.lookup t $ ms
        (i:is) = iItems m
    put $ IntMonS (Map.insert t (IntMonkey is $ ((iMonkey m) { inspections = (inspections $ iMonkey m) + 1})) ms) t r
    return (i, iMonkey m)

throwItemInt :: State IntMonS ()
-- throwItem = undefined
throwItemInt = do
    (i, m) <- takeItemInt
    let t = test m
        f = unOp $ op m
    -- i' = op auf i anwenden
        i' = (`div` 3) $ f $ i
        nextM = case i' `mod` t of
            0   -> unThrow (throw m) True
            _   -> unThrow (throw m) False
    s <- get
    let oldMonkey = fromJust $ Map.lookup nextM $ iMonkeys s
        oldMonkeyItems = iItems oldMonkey
        newMonkeys = Map.insert nextM (IntMonkey (oldMonkeyItems ++ [i']) (iMonkey oldMonkey)) $ iMonkeys s
    put $ IntMonS (newMonkeys) (iTurn s) $ iMonRound s

    -- entsprechendem affen i'' ans ende der liste packen
    return ()

untilRoundInt :: Int -> State IntMonS ()
untilRoundInt maxRound = do
    tryItemInt
    s <- get    
    if iMonRound s <= maxRound then untilRoundInt maxRound else return ()

tryItemInt = do
    s' <- get
    let t = iTurn s'
        r = iMonRound s'
        ms = iMonkeys s'
        m = fromJust $ Map.lookup t $ ms
    if iItems m == [] then do
        let (u, r') = ((t + 1) `nextTurn` (size ms)) r
        put $ s' { iTurn = u, iMonRound = r'}
    else do
        throwItemInt


-- Restklassen

--NOTUSABLE
dONTUSEdivBy3 :: Int -> Int -> (Int, Int) -> (Int, Int)
dONTUSEdivBy3 mod3 k (l, n) = (((l - mod3) * n) `mod` k, n)

-- parsing

-- main parsing function
parseMonS :: State String MonS
parseMonS = do
    ms <- parseMonkeys Map.empty
    let tests = Map.elems $ fmap (test.iMonkey) ms
        modMonkeys = fmap (intToModMonkey tests) ms
    return $ MonS modMonkeys 0 1

-- main IntParsing function
parseIntMonS :: State String IntMonS
parseIntMonS = do
    ms <- parseMonkeys Map.empty
    return $ IntMonS ms 0 1

intToModItem :: [Int] -> Int -> Worry
intToModItem [] n = Map.empty
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

    return $ Map.insert n (IntMonkey items $ Monkey (Op op) test (Throw throw) 0) iM


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
monkeyBusiness mM = (\(a:b:bs) -> a*b) $ reverse $ sort $ elems $ fmap (inspections) mM

numberOfItems :: MonS -> Int
numberOfItems s = sum $ fmap (\ms -> length $ items ms) $ monkeys s

ofItems s = elems $ fmap items $ monkeys s

main = do
    input <- readFile("in")
    -- mod version
    let monS = evalState parseMonS input
        monSFinal = execState (untilRound 10000) monS
        mB = monkeyBusiness $ fmap monkey $ monkeys monSFinal

    -- int version
    -- let intMonS = evalState parseIntMonS input
    --     intMonSFinal = execState (untilRoundInt 20) intMonS
    --     mB = monkeyBusiness $ fmap iMonkey $ iMonkeys intMonSFinal

    return mB
