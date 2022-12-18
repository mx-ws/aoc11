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

type Worry = Map Int (Int, Int)

data MonS = MonS {
    monkeys :: Map Int ModMonkey,
    turn :: Int,
    monRound :: Int
    -- sizeM :: Int
} deriving Show

-- instance Show MonS where
    -- show m = show $ fmap (\n -> show $ fromJust $ Map.lookup n $ monkeys m) [0..7]

data IntMonS = IntMonS {
    iMonkeys :: Map Int IntMonkey,
    iTurn :: Int
}

takeItem :: State MonS (Worry, Monkey)
takeItem = do
    s <- get
    let t = turn s
        r = monRound s
        ms = monkeys s
        m = fromJust $ Map.lookup t $ ms
        (i:is) = items m
        (u,r') = if is == [] then ((t + 1) `nextTurn` (size ms)) r else (t, r)
    put $ MonS (Map.insert t (ModMonkey is $ ((monkey m) { inspections = (inspections $ monkey m) + 1})) ms) u r'
    return (i, monkey m)

nextTurn :: Int -> Int -> Int -> (Int, Int)
nextTurn turn size r = let turnMod = turn `mod` size in
    if turnMod == turn then (turn, r) else (turnMod, r+1)

throwItem :: State MonS ()
-- throwItem = undefined
throwItem = do
    (i, m) <- takeItem
    let t = test m
        f = op m
    -- i' = op auf i anwenden
        i' = mapWithKey (\k (x,n) -> ((f x) `mod` k,n)) i
    -- i'' = div3 auf auf i' anwenden
        i'' = mapWithKey divBy3 i'
    -- i'' testen und throw auf Ergebnis anwenden.
        nextM = case fromJust $ Map.lookup t i'' of
            (0,_)   -> throw m True
            _       -> throw m False
    s <- get
    let oldMonkey = fromJust $ Map.lookup nextM $ monkeys s
        oldMonkeyItems = items oldMonkey
        newMonkeys = Map.insert nextM (oldMonkey {items = oldMonkeyItems ++ [i'']}) $ monkeys s
    put $ MonS (newMonkeys) (turn s) $ monRound s

    -- entsprechendem affen i'' ans ende der liste packen
    return ()

untilRound :: Int -> State MonS ()
untilRound maxRound = do
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
    s <- get
    if monRound s <= maxRound then untilRound maxRound else return ()


-- TODO
thisMonkeyTurn :: Int -> State MonS ()
thisMonkeyTurn item = do
    s <- get
    let m = Map.empty
        t = (turn s + 1 `mod` ((size $ monkeys s) - 1))
    put s
    put $ MonS m t (monRound s)


-- Restklassen

divBy3 :: Int -> (Int, Int) -> (Int, Int)
divBy3 mod3 (l, n) = ((l - mod3) * n, n)

-- parsing

-- main parsing function
parseMonS :: State String MonS
parseMonS = do
    ms <- parseMonkeys Map.empty
    let tests = Map.elems $ fmap (test.iMonkey) ms
        modMonkeys = fmap (intToModMonkey tests) ms
    return $ MonS modMonkeys 0 0

intToModItem :: [Int] -> Int -> Worry
intToModItem [] n = Map.singleton 3 (n `mod` 3, undefined)
intToModItem (x:xs) n = let w = intToModItem xs n
                            (_, inv3) = gcdExt 3 x in
    Map.insert x (n `mod` x, inv3) w

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
    let monS = evalState parseMonS input
        monSFinal = execState (untilRound 21) monS
        mB = monkeyBusiness $ fmap monkey $ monkeys monSFinal
    -- let s = evalState (parseMonkey Map.empty) input
    --     monS = execState (forM_ [1..20] $ const monRound) s
    -- putStrLn $ show $ monkeyBusiness $ monkeys $ monS
    putStrLn $ show $ mB