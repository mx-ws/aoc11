import Data.Map as Map
import Control.Monad.State.Lazy
import Data.Maybe
import Data.List (sort)

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

parseMonkeys :: String -> MonS
parseMonkeys = undefined

monkeyBusiness:: Map Int Monkey -> Int
monkeyBusiness mM = (\(a:b:bs) -> a*b) $ sort $ elems $ fmap (inspections) mM

main = do
    input <- readFile("in")
    let s = parseMonkeys input
    --     monS = execState (forM_ [1..20] $ const monRound) s
    -- putStrLn $ show $ monkeyBusiness $ monkeys $ monS
    putStrLn $ show $ 5