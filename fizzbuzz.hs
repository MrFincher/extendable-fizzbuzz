import Control.Monad
import Data.Maybe
import Data.Foldable

-- getLines until an empty line is entered
-- rules are created and applied, output is printed
main :: IO ()
main = do
  rules <- map toRule <$> untilM (=="") getLine
  mapM_ putStrLn $ fizzbuzz rules [1..20]

type Rule = Int -> Maybe String

-- convert a string to a rule
toRule :: String -> Rule
toRule = liftM2 rule (read . head) (concat . tail) . words

-- construct a rule
rule :: Int -> String -> Rule
rule k w x = guard (mod x k == 0) >> Just w

-- apply rules to list of ints
fizzbuzz :: [Rule] -> [Int] -> [String]
fizzbuzz rules = map (fromMaybe <$> show <*> fold rules)

-- run a monadic computation until the given predicate returns ture
untilM :: Monad m => (a -> Bool) -> m a -> m [a]
untilM p a = a >>= \r -> if p r then return [] else (r:) <$> untilM p a