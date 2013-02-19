import Control.Monad  
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as Map


main = do
	numEntries <- getLine
	processLine 1

processLine i = do 
		l <- getLine
		putStrLn $ "Case #" ++ (show i) ++ ": " ++ (show $ alienSolve l)
		processLine (i + 1)

powerlist n = 1 : map (*n) (powerlist n)

alienSolve :: String -> Int
alienSolve alien = foldr1 (+) [x * i | (x, i) <- zip converted (powerlist base)]
	where   converted = reverse [fromJust (Map.lookup a mapping) | a <- alien]
		mapping = Map.fromList $ (head alien, 1) : (zip (tail symbolList) (0 : [2..]))
		base = max 2 (length symbolList)
		symbolList = nub alien
