-- TODO
-- files from command line
-- strip in clearString

module Main where

import Data.List (sort, sortBy, group)
import Data.Char (toLower)
import Data.Map (assocs, fromListWith, foldrWithKey)

data Token = Token { word :: String, count :: Int } deriving Show

lineWidth = 80
invalidChars = ",:;.!?"

clearString :: String -> String
clearString s = map toLower $ filter (`notElem` invalidChars) s

-- Using sort
countTokensSort :: String -> [Token]
countTokensSort s = map 
	(\x -> Token (head x) (length x)) 
	(group . sort $ map clearString $ words s)

-- Using Data.Map
countTokensMap :: String -> [Token]
countTokensMap s = foldrWithKey
	(\w n acc -> (Token w n):acc)
	[]
	(fromListWith (+) (zip (map clearString (words s)) (repeat 1)))

countTokens = countTokensMap

printToken :: Int -> Int -> Token -> IO ()
printToken maxLength maxCount (Token w c) = 
	let lenWord = length w;
		numSpaces = maxLength - lenWord + 1;
		numHashes = div (lineWidth * c) maxCount - numSpaces - lenWord
	in if numHashes <= 0 then
		return ()
	else
		putStrLn $
		w ++ 
		(concat $ (replicate numSpaces " ") ++ (replicate numHashes "#"))

printTokens :: [Token] -> IO ()
printTokens tokens =
	let maxLength = maximum $ map (length . word) tokens
	    maxCount = maximum $ map count tokens
	    sortedTokens = sortBy (\t1 t2 -> compare (count t2) (count t1)) tokens
	in mapM_ (printToken maxLength maxCount) sortedTokens

main = do
	words <- getContents
	printTokens $ countTokens words
