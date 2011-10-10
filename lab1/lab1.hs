-- TODO
-- read files from command line
-- use "strip" in clearString instead of "elem"

module Main where

import Data.List (sort, sortBy, group)
import Data.Char (toLower)
import Data.Map (assocs, fromListWith, foldrWithKey)

-- Stores a word and its frequency in the text.
data Token = Token { word :: String, count :: Int } deriving Show

-- Max linewidth of output histogram.
lineWidth = 80

-- Characters stripped off each word.
invalidChars = ",:;.!?"

-- Strips invalid characters off a word.
clearString :: String -> String
clearString s = map toLower $ filter (`notElem` invalidChars) s

-- Given a (long) string, count the frequency of each word (using sort).
countTokensSort :: String -> [Token]
countTokensSort s = map tokenFromGroup (group . sort $ map clearString $ words s)
	where tokenFromGroup g = Token (head g) (length g)

-- Given a (long) string, count the frequency of each word (using Data.Map).
countTokensMap :: String -> [Token]
countTokensMap s = foldrWithKey accumulateToken []
	(fromListWith (+) (zip (map clearString (words s)) (repeat 1)))
	where accumulateToken w n acc = (Token w n):acc

-- Choose which implementation to use.
countTokens = countTokensMap

-- Print a Token as a word and a list of '#' representing its frequency.
-- The length of the line will not exceed "lineWidth".
printToken :: Int -> Int -> Token -> IO ()
printToken maxLength maxCount (Token w c) = 
	if numHashes <= 0 then
		return ()
	else
		putStrLn $
		w ++ 
		(concat $ (replicate numSpaces " ") ++ (replicate numHashes "#"))
	where lenWord = length w;
		  numSpaces = maxLength - lenWord + 1;
		  numHashes = div (lineWidth * c) maxCount - numSpaces - lenWord

-- Print the histogram of a list of Tokens
printTokens :: [Token] -> IO ()
printTokens tokens =
	mapM_ (printToken maxLength maxCount) sortedTokens
	where maxLength = maximum $ map (length . word) tokens
	      maxCount = maximum $ map count tokens
	      cmpTokens t1 t2 = compare (count t2) (count t1)
	      sortedTokens = sortBy cmpTokens tokens

main = do
	words <- getContents
	printTokens $ countTokens words
