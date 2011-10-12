-- TODO
-- read files from command line
-- use "strip" in clearString instead of "elem"

module Main where

import Data.List (sort, sortBy, group)
import Data.Char (toLower)
import Data.Map (Map, empty, foldrWithKey, insertWith')
import Data.Foldable (foldl')

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
	(mapFromList (map clearString (words s)))
	where accumulateToken w n acc = (Token w n):acc

-- Create a map from a list of words
mapFromList :: [String] -> Map String Int
mapFromList words = foldl' ins empty words
	where ins m w = insertWith' (+) w 1 m

-- Choose which implementation to use.
countTokens = countTokensMap

-- Print a Token as a word and a list of '#' representing its frequency.
-- The length of the line will not exceed "lineWidth".
tokenToString :: Int -> Int -> Token -> String
tokenToString maxLength maxCount (Token w c) = 
	w ++ (concat $ (replicate numSpaces " ") ++ (replicate numHashes "#"))
	where lenWord = length w
	      numSpaces = maxLength - lenWord + 1
	      numHashes = div ((lineWidth - numSpaces - lenWord) * c) maxCount

-- Print the histogram of a list of Tokens
tokensToString:: [Token] -> String
tokensToString tokens =
	unlines (map (tokenToString maxLength maxCount) sortedTokens)
	where maxLength = maximum $ map (length . word) tokens
	      maxCount = maximum $ map count tokens
	      cmpTokens t1 t2 = compare (count t2) (count t1)
	      sortedTokens = sortBy cmpTokens tokens

main = do
	words <- getContents
	putStrLn $ tokensToString $ countTokens words
