import qualified Data.List as L
import qualified Data.Char as C

sonnet18 = "Shall I compare thee to a summer’s day? \n"
  ++ "Thou art more lovely and more temperate: \n"
  ++ "Rough winds do shake the darling buds of May, \n"
  ++ "And summer’s lease hath all too short a date: \n"
  ++ "Sometime too hot the eye of heaven shines, \n"
  ++ "And often is his gold complexion dimm’d; \n"
  ++ "And every fair from fair sometime declines, \n"
  ++ "By chance or nature’s changing course untrimm’d; \n"
  ++ "But thy eternal summer shall not fade \n"
  ++ "Nor lose possession of that fair thou owest; \n"
  ++ "Nor shall Death brag thou wander’st in his shade, \n"
  ++ "When in eternal lines to time thou growest: \n"
  ++ "  So long as men can breathe or eyes can see, \n"
  ++ "  So long lives this and this gives life to thee."

sonnet73 = "That time of year thou mayst in me behold\n"
  ++ "When yellow leaves, or none, or few, do hang\n"
  ++ "Upon those boughs which shake against the cold,\n"
  ++ "Bare ruin’d choirs, where late the sweet birds sang.\n"
  ++ "In me thou seest the twilight of such day\n"
  ++ "As after sunset fadeth in the west,\n"
  ++ "Which by and by black night doth take away,\n"
  ++ "Death’s second self, that seals up all in rest.\n"
  ++ "In me thou see’st the glowing of such fire\n"
  ++ "That on the ashes of his youth doth lie,\n"
  ++ "As the death-bed whereon it must expire\n"
  ++ "Consumed with that which it was nourish’d by.\n"
  ++ "This thou perceivest, which makes thy love more strong,\n"
  ++ "To love that well which thou must leave ere long."

count :: Eq a => a -> [a] -> Int
count x [] = 0
count x (y:ys) | x == y = succ (count x ys)
               | otherwise = count x ys

average :: [Int] -> Float
average [] = error "Empty List"
average xs = fromRational (toRational (sum xs) / toRational (length xs))

averageLength :: String -> Float
averageLength str = average $ map length $ words str

prefix :: Eq a => [a] -> [a] -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x==y) && prefix xs ys

sublist :: Eq a => [a] -> [a] -> Bool
sublist [] ys = True
sublist xs [] = False
sublist xs ys = (prefix xs ys) || sublist xs (tail ys)

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (\y -> (y /= x)) xs)

-- to remove everything but alphanumeric characters
preprocess :: String -> String
preprocess = (map C.toLower) . filter (`notElem` "?;:.,")


-- to text process given sonnets
process :: String -> [String]
process = L.sort . nub . words

cnt :: String -> [(String, Int)]
cnt sonnet = [ (x,y) | x <- (process . preprocess) sonnet,
               y <- [count x ((words . preprocess) sonnet)],
               y > 1]
