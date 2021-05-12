import Data.List

-- anagrams

type Code' = [(Char, Int)]
type Code = String

-- insert char into list if not present already. keep it sorted.
insertUnique :: Ord a => [a] -> a -> [a]
insertUnique [] c = [c]
insertUnique (x:xs) c 
  | c < x     = c:x:xs
  | c > x     = x:(insertUnique xs c)
  | otherwise = x:xs

unique :: Ord a => [a] -> [a]
unique = foldl insertUnique []

count :: Char -> String -> Int
count c = foldr (\x acc -> if x==c then (acc + 1) else acc) 0 

word2code' :: [Char] -> Code'
word2code' word = [(c, count c word) | c <- unique word]

code2nice :: Code' -> Code
code2nice = foldl (\acc tup -> acc ++ [fst tup] ++ (show (snd tup))) ""

word2code :: [Char] -> Code
word2code = code2nice . word2code'

ana :: String -> String -> Bool
ana w1 w2 = word2code w1 == word2code w2

-- from list of any type, find all possible sublists, where order is ignored
sublists :: [a] -> [[a]]
sublists list = go [] list 
  where go :: [[a]] -> [a] -> [[a]]
        go acc [] = acc
        go acc (x:xs) = go (acc ++ [[x]] ++ [a ++ [x] | a <- acc]) xs

-- create code for every sublist
taggedSublists :: [String] -> [(Code, [String])]
taggedSublists words = zip (map (word2code . concat) subs) subs
  where subs = sublists words

-- get all unique codes
allCodes :: [(Code, [String])] -> [Code]
allCodes ts = unique $ map fst ts

-- find all with given code
findByCode :: Code -> [(Code, [String])] -> [(Code, [String])]
findByCode c = filter (\t -> fst t == c)

-- find only the ones with disjoint wordlists...
findByCode' c = go [] xs 
  where go acc [] = acc
        go acc (x:xs) 
          | fst x == c && disjoint (snd x) acc = go (x:acc) xs
          | otherwise                           = go acc xs

disjoint :: [a] -> [[a]] -> Bool
disjoint xs ys = all (\y -> all (\x -> notin x y) xs) ys

notin :: a -> [a] -> Bool
notin = not elem

findAnagrams words = [findByCode code tagged| code <- uniqueCodes] 
  where tagged = taggedSublists words
        uniqueCodes = allCodes tagged

onlyAnas words = filter (\x -> length x > 1) (findAnagrams words)

onlyWords words = map (\xs -> map snd xs) (onlyAnas words)


findanas words = [filter (\tup -> code == fst tup && sublist /= snd tup) tagged| (code, sublist) <- tagged]
  where tagged = taggedSublists words
        filtered = undefined

findanas2 = filter (\x -> length x > 1) . findanas




findanas' words = [map ((intercalate " ") . snd) (filter (\tup -> code == fst tup && sublist /= snd tup) tagged)| (code, sublist) <- tagged]
  where tagged = taggedSublists words

findanas2' = filter (\x -> length x > 1) . findanas'












