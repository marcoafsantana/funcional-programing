import Data.Char (chr, isLower, ord)
import Data.List (elemIndex)

-- frequency tagle
table :: [Float]
table =
  [ 8.1,
    1.5,
    2.8,
    4.2,
    12.7,
    2.2,
    2.0,
    6.1,
    7.0,
    0.2,
    0.8,
    4.0,
    2.4,
    6.7,
    7.5,
    1.9,
    0.1,
    6.0,
    6.3,
    9.0,
    2.8,
    1.0,
    2.4,
    0.2,
    2.0,
    0.1
  ]

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | otherwise = c

encode :: Int -> String -> String
encode n s = [shift n c | c <- s]

-- decode
count :: (Eq a) => a -> [a] -> Int
count x = length . filter (== x)

lowers :: String -> String
lowers = filter isLower

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs s = [percent (count c ls) n | c <- ['a' .. 'z']]
  where
    ls = lowers s
    n = length ls

-- sum (os_i - es_i)^2 / es_i
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack s = encode (-factor) s
  where
    table' = freqs s
    chis = [chisqr (rotate n table') table | n <- [0 .. 25]]
    factor = case elemIndex (minimum chis) chis of
      Just n -> n
      Nothing -> 0

main :: IO ()
main = do
  putStrLn "#### testing ceasar cipher ####"

  let encoded = encode 3 "haskell is fun"
  putStrLn $ "encoding 'haskell is fun' com fator 3: " ++ encoded

  let decoded = crack "kdvnhoo lv ixq"
  putStrLn $ "decoding 'kdvnhoo lv ixq': " ++ decoded

  let complexEncoded = encode 5 "the quick brown fox jumps over the lazy dog"
  putStrLn $ "\n original: the quick brown fox jumps over the lazy dog"
  putStrLn $ "encoded with factor 5: " ++ complexEncoded
  putStrLn $ "'crack' result: " ++ crack complexEncoded