module Huffman where

import Data.List ( groupBy, sortBy )
import Data.Function(on);
import System.IO(openFile, hGetContents, IOMode (WriteMode), withFile, hPutStr, Handle)
import GHC.IO.IOMode (IOMode(ReadMode))
import GHC.IO.Handle (hClose)

data DataCode a
  = Null
  | Code a String

instance Show a => Show (DataCode a) where
    show (Code item code) = "(" ++ show item ++ "," ++ show code ++ ")"

-- Getters for DataCode
firstData :: DataCode a -> a
firstData Null = error "Its null"
firstData (Code item _) = item

secondData :: DataCode a -> String
secondData Null = ""
secondData (Code _ code) = code;
-- -----------------------------

data HTree a
  = Empty
  | Leaf a Int
  | Fork (HTree a) (HTree a) Int
  deriving (Show)

-- Operations for Huffman Tree
getWeight :: HTree a -> Int
getWeight Empty = 0
getWeight (Leaf _ weight) = weight
getWeight (Fork _ _ weight) = weight

mergeTwoTrees :: HTree a -> HTree a -> HTree a
mergeTwoTrees hTree1 hTree2 = Fork hTree1 hTree2 (getWeight hTree1 + getWeight hTree2)
-- -----------------------------

data DecodeProduct a 
    = Encode String (HTree a)
    deriving (Show)

-- Getters for DecodeProduct
getCode :: DecodeProduct a  -> String
getCode (Encode code _) = code

getTree :: DecodeProduct a -> HTree a
getTree (Encode _ tree) = tree;
-- -----------------------------

-- Usefull Operations 
areEqualString :: String -> String -> Bool
areEqualString s1 s2 = s1 == s2
-- -----------------------------

-- Operations for Frequency List
removeDuplicates :: (Eq a) => [(a, Int)] -> (a -> a -> Bool) -> [(a, Int)]
removeDuplicates [] _ = []
removeDuplicates (x:xs) equal = x : removeDuplicates (filter (\y -> not (fst y `equal` fst x)) xs) equal

countTheSameElements :: Eq a => a -> [a] -> (a -> a -> Bool)-> Int
countTheSameElements _ [] _ = 0
countTheSameElements elem list equal = length (filter (`equal` elem) list)

groupedList :: Eq a => [a] -> (a -> a -> Bool) -> [(a,Int)]
groupedList [] _ = []
groupedList list equal = removeDuplicatesList
    where 
    groupedList = map (\x -> (x, countTheSameElements x list equal) ) list
    removeDuplicatesList = removeDuplicates groupedList equal

sortList :: [(a, Int)] -> [(a, Int)]
sortList = sortBy (compare `on` snd)
-- -- -----------------------------

-- -- Building the Huffman Tree
sortTrees :: [HTree a] -> [HTree a]
sortTrees = sortBy (compare `on` getWeight)

makeAllLeaves :: [(a, Int)] -> [HTree a]
makeAllLeaves = map (uncurry Leaf)

buildTree :: [(a, Int)] -> HTree a
buildTree [] = Empty
buildTree list = build $ makeAllLeaves list
    where
    build [tree] = tree
    build (first:second:xs) = build $ sortTrees $ mergeTwoTrees first second : xs

buildCodesHelper :: HTree a -> String -> [DataCode a]
buildCodesHelper (Leaf item _) path = [Code item path]
buildCodesHelper (Fork left right _) path = buildCodesHelper left (path ++ "0") ++ buildCodesHelper right (path ++ "1")

buildCodes :: HTree a-> [DataCode a]
buildCodes Empty = []
buildCodes tree  = buildCodesHelper tree ""

codeList :: Eq a => [a] -> (a -> a -> Bool) -> [DataCode a]
codeList list equal = buildCodes $ huffmanTree list equal

findInList :: Eq a => a -> [DataCode a] -> (a -> a -> Bool) -> String
findInList _ [] _= ""
findInList x (y:ys) equal= case y of
    Code item code -> if item `equal` x then code else findInList x ys equal
    Null -> findInList x ys equal
-- -- -----------------------------

-- -- Encoding
findEncodeString :: Eq a => [a] -> [DataCode a] -> (a -> a -> Bool) -> String
findEncodeString [] _ _ = []
findEncodeString (x:xs) listWithCodes equal = currentCode ++ findEncodeString xs listWithCodes equal
    where
    currentCode = findInList x listWithCodes equal
-- -- -----------------------------

-- -- Decoding
decodeWrapper :: String -> HTree a -> HTree a -> [a]
decodeWrapper [] (Leaf item _ ) _ = [item]
decodeWrapper [] _ _ = []
decodeWrapper word root mainRoot = case root of
    Leaf symbol _ -> symbol : decodeWrapper word mainRoot mainRoot
    Fork left right _ -> if head word == '0' then decodeWrapper (tail word) left mainRoot else decodeWrapper (tail word) right mainRoot
-- -- -----------------------------

-- -- Operations for Files
readFileToEncode :: FilePath -> FilePath -> IO String
readFileToEncode inputEncode ouputEncode = do
    handle <- openFile inputEncode ReadMode
    contents <- hGetContents handle
    writeFile ouputEncode (getCode (encode contents (==)))
    hClose handle
    putStrLn "Encoding is done!"
    return contents

readFileToMakeHuffmanTree :: Handle  -> IO (HTree Char)
readFileToMakeHuffmanTree handle = do
    contents <- hGetContents handle
    let htree = huffmanTree contents (==)
    putStrLn "HuffmanTree is done!"
    return htree 

readFileToDecode :: FilePath -> FilePath -> FilePath -> IO String
readFileToDecode inputTree inputDecode outputDecode = do
    handle <- openFile inputDecode ReadMode
    contents <- hGetContents handle
    handleTree <- openFile inputTree ReadMode
    htree <- readFileToMakeHuffmanTree handleTree
    writeFile outputDecode (decode (Encode contents htree))
    hClose handleTree
    hClose handle
    putStrLn "Decoding is done!"
    return contents

-- ReadFile Int
readFileToEncodeInt :: FilePath -> FilePath -> IO [Int]
readFileToEncodeInt inputEncode ouputEncode = do
    handle <- openFile inputEncode ReadMode
    contents <- hGetContents handle
    let transformedContens = transform contents :: [Int]
    let htree = huffmanTree transformedContens (==)
    writeFile ouputEncode (getCode (encode transformedContens (==)))
    hClose handle
    putStrLn "IntEncoding is done!"
    return transformedContens

readFileToMakeHuffmanTreeInt :: Handle  -> IO (HTree Int)
readFileToMakeHuffmanTreeInt handle = do
    contents <- hGetContents handle
    let transformedContens = transform contents :: [Int]
    let htree = huffmanTree transformedContens (==)
    putStrLn "IntHuffmanTree is done!"
    return htree  

readFileToDecodeInt :: FilePath -> FilePath -> FilePath -> IO String
readFileToDecodeInt inputTree inputDecode outputDecode = do
    handle <- openFile inputDecode ReadMode
    contents <- hGetContents handle
    handleTree <- openFile inputTree ReadMode
    htree <- readFileToMakeHuffmanTreeInt handleTree
    writeFile outputDecode $ show (decode (Encode contents htree))
    hClose handleTree
    hClose handle
    putStrLn "IntDecoding is done!"
    return contents    

transform :: Read a => String -> [a]
transform list = map read $ words list
-- -- -----------------------------

-- The Most Important Functions
freqList :: Eq a => [a] -> (a -> a -> Bool) -> [(a, Int)]
freqList list equal = sortList $ groupedList list equal

huffmanTree :: Eq a => [a] -> (a -> a -> Bool) -> HTree a
huffmanTree list equal = buildTree $ freqList list equal

encode :: Eq a => [a] -> (a -> a -> Bool) -> DecodeProduct a
encode list equal = Encode code tree
    where
    code = findEncodeString list (codeList list equal) equal
    tree = huffmanTree list equal

decode :: DecodeProduct a -> [a]
decode toEncode = 
    decodeWrapper (getCode toEncode) (getTree toEncode) (getTree toEncode)

-- -- -----------------------------

-- -- Main Function
main = do
    -- putStrLn "Write down a word you want to encode"
    -- line <- getLine
    -- let encoding = encode line (==)
    -- putStrLn "Huffman Tree: "
    -- print (getTree encoding)
    -- putStrLn "Sequence of bits: "
    -- print (getCode encoding)
    -- let finalText = decode encoding
    -- print finalText
    
    -- putStrLn "Start"
    -- firstText <- readFile "D:/Uni/Second_Course/Winter_Sem_2023/FP/Project/textToEncode.txt"

    -- readFileToEncode "D:/Uni/Second_Course/Winter_Sem_2023/FP/Project/textToEncode.txt" 
    --  "D:/Uni/Second_Course/Winter_Sem_2023/FP/Project/textToDecode.txt"

    -- readFileToDecode "D:/Uni/Second_Course/Winter_Sem_2023/FP/Project/textToEncode.txt" 
    --  "D:/Uni/Second_Course/Winter_Sem_2023/FP/Project/textToDecode.txt" 
    --  "D:/Uni/Second_Course/Winter_Sem_2023/FP/Project/finalText.txt"

    -- finalText <- readFile "D:/Uni/Second_Course/Winter_Sem_2023/FP/Project/finalText.txt"
    -- let result = areEqualString firstText finalText
    -- putStrLn $ "Are the two texts equal: " ++ show result

    putStrLn "The other encoding is starting with Int"
    readFileToEncodeInt "D:/Uni/Second_Course/Winter_Sem_2023/FP/Project/textToEncodeInt.txt" "D:/Uni/Second_Course/Winter_Sem_2023/FP/Project/textToDecodeInt.txt"
    readFileToDecodeInt "D:/Uni/Second_Course/Winter_Sem_2023/FP/Project/textToEncodeInt.txt" "D:/Uni/Second_Course/Winter_Sem_2023/FP/Project/textToDecodeInt.txt" "D:/Uni/Second_Course/Winter_Sem_2023/FP/Project/finalTextInt.txt"
    putStrLn "Done"
-- -----------------------------