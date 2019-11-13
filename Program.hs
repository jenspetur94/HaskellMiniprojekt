import Data.Char
import Data.List
import Data.Function
data BTree = BLeaf (Char, Integer) | BBranch (Integer, BTree, BTree) 
    deriving(Show)

-- returns the weight of a leaf or branch
weight (BLeaf (_, w)) = w
weight (BBranch (w, _, _)) = w


--function that take a string as input and creates a Binary tree with the optimal prefix codes as leafs
createTree :: [Char] -> BTree
createTree str = 
    buildHuffmanTree leafLst
    where
        leafLst = convertToLeafs fLst
        fLst = countLetters str

-- counts the frequency of characters in a string
-- takes a string as input
-- outputs a list of touples containing a character and number of occurences
countLetters::[Char] -> [(Char, Integer)]
countLetters [] = []

countLetters str = countLettersHelper str [] 

-- helper function for countLettersHelper that counts the occurences and sorts the list
-- takes a string and a list of touples as input
--outputs a list of touples
countLettersHelper :: [Char] -> [(Char, Integer)] -> [(Char, Integer)]
countLettersHelper [] lst = sortBy (compare `on` snd) lst

countLettersHelper (c:str) lst = 
    let fLst = addLetter c  lst
    in countLettersHelper str fLst

-- helper function fir countLettersHelper that add a letter to the list either by incrementing a existing counter
-- or adding the letter at the end of the list
addLetter :: Char ->  [(Char, Integer)] -> [(Char, Integer)]
addLetter c [] = [(c, 1)]

addLetter c (x:xs) = 
    let (left, right) = break (keyEqualsCharacter c ) (x:xs)
    in case right of [] -> left ++ (c, 1):[]
                     ((a, b): xs) -> left ++ (a ,b + 1):xs
                     
-- function that compairs a character and the first element of a touple
keyEqualsCharacter :: Eq a => a -> (a, b) -> Bool
keyEqualsCharacter c p = (fst p) == c

-- function that converts a list of touples containing a character and their number of occurences into BTree leaf ndoes
convertToLeafs :: [(Char, Integer)] -> [BTree]
convertToLeafs [] = []

convertToLeafs ((a,b):xs) = BLeaf (a,b) : convertToLeafs xs

--function that builds the huffman tree
-- takes a list of BTrees as input
-- outputs a BTree that is the root of the huffmann tree
buildHuffmanTree :: [BTree] -> BTree 
buildHuffmanTree (x:[]) = x

buildHuffmanTree (x:y:xs) = buildHuffmanTree treeLst
                            where treeLst = sortTreeList lst 
                                  lst = BBranch(nodeWeight, x, y) : xs
                                  nodeWeight = (weight x) + (weight y)

-- helper function for buildHuffmanTree that sorts the BTree list in ascending order everytime a branch is created
sortTreeList :: [BTree] -> [BTree]
sortTreeList lst = sortBy (compare `on` weight) lst

-- converts a Binary tree to a list of touples containing prefix code and corresponding character by traversing the tree and returnin the route to the character and the character
-- takes a BTree as input
-- outputs a list of touples containing prefixcodes and corresponding character
convertTreeToPrefixCode :: BTree -> [([Integer], Char)]
convertTreeToPrefixCode (BBranch (_, t1, t2)) = paths1 ++ paths2
    where paths1 = case t1 of (BBranch(_,_,_)) -> addPrefixToList 0 (convertTreeToPrefixCode t1)
                              (BLeaf(c, _)) -> [([0], c)]
          paths2 = case t2 of (BBranch(_,_,_)) -> addPrefixToList 1 (convertTreeToPrefixCode t2)
                              (BLeaf(c, _)) -> [([1], c)]

-- helper function for convertTreeToPrefixCode that adds a prefix 0 or 1 to every element of a list
-- takes a prefix and a list visited paths as input
-- outpats a list of all visited paths conactenated with the prefix
addPrefixToList :: Integer -> [([Integer], a)] -> [([Integer], a)]
addPrefixToList prefix [] = []
addPrefixToList prefix ((path, c):xs) = (prefix : path, c) : addPrefixToList prefix xs

-- compresses a string
-- takes a string and a list of prefix codes with corresponding characters as input
-- outputs a compressed string
compressString :: [Char] -> [([Integer], Char)] -> [Integer]
compressString [] prefixCodes = []
compressString (c:str) prefixCodes = (getCompressedString c prefixCodes) ++ (compressString str prefixCodes)

-- Helper function for compressString that gets the prefix for one character
-- Takes a character and a list of prefix codes with corresponding characters as input
-- outputs the prefix of a character
getCompressedString :: Char -> [([Integer], Char)] -> [Integer]
getCompressedString c [] = error ("Character was not found")
getCompressedString c ((prefix, pc):xs)
    | c == pc   = prefix
    | otherwise = getCompressedString c xs

-- Function that decompresses a compressed string.
-- Takes a compressed string cstr and a Binary tree that represents some optimal prefix encoding
-- Outputs a decompressed string
decompressString :: [Integer] -> BTree -> [Char]
decompressString [] tree = []
decompressString cstr tree = decodeCharacter cstr tree tree

-- Helper function for decompressString that traverses the tree to find the next character
-- takes a compressed string, a BTree and a Btree roo as input
-- outputs the found character concatenated  with decompress string called recursively
decodeCharacter :: [Integer] -> BTree -> BTree -> [Char]
decodeCharacter (x:xs) (BBranch(_, t0, t1)) root
    | x == 0 = decodeCharacter xs t0 root
    | x == 1 = decodeCharacter xs t1 root
    | otherwise = error "Invalid prefix code"

decodeCharacter lst (BLeaf(c, _)) root = c : decompressString lst root
