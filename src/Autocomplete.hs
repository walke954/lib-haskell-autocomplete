{-# LANGUAGE OverloadedStrings #-}

module Autocomplete where

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map

-- simple aliases
type Result = Text
type ExcludeChar = Char
type Count = Int
type DropIndex = Int

-- complex aliases
type TrieMap = Map Char Trie
type SearchItem = (Trie, Text)

createSearchItem :: Trie -> Text -> SearchItem
createSearchItem t s = (t, s)

-- data types
data Trie = Trie
    { char :: Char
    , key :: Bool
    , children :: TrieMap
    } deriving (Show, Read, Eq)

empty :: Trie
empty = Trie
    { char = '_'
    , key = False
    , children = Map.empty :: TrieMap
    }

new :: Char -> Bool -> Trie
new c k = Trie
    { char = c
    , key = k
    , children = Map.empty :: TrieMap
    }

-- updates a trie with given children
replaceChildren :: Trie -> TrieMap -> Trie
replaceChildren t m = Trie
    { char = (char t)
    , key = (key t)
    , children = m
    }

-- creates a trie from a list
fromList :: [Text] -> Trie
fromList ls = foldl (\t v -> update t v) empty ls

-- updates a trie with a new string
update :: Trie -> Text -> Trie
update t s
    | s == "" = t
    | otherwise = replaceChildren t nm
    where
        c = Text.head s
        cm = children t
        d = new c $ Text.length s == 1
        nt = maybe d id $ Map.lookup c cm
        nm = Map.insert c (update nt (Text.tail s)) cm

data SearchOptions = SearchOptions
    { count :: Int
    , ratio :: Double
    } deriving (Show, Read, Eq)

defaultSearchOptions :: SearchOptions
defaultSearchOptions = SearchOptions
    { count = 10
    , ratio = 0.7
    }

createSearchOptions :: Int -> Double -> SearchOptions
createSearchOptions c r = SearchOptions
    { count = c
    , ratio = if (r <= 0) then 0.01 else r
    }

-- checks if string is contained in trie
contains :: Trie -> Text -> Bool
contains t s
    | s == "" = key t
    | otherwise = maybe False (\v -> contains v (Text.tail s)) nt
    where
        c = Text.head s
        nt = Map.lookup c $ children t

-- search trie
search :: Trie -> SearchOptions -> Text -> [Result]
search t opts s
    | length ts < 1 = []
    | otherwise = searchThroughPath [] n ts
    where
        n = count opts
        d = floor $ ratio opts * fromIntegral (Text.length s)
        ts = findSearchPath 0 d s t

-- helper function for search, searches stack until results found or stack exausted
searchThroughPath :: [ExcludeChar] -> Count -> [SearchItem] -> [Result]
searchThroughPath ec n qs
    | n <= 0 = []
    | length qs == 0 = []
    | otherwise = rs ++ searchThroughPath [char t] (n - length rs) (tail qs)
    where
        (t, _) = head qs
        rs = searchTrieStack ec n [] [head qs]

-- helper function for search, breadth first search of search item queue for all keyed descendants
searchTrieStack :: [ExcludeChar] -> Count -> [Result] -> [SearchItem] -> [Result]
searchTrieStack ec n rs qs
    | n <= 0 = rs
    | length qs == 0 = rs
    | otherwise = kv ++ searchTrieStack [] (n - length kv) rs (tail qs ++ cs)
    where
        (t, ps) = head qs
        c = char t
        kv = if (key t) then [Text.snoc ps c] else []
        cs = map mfn $ filter ffn $ Map.elems $ children t
        mfn v = createSearchItem v $ Text.snoc ps c
        ffn v = notElem (char v) ec

-- helper function for search, adds all trie nodes to a stack along given string path
findSearchPath :: Count -> DropIndex -> Text -> Trie -> [SearchItem]
findSearchPath n d s t
    | n >= Text.length s = []
    | otherwise = maybe [] fn $ Map.lookup c $ children t
    where
        c = Text.index s n
        fn v = findSearchPath (n + 1) d s v ++ si v
        si v = if (n >= d) then [createSearchItem v (Text.take n s)] else []
