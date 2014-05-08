{-# LANGUAGE TemplateHaskell #-}

-- This code is partially from http://www.haskell.org/haskellwiki/Template_Haskell

module PrintF where

-- NB: printf needs to be in a separate module to the one where
-- you intend to use it.

-- Import Template Haskell syntax
import Language.Haskell.TH

-- Possible string tokens: %d %s and literal strings
data Format = D | S | L String
    deriving Show

-- The printf meta-programming function
printf :: String -> ExpQ
printf fmt = gen2 (parse fmt) [| "" |]

-- Generating Haskell code for a particular format
gen :: [Format] -> ExpQ
gen [D]   = [| \n -> show n |]
gen [S]   = [| \s -> s |]
gen [L s] = [| s |]


gen2 :: [Format] -> ExpQ -> ExpQ
gen2 [] acc    = acc
gen2 (x:xs) acc = case x of 
    D -> [| \n -> $(gen2 xs [| $acc ++ (show n) |]) |]
    S -> [| \s -> $(gen2 xs [| $acc ++ s |]) |]
    L s -> [| $(gen2 xs [| $acc + s |]) |]

-- A poor man's tokenizer
parse :: String -> [Format]
parse [] = []
parse ('%':c:rest) | c == 'd' = D : parse rest
                   | c == 's' = S : parse rest
parse (s:str) = L (s:p) : parse rest -- so we don't get stuck on weird '%'
    where (p, rest) = span (/= '%') str


