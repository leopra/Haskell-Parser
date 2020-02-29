module Parser(module Control.Applicative, module Parser) where

import Control.Applicative
import Data.Char

newtype Parser a = P(String -> [(a, String)])

    -- Apply a generic Parser to an input string (app)
parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

    --  instance of Functor
instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f parser = P(\input -> case parse parser input of
                            []                   -> []
                            [(value, remaining)] -> [(f value, remaining)])

    -- instance of Applicative
instance Applicative Parser where
    -- transforms value in parser without consuming the input string
    -- pure :: a -> Parser a
  pure value = P (\input -> [(value, input)])
    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P(\input -> case parse pg input of
                                []        -> []
                                [(g, out)] -> parse (fmap g px) out)


    -- instance of Monad
instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P(\inp -> case parse p inp of
                    [] -> []
                    [(v, remaining)] -> parse (f v) remaining)


    -- instance of Alternative
instance Alternative Parser where
    -- empty :: Parser a
  empty = P (\inp -> [])
    -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P(\inp -> case parse p inp of
                        []         -> parse q inp
                        [(v, out)] -> [(v, out)])


------------------------------------------------------------------

item :: Parser Char
item = P (\inp -> case inp of
                    [] ->     []
                    (x:xs) -> [(x, xs)])

-- Parses the first character if it satisfies the conditions, and returns empty otherwise
sat :: (Char -> Bool) -> Parser Char
sat predicate = do x <- item
                   if predicate x then return x else empty

-- |  Parses a single digit
digit :: Parser Char
digit = sat isDigit

-- | Parses a lowercase letter
lower :: Parser Char
lower = sat isLower

-- | Parses an uppercase letter
upper :: Parser Char
upper = sat isUpper

-- | Parses a letter
letter :: Parser Char
letter = sat isAlpha

-- | Parses a letter or a digit
alphanum :: Parser Char
alphanum = sat isAlphaNum

-- | Parses a specific character
char :: Char -> Parser Char
char x = sat (== x)

-- | Parses a string
string :: String -> Parser String
string []     = return []
string (x:xs) = do char x       -- first char
                   string xs     -- recursion
                   return (x:xs) -- return f string

ident :: Parser String
ident = do v <- lower
           vs <- many alphanum
           return (v:vs)

-- | Parses a natural number
nat :: Parser Int
nat = do ns <- some digit
         return (read ns) -- Converts a string to an integer

-- | Parsers zero or more spaces
space :: Parser ()
space = do many (sat isSpace) -- Takes all the spaces
           return ()          -- Returns the remaining string

-- | Parses an integer number
int :: Parser Int
int = do char '-'    -- If it reads the -
         n <- nat        -- Keep reading the number
         return (-n)     -- Return the negative number
      <|> nat -- Otherwise return the simple number

-- function that ignores spaces between and after applying a parser
token :: Parser a -> Parser a
token parser = do space
                  value <- parser
                  space
                  return value

--  parsers that ignore spaces around identifiers, natural numbers, integers and special symbols

identifier:: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

character :: String -> Parser String
character xs = token (string xs)

