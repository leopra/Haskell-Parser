module BigParsers where

import Expr
import Parser
import SingleParsers

type ScDefn a = (Name, [a], Expr a)  -- Super combinator definition
type CoreScDefn = ScDefn Name

type Program a = [ScDefn a]
type CoreProgram = Program Name

parseProg :: Parser (Program Name)
parseProg = do p <- parseScDef
               do character ";"
                  ps <- parseProg
                  return (p:ps)
                  <|> return [p]


parseScDef :: Parser (ScDefn Name)
parseScDef = do v <- identifier
                pf <- many identifier
                character "="
                body <- parseExpr -- call to parseExpr
                return (v, pf, body)

parseExpr :: Parser (Expr Name)
parseExpr = parseLocalDef
            -- <|> parseRecursiveDef
            <|> parseCase
            -- <|> parseLambda
            <|> parseAExpr

parseAExpr :: Parser (Expr Name)
parseAExpr = parseVar
             <|> parseNum
             <|> parseConst
             <|> parseParExpr

-- | Parses a single alternative
parseAlt :: Parser (Alter Name)
parseAlt = do character "<"
              index <- natural
              character ">"
              vars <- many identifier
              character "->"
              exp <- parseExpr
              return (index, vars, exp)

parseCase :: Parser (Expr Name)
parseCase = do character "case"
               c <- parseExpr
               character "of"
               alts <- parseAlts
               return (ECase c alts) 

-- | Parsers one or more alternatives
parseAlts :: Parser ([Alter Name])
parseAlts = do a <- parseAlt
               ass <- many (do parseAlt)
               return (a:ass)

--parseDef :: Parser (Def Name)
-- | Parses a definition
parseDef :: Parser (Def Name)
parseDef = do i <- identifier   -- Get the identifier
              character "="        -- Read the "="
              v <- parseExpr    -- Get the value
              return (i, v)     -- Return (identifier, value)

parseLocalDef :: Parser (Expr Name)
parseLocalDef = do character "let"
                   defs <- parseMultipleDefs
                   character "in"
                   expr <- parseExpr
                   return (ELet NonRecursive defs expr)

-- | Parses multiple definitions
parseMultipleDefs :: Parser ([Def Name])
parseMultipleDefs = do d <- parseDef
                       df <- many (do character ";"
                                      parseDef)
                       return (d:df)
                       
-- | Parsers a number
parseNum :: Parser (Expr Name)
parseNum = do n <- integer
              return (ENum n)

-- | Parsers a constructor
parseConst :: Parser (Expr Name)
parseConst = do character "Pack"
                character "{"
                tag <- natural
                character ","
                arity <- natural
                character "}"
                return (EConstr tag arity)


-- | Parses an parenthesized expression
parseParExpr :: Parser (Expr Name)
parseParExpr = do character "("
                  e <- parseExpr
                  character ")"
                  return e

-- | Parsers a variable
parseVar :: Parser (Expr Name)
parseVar = do i <- identifier
              return (EVar i)
