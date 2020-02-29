module BigParsers where

import Expr
import Parser

type ScDefn a = (Name, [a], Expr a)
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
                body <- parseExpr
                return (v, pf, body)

parseExpr :: Parser (Expr Name)
parseExpr = parseLocalDef
            <|> parseLetRec
            <|> parseCase
            <|> parseLambda
            <|> parseOp1


------------------------------------------------------- 
-- operations

parseOp1 :: Parser (Expr Name)
parseOp1 = do a <- parseOp2
              do character "|"
                 b <- parseOp1
                 return (EAp (EAp (EVar "|") a) b) 
               <|> return a

parseOp2 :: Parser (Expr Name)
parseOp2 = do a <- parseOp3
              do character "&"
                 b <- parseOp2
                 return (EAp (EAp (EVar "&") a) b) 
               <|> return a

parseOp3 :: Parser (Expr Name)
parseOp3 = do a <- parseOp4
              do k <- relop
                 b <- parseOp4
                 return (EAp (EAp (EVar k) a) b) 
               <|> return a

parseOp4 :: Parser (Expr Name)
parseOp4 = do a <- parseOp5
              do character "+"
                 b <- parseOp4
                 return (EAp (EAp (EVar "+") a) b)
                <|> do character "-"
                       b <- parseOp5
                       return (EAp (EAp (EVar "-") a) b)
                <|> return a

parseOp5 :: Parser (Expr Name)
parseOp5 = do a <- parseOp6
              do character "*"
                 b <- parseOp5
                 return (EAp (EAp (EVar "*") a) b)
                <|> do character "/"
                       b <- parseOp6
                       return (EAp (EAp (EVar "*") a) b)
                <|> return a

parseOp6:: Parser (Expr Name)
parseOp6 = do x <- parseAExpr
              xs <- many parseAExpr
              return (foldl(\a b -> (EAp a b)) x xs)

relop :: Parser String
relop = character "=="
         <|> character"<="
         <|> character">="
         <|> character ">"
         <|> character "<"
         <|> character"~="



parseLetRec :: Parser (Expr Name)
parseLetRec = do character "letrec"
                 def <- parseDef
                 dd <- many parseDef
                 character "in"
                 expr <- parseExpr
                 return (ELet Recursive (def:dd) expr)

parseLambda :: Parser (Expr Name)
parseLambda = do character "\\"
                 a <- some identifier
                 character "."
                 expr <- parseExpr 
                 return (ELam a expr)

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
              v <- parseAExpr    -- Get the value
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
                       
parseAExpr :: Parser (Expr Name)
parseAExpr = parseVar
             <|> parseNum
             <|> parseConst
             <|> parseParExpr


-- | Parsers a variable
parseVar :: Parser (Expr Name)
parseVar = do i <- identifier
              return (EVar i)

-- | Parses a number
parseNum :: Parser (Expr Name)
parseNum = do n <- integer
              return (ENum n)

-- | Parses a constructor
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

