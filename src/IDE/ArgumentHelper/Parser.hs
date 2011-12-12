module IDE.ArgumentHelper.Parser (
    parseArgumentsFromMethodDeclaration
) where

import Text.Parsec hiding (try)
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Control.Monad (liftM)


-- | Return all arguments of given method declaration.
parseArgumentsFromMethodDeclaration :: String -> [String]
parseArgumentsFromMethodDeclaration str = case (parse argumentsFromMethodDeclaration "" str) of
    Right arguments -> arguments
    Left _ -> []


argumentsFromMethodDeclaration :: Parser [String]
argumentsFromMethodDeclaration = do
    functionName
    whiteSpace
    string "::"
    whiteSpace
    arguments

functionName = do
    c <- lower
    x <- many letter
    return $ c:x

arguments = many $ try argType

typeName = manyTill (letter <|> char '(' <|> char ')' <|> char '_') whiteSpace

argType = do
    liftM unwords $ manyTill typeName $ try nextArg

nextArg = string "->"

lexer = P.makeTokenParser haskellDef
--whiteSpace = P.whiteSpace lexer
whiteSpace = many1 space



















