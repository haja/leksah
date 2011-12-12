module IDE.ArgumentHelper.Parser (
    parseArgumentsFromMethodDeclaration
) where

import Text.Parsec hiding (try)
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)


-- | Return all arguments of given method declaration.
parseArgumentsFromMethodDeclaration :: String -> [String]
parseArgumentsFromMethodDeclaration str = case (parse argumentsFromMethodDeclaration "" str) of
    Right arguments -> arguments
    Left _ -> []


argumentsFromMethodDeclaration :: Parser [String]
argumentsFromMethodDeclaration = do
    functionName
    --many whiteSpace -- TODO this doesn't work. but lookup how ghc is implemented to parse method declarations
    many space
    string "::"
    many space
    args <- argumentsAndReturnType
    return $ init args

functionName = do
    c <- lower
    x <- many letter
    return $ c:x

argumentsAndReturnType = sepBy typeName nextArg

typeName = do -- TODO underscore _ valid?
    c <- upper
    x <- many (letter <|> (char '_'))
    return $ c:x

nextArg = do
    many space
    string "->"
    many space


lexer = P.makeTokenParser haskellDef
whiteSpace = P.whiteSpace lexer
