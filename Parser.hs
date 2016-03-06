{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parser
  (
    JSON(..),
    Parser.parse
  ) where

import qualified Data.ByteString.Lazy as BS
import           Text.Parsec

data JSON = JBool Bool   |
            JNull        |
            JNum Double  |
            JStr String  |
            JArr [JSON]  |
            JObj [(String, JSON)]
            deriving Show

type Parser = Parsec BS.ByteString ()
type JSONParser = Parser JSON
type KeyValuePair = Parser (String, JSON)

parseDouble :: Parser Double
parseDouble = try $ do
  neg    <- optionMaybe $ char '-'
  prefix <- many1 digit
  radix  <- optionMaybe $ char '.'
  parseRest (maybeApp neg prefix) radix
  where
    maybeApp :: Maybe a -> [a] -> [a]
    maybeApp Nothing  xs = xs
    maybeApp (Just r) xs = r:xs

    parseRest :: String -> Maybe Char -> Parser Double
    parseRest prefix Nothing  = return $ read prefix
    parseRest prefix (Just r) = do
      suffix <- many1 digit
      return $ read $ prefix ++ r : suffix

sepBySpaces :: Parser a -> Parser a
sepBySpaces = between spaces spaces

commaBWSpaces :: Parser ()
commaBWSpaces = do
  sepBySpaces $ char ','
  return ()

insideDelims :: (Char, Char) -> Parser a -> Parser a
insideDelims (b, e) =
  between (sepBySpaces $ char b) (sepBySpaces $ char e)

notQuote :: Parser Char
notQuote = satisfy (/= '"')

quotedString :: Parser String
quotedString = between (char '"') (char '"') (many notQuote)

parseToF :: JSONParser
parseToF = try $
  parseTrue <|> parseFalse
  where
    parseTrue  = do
      string "true"
      return $ JBool True
    parseFalse = do
      string "false"
      return $ JBool False

parseNul :: JSONParser
parseNul = try $ do
  string "null"
  return JNull

parseNum :: JSONParser
parseNum = try $ do
  num <- sepBySpaces parseDouble
  return $ JNum num

parseStr :: JSONParser
parseStr = try $ do
  str <- sepBySpaces quotedString
  return $ JStr str

parseArr :: JSONParser
parseArr = try $ do
  arr <- insideDelims ('[', ']') (sepBy parseJSON commaBWSpaces)
  return $ JArr arr

parseKeyValuePair :: KeyValuePair
parseKeyValuePair = try $ do
  (JStr key) <- parseStr
  sepBySpaces $ char ':'
  val        <-  parseJSON
  return (key, val)

parseObj :: JSONParser
parseObj = try $ do
  obj <- insideDelims ('{', '}') (sepBy parseKeyValuePair commaBWSpaces)
  return $ JObj obj

parseJSON :: JSONParser
parseJSON =
  parseToF <|>
  parseNul <|>
  parseNum <|>
  parseStr <|>
  parseArr <|>
  parseObj

parse :: String -> BS.ByteString -> Either ParseError JSON
parse = runParser parseJSON ()
