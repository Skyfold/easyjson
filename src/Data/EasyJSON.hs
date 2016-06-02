module Data.EasyJSON where

import Text.Trifecta
import qualified Data.Aeson as A
import Data.HashMap.Strict as HM
import Text.Trifecta
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import Data.Vector as V
import Data.Text (Text, pack, unpack)
import Control.Applicative
import Text.Parser.Token.Highlight
import Data.String
import Control.Monad.IO.Class

readFileToJSON :: MonadIO m => String -> m (Maybe A.Value)
readFileToJSON = parseFromFile value

stringToJSON :: String -> Result A.Value
stringToJSON = parseString value mempty

strictByteStringToJSON :: Strict.ByteString -> Result A.Value
strictByteStringToJSON = parseByteString value mempty

lazyByteStringToJSON :: Lazy.ByteString -> Result A.Value
lazyByteStringToJSON = parseByteString value mempty . Lazy.toStrict

textToJSON :: Text -> Result A.Value
textToJSON = parseString value mempty . unpack

maybeValue :: Result A.Value -> Maybe A.Value
maybeValue (Success a) = Just a
maybeValue _ = Nothing

value :: (Monad m, TokenParsing m) => m A.Value
value = object
        <|> array
        <|> A.String <$> textValue
        <|> bool
        <|> nullValue
        <|> number

object :: (Monad m, TokenParsing m) => m A.Value
object = A.Object . HM.unions <$> braces (sepBy obj (symbolic ',')) 
      <?> "object"
  where 
    obj = do
        key <- textValue <* symbolic ':'
        value <- value
        return $ HM.singleton key value
        
array :: (Monad m, TokenParsing m) => m A.Value
array = A.Array . V.fromList <$> brackets (sepBy value (symbolic ','))

bool :: (Monad m, TokenParsing m) => m A.Value
bool = choice 
    [ symbol "true" >> return (A.Bool True)
    , symbol "false" >> return (A.Bool False)
    ]

nullValue :: (Monad m, TokenParsing m) => m A.Value
nullValue = symbol "null" >> return A.Null

number :: (Monad m, TokenParsing m) => m A.Value
number = do 
    sci <- scientific
    return $ A.Number sci

textValue :: TokenParsing m => m Text
textValue = fromString <$> token (highlight StringLiteral lit) where
  lit = between (char '"') (char '"' <?> "end of string") (many stringChar)
    <?> "\"string\""
  stringChar = stringLetter
           <|> stringEscape
           <|> escapedChar
       <?> "string character"
  stringLetter  = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

  stringEscape = highlight EscapeCode $ char '\\' *> esc where
    esc = char '\\'
      <|> char '/'
      <|> char '"'
  escapedChar = highlight EscapeCode esc where
    esc = char '\b'
      <|> char '\f'
      <|> char '\n'
      <|> char '\r'
      <|> char '\t'
      -- <|> Just <$> escapeCode
  -- escapeEmpty = char '&'
  -- escapeGap = skipSome space *> (char '\\' <?> "end of string gap")

