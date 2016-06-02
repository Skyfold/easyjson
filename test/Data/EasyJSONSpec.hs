{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE DeriveAnyClass #-}

module Data.EasyJSONSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck hiding (Success, Failure, Result)
import Data.Aeson hiding (Success, Result)
import Data.HashMap.Strict as HM
import Text.Trifecta
import Data.Scientific 
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Vector as V
import Data.Text (Text)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), bool)

import Data.EasyJSON

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "nullValue" $ do
    it "parses 'null' as Null" $ do
      (runParser nullValue "null") 
        `shouldBe` 
          maybe (error "fail") Success (decode "null")
  describe "bool" $ do
    it "parses 'true' as (Bool True)" $ do
      (runParser bool "true") 
        `shouldBe` 
          maybe (error "fail") Success (decode "true")
    it "parses 'false' as (Bool False)" $ do
      (runParser bool "false") 
        `shouldBe` 
           maybe (error "fail") Success (decode "false")
  describe "number" $ do
    it "parses Scientific numbers" $ do
        runParser number "56.57" 
          `shouldBe` 
             maybe (error "fail") Success (decode "56.57")
  describe "textValue" $ do
    it "handles double quotes" $ do
      (runParser (String <$> textValue) "\"Haskell is the best\"") 
        `shouldBe` 
            maybe (error "fail") Success (decode "\"Haskell is the best\"")
    it "escaped \\\" " $ do
      (runParser (String <$> textValue) "\"escaped \\\" char\"") 
        `shouldBe` 
            maybe (error "fail") Success (decode "\"escaped \\\" char\"")
    it "escaped \\\\ " $ do
      (runParser (String <$> textValue) "\"escaped \\\\ \"") 
        `shouldBe` 
            maybe (error "fail") Success (decode "\"escaped \\\\ \"")
    it "escaped \\/ " $ do
      (runParser (String <$> textValue) "\"escaped \\/ \"") 
        `shouldBe` 
            maybe (error "fail") Success (decode "\"escaped \\/ \"")
    it "escaped \\b " $ do
      (runParser (String <$> textValue) "\"escaped \b \"") 
        `shouldBe` 
            maybe (error "fail") Success (decode "\"escaped \b \"")
    it "escaped \\f " $ do
      (runParser (String <$> textValue) "\"escaped \f \"") 
        `shouldBe` 
            maybe (error "fail") Success (decode "\"escaped \f \"")
    it "escaped \\n " $ do
      (runParser (String <$> textValue) "\"escaped \n \"") 
        `shouldBe` 
            maybe (error "fail") Success (decode "\"escaped \n \"")
    it "escaped \\r " $ do
      (runParser (String <$> textValue) "\"escaped \r \"") 
        `shouldBe` 
            maybe (error "fail") Success (decode "\"escaped \r \"")
    it "escaped \\t " $ do
      (runParser (String <$> textValue) "\"escaped \t \"") 
        `shouldBe` 
            maybe (error "fail") Success (decode "\"escaped \t \"")
    -- it "escaped \\ followed by valid char" $ do
    --   (runParser textValue "\"escaped \\n \"") 
    --     `shouldBe` 
    --         maybe (error "fail") Success (decode "\"escaped \\n \"")
  describe "parse object" $ do
    it "round trip a singleton vector" $ do
        runParser value (encode (V.singleton (Bool True)))
        `shouldBe` 
            maybe (error "fail") Success (decode (encode (V.singleton (Bool True))))
  describe "parse array" $ do
    it "round trip a singleton HashMap" $ do
        runParser value (encode (HM.singleton ("test" :: Text) (Bool True)))
        `shouldBe` 
            maybe (error "fail") Success 
                (decode (encode (HM.singleton ("test" :: Text) (Bool True))))
  -- describe "checkJSON" $ do
  --   it "is identical to decode from Aeson" $ property $
  --     \(val :: Value) -> decode (encode val) 
  --       == runParser checkJSON (encode val)

runParser :: Parser Value -> Data.ByteString.Lazy.ByteString -> Result Value
runParser p str = parseByteString p mempty (toStrict str)

-- deriving instance Arbitrary Value
instance Eq a => Eq (Result a) where
    (==) l r = case (l, r) of
        (Success a, Success b) -> a == b
        (Failure a, Failure b) -> 
            error $ 
                displayS (renderPretty 0.4 80 a) 
                    ((displayS (renderPretty 0.4 80 b)) "")
        (Failure a, _) -> 
            error $ displayS (renderPretty 0.4 80 a) ""
        (_, Failure a) -> 
            error $ displayS (renderPretty 0.4 80 a) ""
