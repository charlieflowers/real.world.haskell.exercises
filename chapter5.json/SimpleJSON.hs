-- file: ch05/SimpleJSON.hs
module SimpleJSON
   (
     JValue (..)
   , getString
   , getInt
   , getDouble
   , getBool
   , getObject
   , getArray
   , isNull
   ) where

data JValue = JString String
              | JNumber Double
              | JBool Bool
              | JNull
              | JObject [(String, JValue)]
              | JArray [JValue]
                deriving (Eq, Ord, Show)

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _ = Nothing

-- Interesting that they chose not to provide types for the following functions. Does that imply some kind of recommended guideline?
getInt (JNumber d) = Just (truncate d)
getInt _ = Nothing

getDouble (JNumber d) = Just d
getDouble _ = Nothing

getBool (JBool b) = Just b
getBool _ = Nothing

getObject (JObject o) = Just o
getObject _ = Nothing

getArray (JArray a) = Just a
getArray _ = Nothing

isNull v = v == JNull

