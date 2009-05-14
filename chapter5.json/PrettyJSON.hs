-- PrettyJSON
module PrettyJSON 
   (
     renderJValue -- This is the ONLY thing we export from this module!!
   ) where

import Numeric (showHex)
import Data.Char (ord)
import Data.Bits (shiftR, (.&.))

import Prettify (Doc, (<>), char, double, fsep, hcat, punctuate, text, compact, pretty)
import SimpleJSON (JValue(..))

renderJValue :: JValue -> Doc
renderJValue (JBool True) = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull = text "null"
renderJValue (JNumber n) = double n
renderJValue (JString str) = string str
renderJValue (JArray ary) = series '[' ']' renderJValue ary
renderJValue (JObject obj) = series '{' '}' field obj
   where field (name,val) = string name
                         <> text ": "
                         <> renderJValue val

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

-- enclose modifes a Doc tree such that it has a quote mark (or whatever u pass) at the front and at the end.
enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

-- oneChar renders a single character. Some single chars have to be escaped, so that's the meat of what oneChar does.
--  REMEMBER: "render" means "turn a raw value into a Doc!!!!"
oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
              Just r  -> text r
              Nothing | mustEscape c -> hexEscape c
                      | otherwise    -> char c -- normal old char
   where
      mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

-- This is a variable that contains a list of associations between Char and String. the Prelude lookup function can hit that like 
--  a hashtable (tho the impl is not that of a hashtable)
-- In Haskell jargon, this is called an "association list", aka "alist"
simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/" -- I bet zipWith is used a lot to make alists.
   where
      ch a b = (a, ['\\', b])

smallHex :: Int -> Doc
smallHex x = text "\\u"
          <> text (replicate (4 - length h) '0')
          <> text h
   where
      h = showHex x ""

-- Where the name astral comes from I have no idea. This takes an Int and turns it into hexadecimal, and then
--  renders it to a Doc.
astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
   where a = (n `shiftR` 10) .&. 0x3ff
         b = n .&. 0x3ff

-- This turns a Char into a Doc that contains the string representation of the hex for that character.
-- ord is in Data.Char, and it returns an Int with basically the ascii (if you will) for the Char (we're unicode of course)
hexEscape :: Char -> Doc
hexEscape c   | d < 0x10000 = smallHex d
              | otherwise   = astral (d - 0x10000)
   where d = ord c  -- Seeing a lot of cases where variable and where are on same line now.

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close
                       . fsep . punctuate (char ',') . map item












