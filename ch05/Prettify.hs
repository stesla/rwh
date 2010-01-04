module Prettify where
       
import Numeric (showHex)
import Data.Char (ord)
import Data.Bits (shiftR, (.&.))

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         deriving (Show, Eq)

empty :: Doc
empty = Empty

line :: Doc
line = Line

char :: Char -> Doc
char c = Char c

double :: Double -> Doc
double d = text (show d)

text :: String -> Doc
text "" = Empty
text s = Text s

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

(<>) :: Doc -> Doc -> Doc
x <> Empty = x
Empty <> y = y
x <> y = x `Concat` y

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line = char ' '
flatten (x `Union` _) = flatten x
flatten other = other

fsep :: [Doc] -> Doc
fsep = fold (</>)

hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds                  

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close
                         . fsep . punctuate (char ',') . map item

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
  Just r -> text r
  Nothing | mustEscape c -> hexEscape c          
          | otherwise -> char c
  where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
  where ch a b = (a, ['\\',b])
        
smallHex :: Int -> Doc
smallHex x = text "\\u"
             <> text (replicate (4 - length h) '0')
             <> text h
             where h = showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
  where a = (n `shiftR` 10) .&. 0x3ff
        b = n .&. 0x3ff

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise = astral (d - 0x10000)
              where d = ord c

fill :: Int -> Doc -> Doc
fill w = hcat . map ((<> line) . fillLine (w-1)) . lines'

lines' :: Doc -> [Doc]
lines' (a `Concat` b) | isLine b = lines' a
lines' (a `Concat` b) = lines' a ++ lines' b
lines' other = [other]

isLine :: Doc -> Bool
isLine x | x == line || x == softline = True
         | otherwise = False

fillLine :: Int -> Doc -> Doc
fillLine w doc = doc <> filler
  where filler | space > 0 = text (replicate space ' ')
               | otherwise = Empty
        space = w - lineWidth doc

lineWidth :: Doc -> Int
lineWidth Empty = 0
lineWidth Line = error "multi-line doc"
lineWidth (Char _) = 1
lineWidth (Text str) = length str
lineWidth (a `Concat` b) = lineWidth a + lineWidth b
lineWidth (x `Union` _) = lineWidth x

compact :: Doc -> String
compact x = transform [x]
  where transform [] = ""
        transform (d:ds) =
          case d of
            Empty -> transform ds
            Char c -> c : transform ds
            Text s -> s ++ transform ds
            Line -> '\n' : transform ds
            a `Concat` b -> transform (a:b:ds)
            _ `Union` b -> transform (b:ds)

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
  where best col (d:ds) =
          case d of
            Empty -> best col ds
            Char c -> c : best (col + 1) ds
            Text s -> s ++ best (col + length s) ds
            Line -> '\n' : best 0 ds
            a `Concat` b -> best col (a:b:ds)
            a `Union` b -> nicest col (best col (a:ds)) (best col (b:ds))
        best _ _ = ""
        
        nicest col a b | (width - least) `fits` a = a
                       | otherwise = b
                         where least = min width col

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` "" = True
w `fits` ('\n':_) = True
w `fits` (c:cs) = (w - 1) `fits` cs