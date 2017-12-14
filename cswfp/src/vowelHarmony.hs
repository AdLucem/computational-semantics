data Feature = F Attr Value deriving (Eq,Show)

data Attr    = Back | High | Round | Cons deriving (Eq,Show)

data Value   = Plus | Minus               deriving (Eq,Show)

type Phoneme = [Feature]

yawelmaniVowels = [i,a,o,u,e]

i = [F Cons Minus,  F High Plus,
  F Round Minus, F Back Minus]
a = [F Cons Minus,  F High Minus,
  F Round Minus, F Back Plus ]
o = [F Cons Minus,  F High Minus,
  F Round Plus,  F Back Plus ]
u = [F Cons Minus,  F High Plus ,
  F Round Plus,  F Back Plus ]
e = [F Cons Minus,  F High Minus,
  oF Round Minus, F Back Minus]

c = [F Cons Plus]

realize :: Phoneme -> Char
realize x | x == i = 'i'
  | x == a = 'a'
  | x == o = 'o'
  | x == u = 'u'
  | x == e = 'e'
  | x == c = 'c'

fValue :: Attr -> Phoneme -> Value
fValue attr [] = error "feature not found"
fValue attr ((F a v):fs) | attr == a = v
  | otherwise = fValue attr fs

fMatch :: Attr -> Value -> Phoneme -> Phoneme
fMatch attr value fs = map (match attr value) fs
  where match a v f@(F a’ v’) | a == a’   = F a’ v
 | otherwise = f

-- works on the assumption that there is only
-- one vowel in a word/suffix
extractYVowel :: [Phoneme] -> Phoneme
extractYVowel str = head $ filter (\ x -> (x `elem` yawelmaniVowels)) str


appendSuffixY :: [Phoneme] -> [Phoneme] -> [Phoneme]
appendSuffixY word [] = word
appendSuffixY [] suffix = error "No word stem attached"
appendSuffixY word suffix =
  let
    trigger = extractYVowel word
    target = extractYVowel suffix
  in
    case ((fValue High trigger) == (fValue High target)) of
      True  -> let
        updatedSuffix suffix =
          let vh x | (fValue x Cons) == Plus = x
                   | otherwise               = (fMatch Round (fValue Round trigger) (fMatch Back (fValue Back trigger) x))
          in map vh suffix
        in
        word ++ (updatedSuffix suffix)
      False -> word ++ suffix
