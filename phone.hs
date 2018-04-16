type Digit = Char

type Sequence = String

type Presses = Int

data Button
  = Button Digit
           Sequence
  | SpecialButton Digit
  deriving (Eq, Show)

data DaPhone =
  DaPhone [Button]
  deriving (Eq, Show)

phoneMap :: DaPhone
phoneMap =
  DaPhone
    [ Button '1' "1"
    , Button '2' "abc1"
    , Button '3' "def2"
    , Button '4' "ghi3"
    , Button '5' "jkl4"
    , Button '6' "mno5"
    , Button '7' "pqrs6"
    , Button '8' "tuv8"
    , Button '9' "wxyz9"
    , SpecialButton '*'
    , Button '0' " 0"
    , Button '#' ".,#"
    ]

findButton :: DaPhone -> Char -> Button
findButton (DaPhone phones) char =
  (head . filter (\(Button x y) -> char `elem` y)) phones

baseCount :: [(Digit, Presses)]
baseCount = [(x, 0) | x <- ['0' .. '9'] ++ ['a' .. 'z'] ++ ".,"]
