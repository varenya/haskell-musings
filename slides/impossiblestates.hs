data BadPrompt = BadPrompt
  { questions :: [String]
  , answers   :: [String]
  } deriving (Eq, Show)

badPrompts = BadPrompt {questions = ["why"], answers = ["who knows","wlo"]}

data Prompt = Prompt
  { question :: String
  , answer   :: Maybe String
  }

goodPrompts =
  [ Prompt {question = "why", answer = Nothing}
  , Prompt {question = "how", answer = Just "like this"}
  ]

