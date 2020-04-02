module Types where
   data Ground = Mine | Safe deriving (Eq, Show)
   data State = Blank | Flagged | Questioned | Revealed Int deriving (Eq, Show)
   data Outcome = Win | Loss | Continue
   data Act = Reveal | Flag | Question deriving (Eq, Show)
   type Action = (Int, Int, Act)

