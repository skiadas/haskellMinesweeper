module Types where
   data Ground = Mine | Safe deriving (Eq, Show)
   data State = Blank | Flagged | Questioned | Revealed Int deriving (Eq, Show)