module Person where

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
  NameEmpty
    | AgeTooLow
    | PersonInvalidUnknown String
    deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
    Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
    Left $ PersonInvalidUnknown $
      "Name was: " ++ show name ++
      " Age was: " ++ show age


gimmePerson :: IO ()
gimmePerson = do
  putStrLn "We are making a person. Help me."
  putStr "Name: "
  name <- getLine
  putStr "Age: "
  ageStr <- getLine
  let age = read ageStr :: Integer
  let person = mkPerson name age
  putStrLn $ personFeedback person


personFeedback :: Either PersonInvalid Person -> String
personFeedback epp =
  case epp of
    (Right (Person name age)) -> "Yay! Succesfully got a person: " ++ name ++ " " ++ show age
    (Left NameEmpty) -> "Error: Name was empty."
    (Left AgeTooLow) -> "Error: Age below 0 is a no go."
    (Left (PersonInvalidUnknown s)) -> "Error: Unknown - " ++ s
