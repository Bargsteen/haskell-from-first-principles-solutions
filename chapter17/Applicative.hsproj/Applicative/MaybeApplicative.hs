module MaybeApplicative where
  
import Control.Applicative(liftA2, liftA3)
  
validateLength :: Int -> String -> Maybe String
validateLength maxLen s = 
  if (length s) > maxLen
  then Nothing
  else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = 
  fmap Name $ validateLength 25 s 
  
mkAddress :: String -> Maybe Address
mkAddress a = 
  fmap Address $ validateLength 100 a
  
data Person =
  Person Name Address
  deriving (Eq, Show)
  
mkPersonVerbose :: String -> String -> Maybe Person
mkPersonVerbose n a = 
  case mkName n of 
    Nothing -> Nothing 
    Just n' -> 
      case mkAddress a of
        Nothing -> Nothing
        Just a' -> 
          Just $ Person n' a'
          
-- We can do better than that ^ !

mkPerson :: String -> String -> Maybe Person
mkPerson n a = Person <$> mkName n <*> mkAddress a
-- OR:         liftA2 Person (mkName n) (mkAddress a)


-- Before we moooove on...
data Cow = Cow {
    name   :: String
  , age    :: Int
  , weight :: Int
  } deriving (Eq, Show)
  
noEmpty :: String -> Maybe String 
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int 
noNegative n | n >= 0 = Just n
             | otherwise = Nothing
             

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just name -> 
      case noNegative age' of
        Nothing -> Nothing
        Just age ->
          case noNegative weight' of 
            Nothing -> Nothing
            Just weight ->
              Just (Cow name age weight)
              
cowFromString' :: String -> Int -> Int -> Maybe Cow
cowFromString' name age weight = 
  liftA3 Cow (noEmpty name) (noNegative age) (noNegative weight)
--OR: Cow <$> noEmpty name <*> noNegative age <*> noNegative weight





