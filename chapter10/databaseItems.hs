module DatabaseItems where

import Data.Time
import Data.Maybe (catMaybes)
import Data.List

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
    (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
              (fromGregorian 1921 5 1)
              (secondsToDiffTime 34123))
  ]


filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate [] = []
filterDbDate (x:xs) = 
  case x of
      DbDate time -> [time] ++ filterDbDate xs
      _ -> filterDbDate xs

filterDbDate1 :: [DatabaseItem] -> [UTCTime]
filterDbDate1 = 
  foldr maybeCons []
  where maybeCons a b =
         case a of
          (DbDate date) -> date : b
          _ -> b

filterDbDate2 :: [DatabaseItem] -> [UTCTime]
filterDbDate2 = foldr (\a b -> filterHelper a ++ b) []

filterHelper :: DatabaseItem -> [UTCTime]
filterHelper (DbDate date) = [date]
filterHelper _ = []


filterDbDate3 :: [DatabaseItem] -> [UTCTime]
filterDbDate3 db = catMaybes xs
  where xs = map getDate db

getDate :: DatabaseItem -> Maybe UTCTime
getDate (DbDate t) = Just t
getDate _ = Nothing

-- Factor out the "choice" of value
-- so you can call: filterDb getDate theDatabase


filterDb :: (DatabaseItem -> Maybe a) -> [DatabaseItem] -> [a]
filterDb getter db = catMaybes xs
  where xs = fmap getter db

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr maybeCons []
  where maybeCons a b =
         case a of
           (DbNumber num) -> num : b
           _ -> b

getNumber :: DatabaseItem -> Maybe Integer
getNumber (DbNumber n) = Just n
getNumber _ = Nothing

filterDbNumber' = filterDb getNumber theDatabase

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent db = undefined -- $ filterDb getDate db

sumDb :: [DatabaseItem] -> Integer
sumDb db = foldr (+) 0 (filterDb getNumber db) 

avgDb :: [DatabaseItem] -> Double
avgDb db = sum / count
  where 
    sum = fromIntegral $ sumDb db
    count = fromIntegral $ length $ filterDbNumber db

