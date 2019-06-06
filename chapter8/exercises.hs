module Exercises where

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "Woops"

frappe :: String -> String
frappe = flippy "haha"

-- frappe (appedCatty "2") = Woops mrow 2 mrow haha
-- appedCatty (frappe "blue") = Woops mrow blue mrow haha
-- cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue")) = pink mrow haha mrow green mrow Woops mrow blue
-- cattyConny (flippy "Pugs" "are") "awesome" = are mrow Pugs mrow awesome
