module WarmUp where

stops = "pbtdkg"
vowels = "aeiou"

svs = [(x, y, z) | x <- stops, y <- vowels, z <- vowels]

svsP = filter (\(x,y,z) -> x == 'p') svs

nouns = ["hammer", "ice", "queen", "torch", "milk", "mouse", "skating", "moose", "man", "woman"]
verbs = ["hammers", "locks", "skates", "sings", "dances", "drinks", "eats", "smells", "sells", "buys"]

nvn = [(n,v,n') | n <- nouns, v <- verbs, n' <- nouns]


