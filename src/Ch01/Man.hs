module Ch01.Man where

data Man = Man { name :: String }

hello :: Man -> String
hello m = "Hello " ++ name m ++ "!"

goodbye :: Man -> String
goodbye m = "Good-bye " ++ name m ++ "!"

-- λ> let m = Man "David"
-- λ> hello m
-- "Hello David!"
-- λ> goodbye m
-- "Good-bye David!"
