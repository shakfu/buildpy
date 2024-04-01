module Utils where

import Data.Char (toLower)

lowercase :: String -> String
lowercase = map toLower

-- | replace chars in a string
--
-- >>> replace '.' '_' "hello.world"
-- "hello_world"
-- 
replace :: Eq b => b -> b -> [b] -> [b]
replace a b =
    map (\c ->
             if c == a
                 then b
                 else c)

-- | takes function an splits string
--
-- >>> wordsWhen (=='.') "3.12.2"
-- ["3","12","2"]
--
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =
    case dropWhile p s of
        "" -> []
        s' -> w : wordsWhen p s''
            where (w, s'') = break p s'
