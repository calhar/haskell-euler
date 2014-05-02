import Data.Char
import Data.List
import Data.List.Utils

-- get names from file, split on "," and remove the "s surround each name
-- combine with line index and then multiply index by nameVal
--
-- nameVal is the sum of the integer representation of the uppercase char
-- minus 64 to map [A..Z] to [1..26] rather than [65..90]
main = do
        contents <- readFile "names.txt"
        print $ sum $ map (\(t, name) -> t * (nameVal name)) $
            zip [1..] $ sort $ map (filter (/= '\"')) $ split "," contents

nameVal name = sum $ map ((64 `subtract`) . ord . toUpper) name
