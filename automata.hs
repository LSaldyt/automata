-- Lucas Saldyt
-- Wolfram-style cellular automata rules are 8 bits

import Data.List (find)
import Data.Maybe

initial = [1]

expand array = [0] ++ array ++ [0]

toBinary 0 = [ 0 ]
toBinary n = toBinary ( n `quot` 2 ) ++ [ n `rem` 2 ]

extract Nothing = 0
extract (Just (a, b)) = b

create_rule n = rule 
    where rule segment = extract $ find (\(a, b) -> a == segment) (zip space (toBinary n))
          space = [[x, y, z] | x <- [0, 1], y <- [0, 1], z <- [0, 1]]

placeholder_rule = create_rule 30

iterate_automata array = [placeholder_rule segment | 
                             (a, b, c) <- zip3 (init . init $ expanded) (init . tail $ expanded) (tail . tail $ expanded),
                             let segment = [a, b, c]] where expanded = expand . expand $ array

main = print $ iterate_automata . iterate_automata . expand $ initial
