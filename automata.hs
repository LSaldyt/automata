-- Lucas Saldyt
-- Wolfram-style cellular automata rules are 8 bits

import Data.List (find)
import Data.Maybe



extract Nothing = 0
extract (Just (a, b)) = b

-- s = [0, 1]
-- s = [0..8]
s = [0, 1]

arity = length s

toArity 0 = [0]
toArity n = toArity ( n `quot` arity ) ++ [ n `rem` arity ]

initial = [1]
expand array = [0] ++ array ++ [0]

total_space = [[x, y, z] | x <- s, y <- s, z <- s]
-- test_arity = toArity 30

create_rule n = rule 
    where rule segment = extract $ find (\(a, b) -> a == segment) (zip space (toArity n))
          space = [[x, y, z] | x <- s, y <- s, z <- s]


placeholder_rule = create_rule 30

iterate_automata array = [placeholder_rule segment | 
                             (a, b, c) <- zip3 (init . init $ expanded) (init . tail $ expanded) (tail . tail $ expanded),
                             let segment = [a, b, c]] where expanded = expand . expand $ array

max_n = 20
loop_it state n = do
    let next_state = iterate_automata state
    print next_state
    if n < max_n
        then
            loop_it next_state (n + 1)
        else
            return ()

main = do
    print total_space
    print initial
    print (expand initial)
    loop_it (expand initial) 0
