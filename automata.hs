-- Lucas Saldyt
-- Wolfram-style cellular automata rules are 8 bits

import Data.List (find)
import Data.Maybe



extract Nothing = 0
extract (Just (a, b)) = b

space = [0, 1]
arity = length space

toArity 0 = [0]
toArity n = toArity ( n `quot` arity ) ++ [ n `rem` arity ]

initial = [1]

build_space s = [[x, y, z] | x <- s, y <- s, z <- s]
create_rule n = rule where rule segment = extract $ find (\(a, b) -> a == segment) (zip (build_space space) (toArity n))


expand array = [0] ++ array ++ [0]

iterate_automata rule array = [rule segment | 
                             (a, b, c) <- zip3 (init . init $ row) (init . tail $ row) (tail . tail $ row),
                             let segment = [a, b, c]] where row = expand . expand $ array

rule = create_rule 30
max_n = 60

loop_it state n = do
    let next_state = iterate_automata rule state
    print next_state
    if n < max_n
        then
            loop_it next_state (n + 1)
        else
            return ()

main = do
    print initial
    print (expand initial)
    loop_it (expand initial) 0
