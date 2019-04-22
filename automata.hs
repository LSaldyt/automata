-- Lucas Saldyt

import Data.List (find)
import Data.Maybe

extract Nothing = 0
extract (Just (a, b)) = b

space = [0..2]

basify 0 b = [0]
basify n b = [n `rem` b] ++ basify (n `quot` b) b

build_space s = [[x, y, z] | x <- s, y <- s, z <- s]
create_rule n = rule where rule seg = extract $ find (\(a, b) -> a == seg) (zip (build_space space) (basify n (length space)))

expand array = [0] ++ array ++ [0]
-- expand array = array

iterate_automata rule array = [rule seg | 
                             (a, b, c) <- zip3 (init . init $ row) (init . tail $ row) (tail . tail $ row),
                             let seg = [a, b, c]] where row = expand . expand $ array

-- rule = create_rule 30
-- base 3 rule 30 ignoring third base
--
-- Rule space size = base ** 
--
-- rule = create_rule 19794 -- Rule 30 for base 3
-- rule = create_rule 30123 -- A Sierpinski like pattern with three colors
-- rule = create_rule 123456 -- ?
-- rule = create_rule 987654 -- A wave pattern (base 8)
-- rule = create_rule 393939888 -- Wave pattern 2 (base 8)
-- rule = create_rule 929193872 -- Diamonds and stripes (base 8)
-- rule = create_rule 9223372036854775807
-- rule = create_rule 123456 (base 4)
-- rule = create_rule 30278 -- (base 4)
rule = create_rule 30123 -- A Sierpinski like pattern with three colors

initial = [1]

max_n = 60

loop_it state n = do
    let next_state = iterate_automata rule state
    print next_state
    if n < max_n 
        then loop_it next_state (n + 1)
        else return ()

main = do
    print initial
    print (expand initial)
    loop_it (expand initial) 0
